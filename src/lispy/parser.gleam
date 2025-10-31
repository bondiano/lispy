import gleam/dict
import gleam/erlang/atom
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

import lispy/value.{type Value}

pub type ParseError {
  UnexpectedEndOfInput(context: String)
  UnclosedDelimiter(delimiter: String, position: Int)
  UnterminatedString(position: Int)
  InvalidEscape(sequence: String, position: Int)
  EmptyAtom(position: Int)
  ExpectedClosingParen(after: String, position: Int)
  InvalidCharacter(char: String, position: Int)
  UnexpectedToken(expected: String, got: String, position: Int)
}

pub fn error_to_string(error: ParseError) -> String {
  case error {
    UnexpectedEndOfInput(context) ->
      "Unexpected end of input while parsing " <> context

    UnclosedDelimiter(delimiter, pos) ->
      "Unclosed '"
      <> delimiter
      <> "' starting at position "
      <> int.to_string(pos)

    UnterminatedString(pos) ->
      "Unterminated string starting at position " <> int.to_string(pos)

    InvalidEscape(seq, pos) ->
      "Invalid escape sequence '\\"
      <> seq
      <> "' at position "
      <> int.to_string(pos)

    EmptyAtom(pos) -> "Empty atom at position " <> int.to_string(pos)

    ExpectedClosingParen(after, pos) ->
      "Expected ')' after " <> after <> " at position " <> int.to_string(pos)

    InvalidCharacter(char, pos) ->
      "Invalid character '" <> char <> "' at position " <> int.to_string(pos)

    UnexpectedToken(expected, got, pos) ->
      "Expected "
      <> expected
      <> " but got '"
      <> got
      <> "' at position "
      <> int.to_string(pos)
  }
}

pub fn parse(input: String) -> Result(Option(#(Value, String)), ParseError) {
  let trimmed = skip_whitespace_and_comments(input)

  case trimmed {
    "" -> Ok(None)
    _ -> {
      use #(value, rest) <- result.map(parse_value(input, trimmed))
      Some(#(value, rest))
    }
  }
}

fn parse_value(
  original: String,
  input: String,
) -> Result(#(Value, String), ParseError) {
  let input = skip_whitespace_and_comments(input)

  case input {
    "" -> Error(UnexpectedEndOfInput("value"))

    "(" <> rest -> parse_list(original, rest)

    "{" <> rest -> parse_dict(original, rest)

    "'" <> rest -> {
      use #(value, rest) <- result.try(parse_value(original, rest))
      Ok(#(list_to_cons([value.Symbol("quote"), value]), rest))
    }

    "\"" <> rest -> {
      let pos = calculate_position(original, input)
      parse_string(rest, "", pos)
    }

    _ -> parse_atom(original, input)
  }
}

fn parse_list(
  original: String,
  input: String,
) -> Result(#(Value, String), ParseError) {
  let pos = calculate_position(original, input)
  parse_list_items(original, input, [], pos)
}

fn parse_dict(
  original: String,
  input: String,
) -> Result(#(Value, String), ParseError) {
  let pos = calculate_position(original, input)
  parse_dict_items(original, input, [], pos)
}

fn parse_dict_items(
  original: String,
  input: String,
  acc: List(#(Value, Value)),
  start_pos: Int,
) -> Result(#(Value, String), ParseError) {
  let input = skip_whitespace_and_comments(input)

  case input {
    "" -> Error(UnclosedDelimiter("{", start_pos))

    "}" <> rest -> {
      Ok(#(value.Dict(dict.from_list(acc)), rest))
    }

    _ -> {
      use #(key, rest) <- result.try(parse_value(original, input))
      let rest = skip_whitespace_and_comments(rest)

      use #(val, rest) <- result.try(parse_value(original, rest))
      parse_dict_items(original, rest, [#(key, val), ..acc], start_pos)
    }
  }
}

fn parse_list_items(
  original: String,
  input: String,
  acc: List(Value),
  start_pos: Int,
) -> Result(#(Value, String), ParseError) {
  let input = skip_whitespace_and_comments(input)

  case input {
    "" -> Error(UnclosedDelimiter("(", start_pos))

    ")" <> rest -> {
      Ok(#(list_to_cons(list.reverse(acc)), rest))
    }

    // Improper list (a . b) or variadic params (. args) or (x . args)
    "." <> rest -> {
      let rest = skip_whitespace_and_comments(rest)
      use #(tail, rest) <- result.try(parse_value(original, rest))
      let rest = skip_whitespace_and_comments(rest)

      case rest {
        ")" <> rest -> {
          case list.reverse(acc) {
            [] ->
              Ok(#(
                value.Cons(value.Symbol("."), value.Cons(tail, value.Nil)),
                rest,
              ))
            [head, ..rest_items] -> {
              let proper_part = list_to_cons(list.reverse(rest_items))
              Ok(#(cons_with_tail(proper_part, head, tail), rest))
            }
          }
        }
        _ -> {
          let pos = calculate_position(original, rest)
          Error(ExpectedClosingParen("dot notation", pos))
        }
      }
    }

    _ -> {
      use #(value, rest) <- result.try(parse_value(original, input))
      parse_list_items(original, rest, [value, ..acc], start_pos)
    }
  }
}

fn parse_string(
  input: String,
  acc: String,
  start_pos: Int,
) -> Result(#(Value, String), ParseError) {
  case input {
    "" -> Error(UnterminatedString(start_pos))

    "\"" <> rest -> Ok(#(value.String(acc), rest))

    "\\\\" <> rest -> parse_string(rest, acc <> "\\", start_pos)
    "\\\"" <> rest -> parse_string(rest, acc <> "\"", start_pos)
    "\\n" <> rest -> parse_string(rest, acc <> "\n", start_pos)
    "\\t" <> rest -> parse_string(rest, acc <> "\t", start_pos)
    "\\r" <> rest -> parse_string(rest, acc <> "\r", start_pos)

    _ -> {
      case string.pop_grapheme(input) {
        Ok(#(char, rest)) -> parse_string(rest, acc <> char, start_pos)
        Error(_) -> Error(UnterminatedString(start_pos))
      }
    }
  }
}

fn parse_atom(
  original: String,
  input: String,
) -> Result(#(Value, String), ParseError) {
  let atom_str = take_until_delimiter(input)
  let rest = string.drop_start(input, string.length(atom_str))

  case atom_str {
    "" -> {
      let pos = calculate_position(original, input)
      Error(EmptyAtom(pos))
    }

    // Booleans
    "#t" | "true" -> Ok(#(value.Boolean(True), rest))
    "#f" | "false" -> Ok(#(value.Boolean(False), rest))

    "@" <> atom_name -> Ok(#(atom.create(atom_name) |> value.Atom, rest))

    // Nil
    "nil" -> Ok(#(value.Nil, rest))

    // Try parse as number
    _ -> {
      case int.parse(atom_str) {
        Ok(n) -> Ok(#(value.Integer(n), rest))

        Error(_) -> {
          case float.parse(atom_str) {
            Ok(f) -> Ok(#(value.Double(f), rest))

            // Otherwise it's a symbol
            Error(_) -> Ok(#(value.Symbol(atom_str), rest))
          }
        }
      }
    }
  }
}

// --------------
// Helpers
// --------------

fn calculate_position(original: String, remaining: String) -> Int {
  string.length(original) - string.length(remaining)
}

fn skip_whitespace_and_comments(input: String) -> String {
  let trimmed = string.trim_start(input)

  case trimmed {
    ";" <> _ -> {
      case string.split_once(trimmed, "\n") {
        Ok(#(_, rest)) -> skip_whitespace_and_comments(rest)
        Error(_) -> ""
      }
    }
    _ -> trimmed
  }
}

fn take_until_delimiter(input: String) -> String {
  do_take_until_delimiter(input, "")
}

fn do_take_until_delimiter(input: String, acc: String) -> String {
  case string.pop_grapheme(input) {
    Error(_) -> acc
    Ok(#(char, rest)) -> {
      case is_delimiter(char) {
        True -> acc
        False -> do_take_until_delimiter(rest, acc <> char)
      }
    }
  }
}

fn is_delimiter(char: String) -> Bool {
  char == "("
  || char == ")"
  || char == "{"
  || char == "}"
  || char == " "
  || char == "\n"
  || char == "\t"
  || char == "\r"
  || char == "\""
  || char == ";"
  || char == "'"
  || char == "`"
  || char == ","
}

fn list_to_cons(items: List(Value)) -> Value {
  case items {
    [] -> value.Nil
    [head, ..rest] -> value.Cons(head, list_to_cons(rest))
  }
}

fn cons_with_tail(head: Value, item: Value, tail: Value) -> Value {
  case head {
    value.Nil -> value.Cons(item, tail)
    value.Cons(car, cdr) -> value.Cons(car, cons_with_tail(cdr, item, tail))
    _ -> value.Cons(item, tail)
  }
}
