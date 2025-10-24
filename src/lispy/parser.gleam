import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

import lispy/value.{type Value}

pub fn parse(input: String) -> Result(Option(#(Value, String)), String) {
  let trimmed = skip_whitespace_and_comments(input)

  case trimmed {
    "" -> Ok(None)
    _ -> {
      use #(value, rest) <- result.map(parse_value(trimmed))
      Some(#(value, rest))
    }
  }
}

fn parse_value(input: String) -> Result(#(Value, String), String) {
  let input = skip_whitespace_and_comments(input)

  case input {
    "" -> Error("Unexpected end of input")

    "(" <> rest -> parse_list(rest)

    "{" <> rest -> parse_dict(rest)

    "'" <> rest -> {
      use #(value, rest) <- result.try(parse_value(rest))
      Ok(#(list_to_cons([value.Symbol("quote"), value]), rest))
    }

    "\"" <> rest -> parse_string(rest, "")

    // Boolean, number, or symbol
    _ -> parse_atom(input)
  }
}

fn parse_list(input: String) -> Result(#(Value, String), String) {
  parse_list_items(input, [])
}

fn parse_dict(input: String) -> Result(#(Value, String), String) {
  parse_dict_items(input, [])
}

fn parse_dict_items(
  input: String,
  acc: List(#(Value, Value)),
) -> Result(#(Value, String), String) {
  let input = skip_whitespace_and_comments(input)

  case input {
    "" -> Error("Unclosed dict")

    "}" <> rest -> {
      Ok(#(value.Dict(dict.from_list(acc)), rest))
    }

    _ -> {
      use #(key, rest) <- result.try(parse_value(input))
      let rest = skip_whitespace_and_comments(rest)

      use #(val, rest) <- result.try(parse_value(rest))
      parse_dict_items(rest, [#(key, val), ..acc])
    }
  }
}

fn parse_list_items(
  input: String,
  acc: List(Value),
) -> Result(#(Value, String), String) {
  let input = skip_whitespace_and_comments(input)

  case input {
    "" -> Error("Unclosed list")

    ")" <> rest -> {
      Ok(#(list_to_cons(list.reverse(acc)), rest))
    }

    // Improper list (a . b)
    "." <> rest -> {
      let rest = skip_whitespace_and_comments(rest)
      use #(tail, rest) <- result.try(parse_value(rest))
      let rest = skip_whitespace_and_comments(rest)

      case rest {
        ")" <> rest -> {
          case list.reverse(acc) {
            [] -> Error("Invalid dotted list")
            [head, ..rest_items] -> {
              let proper_part = list_to_cons(list.reverse(rest_items))
              Ok(#(cons_with_tail(proper_part, head, tail), rest))
            }
          }
        }
        _ -> Error("Expected ) after dot")
      }
    }

    _ -> {
      use #(value, rest) <- result.try(parse_value(input))
      parse_list_items(rest, [value, ..acc])
    }
  }
}

fn parse_string(input: String, acc: String) -> Result(#(Value, String), String) {
  case input {
    "" -> Error("Unterminated string")

    "\"" <> rest -> Ok(#(value.String(acc), rest))

    "\\\\" <> rest -> parse_string(rest, acc <> "\\")
    "\\\"" <> rest -> parse_string(rest, acc <> "\"")
    "\\n" <> rest -> parse_string(rest, acc <> "\n")
    "\\t" <> rest -> parse_string(rest, acc <> "\t")
    "\\r" <> rest -> parse_string(rest, acc <> "\r")

    _ -> {
      case string.pop_grapheme(input) {
        Ok(#(char, rest)) -> parse_string(rest, acc <> char)
        Error(_) -> Error("Invalid string")
      }
    }
  }
}

fn parse_atom(input: String) -> Result(#(Value, String), String) {
  let atom_str = take_until_delimiter(input)
  let rest = string.drop_start(input, string.length(atom_str))

  case atom_str {
    "" -> Error("Empty atom")

    // Booleans
    "#t" | "true" -> Ok(#(value.Boolean(True), rest))
    "#f" | "false" -> Ok(#(value.Boolean(False), rest))

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
