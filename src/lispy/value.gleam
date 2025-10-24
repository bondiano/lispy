import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/string

import lispy/environment.{type Environment}

pub type Params {
  Fixed(List(Value))
  Variadic(variadic_name: String)
  FixedAndVariadic(fixed: List(Value), variadic_name: String)
}

pub type Value {
  Integer(Int)
  Double(Float)
  String(text: String)
  Symbol(name: String)
  Boolean(Bool)

  Lambda(arguments: Params, body: List(Value), environment: Environment(Value))
  Macro(arguments: Params, body: List(Value))

  Builtin(name: String)

  Dict(dict.Dict(Value, Value))

  Cons(Value, Value)

  Nil
}

pub fn show(value: Value) -> String {
  case value {
    String(text) -> text
    _ -> to_string(value)
  }
}

pub fn to_string(value: Value) -> String {
  case value {
    Integer(x) -> int.to_string(x)
    Double(x) -> float.to_string(x)
    String(text) -> "\"" <> text <> "\""
    Symbol(name) -> name
    Builtin(name) -> name
    Boolean(b) ->
      case b {
        True -> "true"
        False -> "false"
      }

    Lambda(arguments, body, _environment) -> {
      let arguments_str = params_to_string(arguments)
      let body = body |> list.map(to_string) |> string.join(" ")
      "(lambda " <> arguments_str <> " (" <> body <> "))"
    }

    Macro(arguments, body) -> {
      let arguments_str = params_to_string(arguments)
      let body = body |> list.map(to_string) |> string.join(" ")
      "(macro " <> arguments_str <> " (" <> body <> "))"
    }

    Cons(car, cdr) -> {
      "(" <> cons_to_string(car, cdr) <> ")"
    }

    Dict(d) ->
      "{"
      <> dict.to_list(d)
      |> list.map(fn(entry) {
        let #(key, value) = entry
        to_string(key) <> " " <> to_string(value)
      })
      |> string.join(" ")
      <> "}"

    Nil -> "nil"
  }
}

fn cons_to_string(car: Value, cdr: Value) -> String {
  case cdr {
    Nil -> to_string(car)
    Cons(next_car, next_cdr) ->
      to_string(car) <> " " <> cons_to_string(next_car, next_cdr)
    _ -> to_string(car) <> " . " <> to_string(cdr)
  }
}

fn params_to_string(params: Params) -> String {
  case params {
    Fixed(args) -> {
      let args_str = args |> list.map(to_string) |> string.join(" ")
      "(" <> args_str <> ")"
    }
    Variadic(name) -> "(. " <> name <> ")"
    FixedAndVariadic(fixed, variadic) -> {
      let fixed_str = fixed |> list.map(to_string) |> string.join(" ")
      "(" <> fixed_str <> " . " <> variadic <> ")"
    }
  }
}

pub fn equal(a: Value, b: Value) -> Bool {
  case a, b {
    Integer(x), Integer(y) -> x == y
    Double(x), Double(y) -> x == y
    String(x), String(y) -> x == y
    Symbol(x), Symbol(y) -> x == y
    Boolean(x), Boolean(y) -> x == y
    Builtin(x), Builtin(y) -> x == y
    Nil, Nil -> True
    Cons(car_a, cdr_a), Cons(car_b, cdr_b) ->
      equal(car_a, car_b) && equal(cdr_a, cdr_b)
    _, _ -> False
  }
}

pub fn count_list(lst: Value) -> Int {
  case lst {
    Nil -> 0
    Cons(_, rest) -> 1 + count_list(rest)
    _ -> 0
  }
}

pub fn from_list(lst: List(Value)) -> Value {
  case lst {
    [] -> Nil
    [head, ..rest] -> Cons(head, from_list(rest))
  }
}

pub fn parse_params(params: Value) -> Result(Params, String) {
  case params {
    Cons(Symbol("."), Cons(Symbol(name), Nil)) -> Ok(Variadic(name))

    _ -> parse_params_helper(params, [])
  }
}

fn parse_params_helper(
  params: Value,
  acc: List(Value),
) -> Result(Params, String) {
  case params {
    Nil -> Ok(Fixed(list.reverse(acc)))

    Cons(Symbol("."), Cons(Symbol(name), Nil)) ->
      Ok(FixedAndVariadic(list.reverse(acc), name))

    Cons(Symbol(_) as param, rest) -> {
      case rest {
        // Improper list
        Symbol(name) -> Ok(FixedAndVariadic(list.reverse([param, ..acc]), name))
        _ -> parse_params_helper(rest, [param, ..acc])
      }
    }

    _ -> Error("Invalid parameter list")
  }
}
