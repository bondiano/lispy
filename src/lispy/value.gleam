import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/string

import lispy/environment.{type Environment}

pub type Value {
  Integer(Int)
  Double(Float)
  String(text: String)
  Symbol(name: String)
  Boolean(Bool)

  Lambda(
    arguments: List(Value),
    body: List(Value),
    environment: Environment(Value),
  )
  Macro(arguments: List(Value), body: List(Value))

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
      let arguments = arguments |> list.map(to_string) |> string.join(" ")
      let body = body |> list.map(to_string) |> string.join(" ")
      "(lambda (" <> arguments <> ") (" <> body <> "))"
    }

    Macro(arguments, body) -> {
      let arguments = arguments |> list.map(to_string) |> string.join(" ")
      let body = body |> list.map(to_string) |> string.join(" ")
      "(macro (" <> arguments <> ") (" <> body <> "))"
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
