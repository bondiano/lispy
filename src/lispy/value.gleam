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

  Cons(Value, Value)

  Nil
}

pub fn to_string(value: Value) -> String {
  case value {
    Integer(x) -> int.to_string(x)
    Double(x) -> float.to_string(x)
    String(text) -> "\"" <> text <> "\""
    Symbol(name) -> name
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

    Cons(car, cdr) -> "(" <> to_string(car) <> " " <> to_string(cdr) <> ")"

    Nil -> "nil"
  }
}
