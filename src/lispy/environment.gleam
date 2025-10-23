////// A module for managing environments that map variable names to expressions.

import gleam/dict
import gleam/option.{type Option, None}

/// Environments are used during evaluation to keep track of variable bindings
/// and their corresponding values.
pub type Environment(value) {

  Environment(
    bindings: dict.Dict(String, value),
    parent: Option(Environment(value)),
  )
}

pub fn new() -> Environment(value) {
  Environment(bindings: dict.new(), parent: None)
}

pub fn get(env: Environment(value), key: String) -> Result(value, String) {
  case dict.get(env.bindings, key) {
    Ok(value) -> Ok(value)
    Error(_) -> Error("Unknown variable: " <> key)
  }
}

pub fn set(
  env: Environment(value),
  key: String,
  value: value,
) -> Environment(value) {
  Environment(
    bindings: dict.insert(env.bindings, key, value),
    parent: env.parent,
  )
}
