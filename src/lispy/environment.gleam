////// A module for managing environments that map variable names to expressions.

import gleam/dict
import gleam/option.{type Option, None, Some}

/// Environments are used during evaluation to keep track of variable bindings
/// and their corresponding values.
pub opaque type Environment(value) {

  Environment(
    bindings: dict.Dict(String, value),
    parent: Option(Environment(value)),
  )
}

pub fn new() -> Environment(value) {
  Environment(bindings: dict.new(), parent: None)
}

pub fn extend(env: Environment(value)) -> Environment(value) {
  Environment(bindings: dict.new(), parent: Some(env))
}

/// Merge two environments - `child`'s bindings override `parent`'s
pub fn merge(
  child: Environment(value),
  parent: Environment(value),
) -> Environment(value) {
  Environment(bindings: child.bindings, parent: Some(parent))
}

/// Get the value of a variable in the environment.
/// Checks the current environment, then the parent environment if not found.
pub fn get(env: Environment(value), key: String) -> Option(value) {
  case dict.get(env.bindings, key) {
    Ok(value) -> Some(value)
    Error(_) -> {
      case env.parent {
        Some(parent) -> get(parent, key)
        None -> None
      }
    }
  }
}

pub fn define(
  env: Environment(value),
  key: String,
  value: value,
) -> Environment(value) {
  Environment(
    bindings: dict.insert(env.bindings, key, value),
    parent: env.parent,
  )
}

pub fn set(
  env: Environment(value),
  key: String,
  value: value,
) -> Result(Environment(value), String) {
  case dict.get(env.bindings, key) {
    Ok(_) -> Ok(define(env, key, value))
    Error(_) -> {
      case env.parent {
        Some(parent) -> set(parent, key, value)
        None -> Error("Cannot set undefined variable: " <> key)
      }
    }
  }
}
