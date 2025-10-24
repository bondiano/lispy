import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{Some}
import gleam/result
import lispy/environment.{type Environment}
import lispy/error.{type EvaluationError, ArityError, TypeError}
import lispy/value.{type Value}

pub fn expand(
  env: Environment(Value),
  expr: Value,
) -> Result(Value, EvaluationError) {
  case expr {
    value.Nil -> Ok(value.Nil)
    value.Cons(_, _) -> expand_cons(env, expr)
    _ -> Ok(expr)
  }
}

fn expand_cons(
  env: Environment(Value),
  expr: Value,
) -> Result(Value, EvaluationError) {
  case expr {
    value.Cons(value.Symbol(name), tail) -> {
      case environment.get(env, name) {
        Some(value.Macro(params, body)) -> {
          use expanded <- result.try(expand_macro(params, body, tail))
          expand(env, expanded)
        }
        _ -> {
          use expanded_head <- result.try(expand(env, value.Symbol(name)))
          use expanded_tail <- result.try(expand_list(env, tail))
          Ok(value.Cons(expanded_head, expanded_tail))
        }
      }
    }
    value.Cons(head, tail) -> {
      use expanded_head <- result.try(expand(env, head))
      use expanded_tail <- result.try(expand_list(env, tail))
      Ok(value.Cons(expanded_head, expanded_tail))
    }
    _ -> Ok(expr)
  }
}

fn expand_list(
  env: Environment(Value),
  list: Value,
) -> Result(Value, EvaluationError) {
  case list {
    value.Nil -> Ok(value.Nil)
    value.Cons(head, tail) -> {
      use expanded_head <- result.try(expand(env, head))
      use expanded_tail <- result.try(expand_list(env, tail))
      Ok(value.Cons(expanded_head, expanded_tail))
    }
    _ -> Ok(list)
  }
}

fn expand_macro(
  params: List(Value),
  body: List(Value),
  args: Value,
) -> Result(Value, EvaluationError) {
  use arg_values <- result.try(list_to_gleam_list(args))

  let param_count = list.length(params)
  let arg_count = list.length(arg_values)

  case param_count == arg_count {
    False -> Error(ArityError("macro", param_count, arg_count))
    True -> {
      use substitutions <- result.try(build_substitutions(params, arg_values))

      case body {
        [] -> Ok(value.Nil)
        [expr, ..] -> substitute(expr, substitutions)
      }
    }
  }
}

fn build_substitutions(
  params: List(Value),
  args: List(Value),
) -> Result(Dict(String, Value), EvaluationError) {
  use pairs <- result.try(
    list.try_map(list.zip(params, args), fn(pair) {
      case pair {
        #(value.Symbol(name), arg) -> Ok(#(name, arg))
        #(_, _) -> Error(TypeError("Macro parameters must be symbols"))
      }
    }),
  )

  Ok(dict.from_list(pairs))
}

fn substitute(
  expr: Value,
  substitutions: Dict(String, Value),
) -> Result(Value, EvaluationError) {
  case expr {
    value.Symbol(name) -> {
      case dict.get(substitutions, name) {
        Ok(replacement) -> Ok(replacement)
        Error(_) -> Ok(expr)
      }
    }

    value.Cons(head, tail) -> {
      use new_head <- result.try(substitute(head, substitutions))
      use new_tail <- result.try(substitute(tail, substitutions))
      Ok(value.Cons(new_head, new_tail))
    }

    _ -> Ok(expr)
  }
}

fn list_to_gleam_list(val: Value) -> Result(List(Value), EvaluationError) {
  case val {
    value.Nil -> Ok([])
    value.Cons(car, cdr) -> {
      use rest <- result.try(list_to_gleam_list(cdr))
      Ok([car, ..rest])
    }
    _ -> Error(TypeError("Expected list"))
  }
}
