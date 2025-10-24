import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/string
import simplifile

import lispy/builtins
import lispy/environment.{type Environment}
import lispy/eval
import lispy/parser
import lispy/value.{type Value}

pub fn start() -> Nil {
  io.println("Welcome to Lispy - A Lisp interpreter in Gleam!")
  io.println("Type expressions to evaluate, or :quit to exit.")
  io.println("Use :l <filepath> to load a file.")
  io.println("")

  let env = builtins.create_global_env()
  repl_loop(env)
}

fn repl_loop(env: Environment(Value)) -> Nil {
  io.print("> ")

  let input = read_line()

  case input {
    "" -> repl_loop(env)
    _ -> {
      case input {
        ":quit" | ":q" | ":exit" -> {
          io.println("Goodbye!")
          Nil
        }
        _ -> {
          case string.starts_with(input, ":l ") {
            True -> {
              let filepath = string.drop_start(input, 3) |> string.trim
              case load_file(filepath, env) {
                Ok(new_env) -> {
                  io.println("Loaded: " <> filepath)
                  repl_loop(new_env)
                }
                Error(err) -> {
                  io.println("Error loading file: " <> err)
                  repl_loop(env)
                }
              }
            }
            False -> {
              case eval_string(input, env) {
                Ok(#(value, new_env)) -> {
                  io.println(value.to_string(value))
                  repl_loop(new_env)
                }
                Error(err) -> {
                  io.println("Error: " <> error_to_string(err))
                  repl_loop(env)
                }
              }
            }
          }
        }
      }
    }
  }
}

fn read_line() -> String {
  case erlang_read_line("") {
    Ok(line) -> string.trim(line)
    Error(_) -> ":quit"
  }
}

@external(erlang, "lispy_ffi", "read_line")
fn erlang_read_line(prompt: String) -> Result(String, Nil)

fn eval_string(
  input: String,
  env: Environment(Value),
) -> Result(#(Value, Environment(Value)), eval.EvaluationError) {
  case parser.parse(input) {
    Ok(Some(#(parsed, _))) -> eval.eval(env, parsed)
    Ok(None) -> Error(eval.TypeError("Empty input"))
    Error(err) -> Error(eval.TypeError("Parse error: " <> err))
  }
}

pub fn load_file(
  filepath: String,
  env: Environment(Value),
) -> Result(Environment(Value), String) {
  case simplifile.read(filepath) {
    Ok(content) -> {
      case eval_all(content, env) {
        Ok(final_env) -> Ok(final_env)
        Error(err) -> Error(error_to_string(err))
      }
    }
    Error(_) -> Error("Could not read file: " <> filepath)
  }
}

fn eval_all(
  input: String,
  env: Environment(Value),
) -> Result(Environment(Value), eval.EvaluationError) {
  case parser.parse(input) {
    Ok(Some(#(parsed, rest))) -> {
      case eval.eval(env, parsed) {
        Ok(#(_, new_env)) -> {
          case string.trim(rest) {
            "" -> Ok(new_env)
            remaining -> eval_all(remaining, new_env)
          }
        }
        Error(err) -> Error(err)
      }
    }
    Ok(None) -> Ok(env)
    Error(err) -> Error(eval.TypeError("Parse error: " <> err))
  }
}

fn error_to_string(err: eval.EvaluationError) -> String {
  case err {
    eval.ZeroDivisionError -> "Division by zero"
    eval.UndefinedVariableError(name) -> "Undefined variable: " <> name
    eval.InvalidFormError(form) -> "Invalid form: " <> value.to_string(form)
    eval.ArityError(name, expected, got) ->
      "Arity error in "
      <> name
      <> ": expected "
      <> int.to_string(expected)
      <> " arguments, got "
      <> int.to_string(got)
    eval.TypeError(msg) -> "Type error: " <> msg
  }
}
