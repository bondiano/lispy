import argv
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/string
import simplifile

import lispy/builtins
import lispy/environment
import lispy/error
import lispy/eval
import lispy/parser
import lispy/repl
import lispy/value

pub fn main() -> Nil {
  case argv.load().arguments {
    [] | ["help"] | ["--help"] | ["-h"] -> print_help()
    ["repl"] -> repl.start()
    ["run", filepath] -> run_file(filepath)
    ["run", "--eval", code] -> run_eval(code)
    ["run", "-e", code] -> run_eval(code)
    _ -> {
      io.println("Unknown command. Use 'lispy help' for usage information.")
    }
  }
}

fn print_help() -> Nil {
  io.println("Lispy - A Lisp interpreter in Gleam")
  io.println("")
  io.println("Usage:")
  io.println("  lispy repl              Start the REPL")
  io.println("  lispy run <file>        Run a Lisp file")
  io.println("  lispy run --eval <code> Evaluate a Lisp expression")
  io.println(
    "  lispy run -e <code>     Evaluate a Lisp expression (short form)",
  )
  io.println("  lispy help              Show this help message")
  io.println("")
  io.println("Examples:")
  io.println("  lispy repl")
  io.println("  lispy run examples/hello.lisp")
  io.println("  lispy run --eval \"(+ 2 2)\"")
}

fn run_file(filepath: String) -> Nil {
  case simplifile.read(filepath) {
    Ok(content) -> {
      let env = builtins.create_global_env()
      case eval_all(content, env) {
        Ok(_) -> Nil
        Error(err) -> {
          io.println("Error: " <> err)
        }
      }
    }
    Error(_) -> {
      io.println("Error: Could not read file: " <> filepath)
    }
  }
}

fn run_eval(code: String) -> Nil {
  let env = builtins.create_global_env()
  case parser.parse(code) {
    Ok(Some(#(parsed, _))) -> {
      case eval.eval(env, parsed) {
        Ok(#(value, _)) -> {
          io.println(value.to_string(value))
        }
        Error(err) -> {
          io.println("Error: " <> error_to_string(err))
        }
      }
    }
    Ok(None) -> {
      io.println("Error: Empty input")
    }
    Error(err) -> {
      io.println("Parse error: " <> err)
    }
  }
}

fn eval_all(
  input: String,
  env: environment.Environment(value.Value),
) -> Result(environment.Environment(value.Value), String) {
  case parser.parse(input) {
    Ok(Some(#(parsed, rest))) -> {
      case eval.eval(env, parsed) {
        Ok(#(_, new_env)) -> {
          case string.trim(rest) {
            "" -> Ok(new_env)
            remaining -> eval_all(remaining, new_env)
          }
        }
        Error(err) -> Error(error_to_string(err))
      }
    }
    Ok(None) -> Ok(env)
    Error(err) -> Error("Parse error: " <> err)
  }
}

fn error_to_string(err: error.EvaluationError) -> String {
  case err {
    error.ZeroDivisionError -> "Division by zero"
    error.UndefinedVariableError(name) -> "Undefined variable: " <> name
    error.InvalidFormError(form) -> "Invalid form: " <> value.to_string(form)
    error.ArityError(name, expected, got) ->
      "Arity error in "
      <> name
      <> ": expected "
      <> int.to_string(expected)
      <> " arguments, got "
      <> int.to_string(got)
    error.TypeError(msg) -> "Type error: " <> msg
  }
}
