import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import simplifile

import lispy/builtins
import lispy/environment.{type Environment}
import lispy/error
import lispy/eval
import lispy/parser
import lispy/value.{type Value}

pub fn start() -> Nil {
  init_history()
  io.println("")
  io.println("╔════════════════════════════════════════════════╗")
  io.println("║                         Lispy REPL             ║")
  io.println("╚════════════════════════════════════════════════╝")
  io.println("")
  io.println("  Commands:")
  io.println("    :quit, :q       - Exit the REPL")
  io.println("    :help, :h       - Show this help")
  io.println("    :history        - Show command history")
  io.println("    :l <file>       - Load and evaluate a file")
  io.println("")
  io.println("  Navigation:")
  io.println("    ↑/↓ (if supported) - Browse history")
  io.println("    Ctrl+D          - Exit")
  io.println("")

  let env = builtins.create_global_env()
  repl_loop(env, 1)
}

@external(erlang, "lispy_ffi", "init_history")
fn init_history() -> Nil

@external(erlang, "lispy_ffi", "save_history")
fn save_history() -> Nil

@external(erlang, "lispy_ffi", "get_history")
fn get_history() -> List(String)

fn repl_loop(env: Environment(Value), line_num: Int) -> Nil {
  let prompt = "lispy[" <> int.to_string(line_num) <> "]❯ "
  io.print(prompt)

  let input = read_line_with_history()

  case input {
    "" -> repl_loop(env, line_num)
    _ -> {
      case input {
        ":quit" | ":q" | ":exit" -> {
          save_history()
          io.println("\nGoodbye!")
          Nil
        }
        ":help" | ":h" -> {
          print_help()
          repl_loop(env, line_num)
        }
        ":history" -> {
          print_history()
          repl_loop(env, line_num)
        }
        _ -> {
          case string.starts_with(input, ":l ") {
            True -> {
              let filepath = string.drop_start(input, 3) |> string.trim
              case load_file(filepath, env) {
                Ok(new_env) -> {
                  io.println("✓ Loaded: " <> filepath)
                  repl_loop(new_env, line_num + 1)
                }
                Error(err) -> {
                  io.println("✗ Error loading file: " <> err)
                  repl_loop(env, line_num)
                }
              }
            }
            False -> {
              case eval_string(input, env) {
                Ok(#(value, new_env)) -> {
                  io.println("=> " <> value.to_string(value))
                  repl_loop(new_env, line_num + 1)
                }
                Error(err) -> {
                  io.println("✗ Error: " <> error_to_string(err))
                  repl_loop(env, line_num)
                }
              }
            }
          }
        }
      }
    }
  }
}

fn print_help() -> Nil {
  io.println("")
  io.println("═══════════════════════════════════════════")
  io.println("  Lispy REPL Help")
  io.println("═══════════════════════════════════════════")
  io.println("")
  io.println("  Commands:")
  io.println("    :help, :h       Show this help")
  io.println("    :quit, :q       Exit the REPL")
  io.println("    :history        Show command history")
  io.println("    :l <file>       Load and evaluate a file")
  io.println("")
  io.println("  Navigation:")
  io.println("    ↑/↓            Browse history (in terminal)")
  io.println("    Ctrl+D         Exit")
  io.println("")
  io.println("═══════════════════════════════════════════")
  io.println("")
}

fn print_history() -> Nil {
  let history = get_history()
  io.println("")
  io.println("─────────────────────────────────────────────")
  io.println("  Command History")
  io.println("─────────────────────────────────────────────")

  case history {
    [] -> io.println("  No history entries yet")
    _ -> {
      let total = list.length(history)
      let to_show = case total > 20 {
        True -> list.drop(history, total - 20)
        False -> history
      }

      let start_num = case total > 20 {
        True -> total - 20 + 1
        False -> 1
      }

      print_history_entries(to_show, start_num)

      case total > 20 {
        True -> {
          io.println("")
          io.println(
            "  ℹ Showing last 20 of " <> int.to_string(total) <> " entries",
          )
        }
        False -> Nil
      }
    }
  }

  io.println("─────────────────────────────────────────────")
  io.println("")
}

fn print_history_entries(entries: List(String), start_num: Int) -> Nil {
  print_history_loop(entries, start_num)
}

fn print_history_loop(entries: List(String), num: Int) -> Nil {
  case entries {
    [] -> Nil
    [first, ..rest] -> {
      let _ = io.println("  " <> int.to_string(num) <> ": " <> first)
      print_history_loop(rest, num + 1)
    }
  }
}

fn read_line_with_history() -> String {
  case erlang_read_line_with_history("") {
    Ok(line) -> string.trim(line)
    Error(_) -> ":quit"
  }
}

@external(erlang, "lispy_ffi", "read_line_with_history")
fn erlang_read_line_with_history(prompt: String) -> Result(String, Nil)

fn eval_string(
  input: String,
  env: Environment(Value),
) -> Result(#(Value, Environment(Value)), error.EvaluationError) {
  case parser.parse(input) {
    Ok(Some(#(parsed, _))) -> eval.eval(env, parsed)
    Ok(None) -> Error(error.TypeError("Empty input"))
    Error(err) ->
      Error(error.TypeError("Parse error: " <> parser.error_to_string(err)))
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
) -> Result(Environment(Value), error.EvaluationError) {
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
    Error(err) ->
      Error(error.TypeError("Parse error: " <> parser.error_to_string(err)))
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
