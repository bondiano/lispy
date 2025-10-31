import gleam/int
import gleam/option
import gleam/string
import gleeunit
import gleeunit/should
import lispy/builtins
import lispy/error
import lispy/eval
import lispy/parser
import lispy/value

pub fn main() {
  gleeunit.main()
}

pub fn erlang_call_lists_reverse_test() {
  let env = builtins.create_global_env()

  let code = "(erlang-call @lists @reverse (list 1 2 3))"

  let assert Ok(result) = parse_and_eval(code, env)

  // Should return (3 2 1)
  let expected =
    value.from_list([value.Integer(3), value.Integer(2), value.Integer(1)])
  should.equal(result, expected)
}

pub fn erlang_call_erlang_length_test() {
  let env = builtins.create_global_env()

  let code = "(erlang-call @erlang @length (list 1 2 3 4 5))"

  let assert Ok(result) = parse_and_eval(code, env)

  should.equal(result, value.Integer(5))
}

pub fn erlang_call_lists_sum_test() {
  let env = builtins.create_global_env()

  let code = "(erlang-call @lists @sum (list 1 2 3 4))"

  let assert Ok(result) = parse_and_eval(code, env)

  should.equal(result, value.Integer(10))
}

pub fn erlang_call_with_atom_args_test() {
  let env = builtins.create_global_env()

  let code = "(erlang-call @erlang @atom_to_binary @test)"

  let assert Ok(result) = parse_and_eval(code, env)

  should.equal(result, value.String("test"))
}

pub fn erlang_call_erlang_is_atom_test() {
  let env = builtins.create_global_env()

  let code = "(erlang-call @erlang @is_atom @test)"

  let assert Ok(result) = parse_and_eval(code, env)

  should.equal(result, value.Boolean(True))
}

pub fn erlang_call_with_variable_test() {
  let env = builtins.create_global_env()

  let code =
    "(begin (def my-list (list 5 4 3 2 1)) (erlang-call @lists @reverse my-list))"

  let assert Ok(result) = parse_and_eval(code, env)

  let expected =
    value.from_list([
      value.Integer(1),
      value.Integer(2),
      value.Integer(3),
      value.Integer(4),
      value.Integer(5),
    ])
  should.equal(result, expected)
}

fn parse_and_eval(code: String, env) -> Result(value.Value, String) {
  case parser.parse(code) {
    Ok(option.Some(#(form, _))) -> {
      case eval.eval(env, form) {
        Ok(#(result, _)) -> Ok(result)
        Error(err) -> Error("Eval error: " <> error_to_string(err))
      }
    }
    Ok(option.None) -> Error("No form parsed")
    Error(err) -> Error("Parse error: " <> parser.error_to_string(err))
  }
}

pub fn erlang_call_badarg_error_test() {
  let env = builtins.create_global_env()

  let code = "(erlang-call @erlang @binary_to_atom \"test\" 99999)"

  let result = parse_and_eval(code, env)

  should.be_error(result)
  case result {
    Error(msg) -> should.be_true(string.contains(msg, "badarg"))
    _ -> should.fail()
  }
}

pub fn erlang_call_undef_error_test() {
  let env = builtins.create_global_env()

  let code = "(erlang-call @erlang @nonexistent_function 123)"

  let result = parse_and_eval(code, env)

  should.be_error(result)
  case result {
    Error(msg) -> should.be_true(string.contains(msg, "undef"))
    _ -> should.fail()
  }
}

pub fn erlang_call_error_includes_function_name_test() {
  let env = builtins.create_global_env()

  let code = "(erlang-call @erlang @binary_to_atom \"test\" 99999)"

  let result = parse_and_eval(code, env)

  should.be_error(result)
  case result {
    Error(msg) -> {
      should.be_true(string.contains(msg, "erlang:binary_to_atom"))
    }
    _ -> should.fail()
  }
}

fn error_to_string(err) -> String {
  case err {
    error.ZeroDivisionError -> "Division by zero"
    error.UndefinedVariableError(name) -> "Undefined variable: " <> name
    error.InvalidFormError(_) -> "Invalid form"
    error.ArityError(name, expected, got) ->
      "Arity error in "
      <> name
      <> ": expected "
      <> int.to_string(expected)
      <> " got "
      <> int.to_string(got)
    error.TypeError(msg) -> msg
  }
}
