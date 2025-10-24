import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleeunit/should

import lispy/builtins
import lispy/eval
import lispy/parser
import lispy/value

fn eval_string(input: String) -> Result(value.Value, eval.EvaluationError) {
  let assert Ok(Some(#(parsed, _))) = parser.parse(input)
  let env = builtins.create_global_env()
  case eval.eval(env, parsed) {
    Ok(#(val, _)) -> Ok(val)
    Error(e) -> Error(e)
  }
}

fn assert_eval(input: String, expected: value.Value) {
  case eval_string(input) {
    Ok(result) -> should.equal(result, expected)
    Error(e) -> {
      let error_msg = case e {
        eval.ZeroDivisionError -> "ZeroDivisionError"
        eval.UndefinedVariableError(name) -> "UndefinedVariableError: " <> name
        eval.InvalidFormError(_) -> "InvalidFormError"
        eval.ArityError(name, expected, got) ->
          "ArityError: "
          <> name
          <> " expected "
          <> int.to_string(expected)
          <> " got "
          <> int.to_string(got)
        eval.TypeError(msg) -> "TypeError: " <> msg
      }
      panic as error_msg
    }
  }
}

fn value_to_gleam_list(val: value.Value) -> List(value.Value) {
  case val {
    value.Nil -> []
    value.Cons(head, tail) -> [head, ..value_to_gleam_list(tail)]
    _ -> []
  }
}

pub fn eval_dict_get_test() {
  assert_eval("(dict-get {x 10 y 20} 'x)", value.Integer(10))
}

pub fn eval_dict_get_missing_test() {
  assert_eval("(dict-get {x 10} 'z)", value.Nil)
}

pub fn eval_dict_set_test() {
  let input = "(dict-set {x 10} 'y 20)"
  let result = eval_string(input)

  case result {
    Ok(value.Dict(d)) -> {
      let x_val = dict.get(d, value.Symbol("x"))
      let y_val = dict.get(d, value.Symbol("y"))

      should.equal(x_val, Ok(value.Integer(10)))
      should.equal(y_val, Ok(value.Integer(20)))
    }
    _ -> panic as "Expected Dict value"
  }
}

pub fn eval_dict_keys_test() {
  let input = "(dict-keys {x 10 y 20})"
  let result = eval_string(input)

  case result {
    Ok(list_val) -> {
      let keys = value_to_gleam_list(list_val)
      should.equal(list.length(keys), 2)
      should.be_true(list.contains(keys, value.Symbol("x")))
      should.be_true(list.contains(keys, value.Symbol("y")))
    }
    _ -> panic as "Expected list value"
  }
}

pub fn eval_dict_values_test() {
  let input = "(dict-values {x 10 y 20})"
  let result = eval_string(input)

  case result {
    Ok(list_val) -> {
      let values = value_to_gleam_list(list_val)
      should.equal(list.length(values), 2)
      should.be_true(list.contains(values, value.Integer(10)))
      should.be_true(list.contains(values, value.Integer(20)))
    }
    _ -> panic as "Expected list value"
  }
}

pub fn eval_dict_has_true_test() {
  assert_eval("(dict-has? {x 10} 'x)", value.Boolean(True))
}

pub fn eval_dict_has_false_test() {
  assert_eval("(dict-has? {x 10} 'y)", value.Boolean(False))
}
