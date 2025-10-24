import gleam/dict
import gleam/int
import gleam/option.{Some}
import gleeunit/should

import lispy/builtins
import lispy/environment
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

// Self-evaluating forms

pub fn eval_integer_test() {
  assert_eval("42", value.Integer(42))
}

pub fn eval_double_test() {
  assert_eval("3.14", value.Double(3.14))
}

pub fn eval_string_test() {
  assert_eval("\"hello\"", value.String("hello"))
}

pub fn eval_boolean_true_test() {
  assert_eval("#t", value.Boolean(True))
}

pub fn eval_boolean_false_test() {
  assert_eval("#f", value.Boolean(False))
}

pub fn eval_nil_test() {
  assert_eval("nil", value.Nil)
}

// Quote

pub fn eval_quote_symbol_test() {
  assert_eval("'x", value.Symbol("x"))
}

pub fn eval_quote_list_test() {
  assert_eval(
    "'(1 2 3)",
    value.Cons(
      value.Integer(1),
      value.Cons(value.Integer(2), value.Cons(value.Integer(3), value.Nil)),
    ),
  )
}

pub fn eval_quote_nested_test() {
  assert_eval(
    "'(a (b c))",
    value.Cons(
      value.Symbol("a"),
      value.Cons(
        value.Cons(value.Symbol("b"), value.Cons(value.Symbol("c"), value.Nil)),
        value.Nil,
      ),
    ),
  )
}

// Arithmetic

pub fn eval_add_no_args_test() {
  assert_eval("(+)", value.Integer(0))
}

pub fn eval_add_one_arg_test() {
  assert_eval("(+ 5)", value.Integer(5))
}

pub fn eval_add_two_args_test() {
  assert_eval("(+ 2 3)", value.Integer(5))
}

pub fn eval_add_multiple_args_test() {
  assert_eval("(+ 1 2 3 4 5)", value.Integer(15))
}

pub fn eval_add_mixed_types_test() {
  assert_eval("(+ 1 2.5 3)", value.Double(6.5))
}

pub fn eval_subtract_one_arg_test() {
  assert_eval("(- 5)", value.Integer(-5))
}

pub fn eval_subtract_two_args_test() {
  assert_eval("(- 10 3)", value.Integer(7))
}

pub fn eval_subtract_multiple_args_test() {
  assert_eval("(- 20 5 3 2)", value.Integer(10))
}

pub fn eval_multiply_no_args_test() {
  assert_eval("(*)", value.Integer(1))
}

pub fn eval_multiply_args_test() {
  assert_eval("(* 2 3 4)", value.Integer(24))
}

pub fn eval_divide_test() {
  assert_eval("(/ 10 2)", value.Double(5.0))
}

pub fn eval_divide_float_result_test() {
  assert_eval("(/ 10 4)", value.Double(2.5))
}

pub fn eval_divide_by_zero_test() {
  case eval_string("(/ 10 0)") {
    Error(eval.ZeroDivisionError) -> True
    _ -> False
  }
  |> should.be_true()
}

pub fn eval_mod_test() {
  assert_eval("(mod 10 3)", value.Integer(1))
}

pub fn eval_mod_zero_test() {
  case eval_string("(mod 10 0)") {
    Error(eval.ZeroDivisionError) -> True
    _ -> False
  }
  |> should.be_true()
}

// Comparison

pub fn eval_equal_integers_test() {
  assert_eval("(= 5 5)", value.Boolean(True))
}

pub fn eval_not_equal_integers_test() {
  assert_eval("(= 5 6)", value.Boolean(False))
}

pub fn eval_equal_strings_test() {
  assert_eval("(= \"hello\" \"hello\")", value.Boolean(True))
}

pub fn eval_greater_than_test() {
  assert_eval("(> 5 3)", value.Boolean(True))
  assert_eval("(> 3 5)", value.Boolean(False))
}

pub fn eval_less_than_test() {
  assert_eval("(< 3 5)", value.Boolean(True))
  assert_eval("(< 5 3)", value.Boolean(False))
}

// String operations

pub fn eval_concat_empty_test() {
  assert_eval("(++)", value.String(""))
}

pub fn eval_concat_strings_test() {
  assert_eval("(++ \"hello\" \" \" \"world\")", value.String("hello world"))
}

// List operations

pub fn eval_cons_test() {
  assert_eval("(cons 1 nil)", value.Cons(value.Integer(1), value.Nil))
}

pub fn eval_cons_to_list_test() {
  assert_eval(
    "(cons 1 (cons 2 nil))",
    value.Cons(value.Integer(1), value.Cons(value.Integer(2), value.Nil)),
  )
}

pub fn eval_car_test() {
  assert_eval("(car '(1 2 3))", value.Integer(1))
}

pub fn eval_cdr_test() {
  assert_eval(
    "(cdr '(1 2 3))",
    value.Cons(value.Integer(2), value.Cons(value.Integer(3), value.Nil)),
  )
}

pub fn eval_list_test() {
  assert_eval(
    "(list 1 2 3)",
    value.Cons(
      value.Integer(1),
      value.Cons(value.Integer(2), value.Cons(value.Integer(3), value.Nil)),
    ),
  )
}

pub fn eval_list_empty_test() {
  assert_eval("(list)", value.Nil)
}

// If

pub fn eval_if_true_test() {
  assert_eval("(if #t 1 2)", value.Integer(1))
}

pub fn eval_if_false_test() {
  assert_eval("(if #f 1 2)", value.Integer(2))
}

pub fn eval_if_nil_is_false_test() {
  assert_eval("(if nil 1 2)", value.Integer(2))
}

pub fn eval_if_non_false_is_true_test() {
  assert_eval("(if 0 1 2)", value.Integer(1))
  assert_eval("(if \"\" 1 2)", value.Integer(1))
}

pub fn eval_if_without_alternate_test() {
  assert_eval("(if #t 1)", value.Integer(1))
  assert_eval("(if #f 1)", value.Nil)
}

pub fn eval_if_evaluates_condition_test() {
  assert_eval("(if (< 2 3) 1 2)", value.Integer(1))
}

// Lambda

pub fn eval_lambda_identity_test() {
  assert_eval("((lambda (x) x) 42)", value.Integer(42))
}

pub fn eval_lambda_add_test() {
  assert_eval("((lambda (x y) (+ x y)) 2 3)", value.Integer(5))
}

pub fn eval_lambda_closure_test() {
  let input =
    "
    ((lambda (x)
       (lambda (y) (+ x y)))
     10)
  "

  case eval_string(input) {
    Ok(value.Lambda(_, _, _)) -> True
    _ -> False
  }
  |> should.be_true()
}

pub fn eval_lambda_nested_application_test() {
  assert_eval("(((lambda (x) (lambda (y) (+ x y))) 10) 5)", value.Integer(15))
}

// Define

pub fn eval_define_variable_test() {
  let env = builtins.create_global_env()
  let assert Ok(Some(#(parsed, _))) = parser.parse("(define x 42)")
  let assert Ok(#(_, env1)) = eval.eval(env, parsed)

  case environment.get(env1, "x") {
    Some(value.Integer(42)) -> True
    _ -> False
  }
  |> should.be_true()
}

pub fn eval_define_function_test() {
  let env = builtins.create_global_env()
  let assert Ok(Some(#(define_form, _))) =
    parser.parse("(define add (lambda (x y) (+ x y)))")
  let assert Ok(#(_, env1)) = eval.eval(env, define_form)

  case environment.get(env1, "add") {
    Some(value.Lambda(_, _, _)) -> True
    _ -> False
  }
  |> should.be_true()
}

pub fn eval_use_defined_variable_test() {
  let env = builtins.create_global_env()
  let assert Ok(Some(#(define_form, _))) = parser.parse("(define x 10)")
  let assert Ok(#(_, env1)) = eval.eval(env, define_form)

  let assert Ok(Some(#(use_form, _))) = parser.parse("(+ x 5)")
  let assert Ok(#(result, _)) = eval.eval(env1, use_form)

  should.equal(result, value.Integer(15))
}

// Set!

pub fn eval_set_existing_variable_test() {
  let env = builtins.create_global_env()
  let assert Ok(Some(#(define_form, _))) = parser.parse("(define x 10)")
  let assert Ok(#(_, env1)) = eval.eval(env, define_form)

  let assert Ok(Some(#(set_form, _))) = parser.parse("(set! x 20)")
  let assert Ok(#(_, env2)) = eval.eval(env1, set_form)

  case environment.get(env2, "x") {
    Some(value.Integer(20)) -> True
    _ -> False
  }
  |> should.be_true()
}

pub fn eval_set_undefined_variable_error_test() {
  case eval_string("(set! x 10)") {
    Error(eval.UndefinedVariableError(_)) -> True
    _ -> False
  }
  |> should.be_true()
}

// Begin

pub fn eval_begin_empty_test() {
  assert_eval("(begin)", value.Nil)
}

pub fn eval_begin_single_test() {
  assert_eval("(begin 42)", value.Integer(42))
}

pub fn eval_begin_multiple_test() {
  assert_eval("(begin 1 2 3)", value.Integer(3))
}

pub fn eval_begin_with_side_effects_test() {
  let env = builtins.create_global_env()
  let input =
    "
    (begin
      (define x 10)
      (set! x 20)
      x)
  "
  let assert Ok(Some(#(parsed, _))) = parser.parse(input)
  let assert Ok(#(result, _)) = eval.eval(env, parsed)

  should.equal(result, value.Integer(20))
}

// Symbol

pub fn eval_symbol_from_string_test() {
  assert_eval("(symbol \"foo\")", value.Symbol("foo"))
}

// Complex expressions

pub fn eval_nested_arithmetic_test() {
  assert_eval("(+ (* 2 3) (- 10 5))", value.Integer(11))
}

pub fn eval_factorial_recursive_test() {
  let env = builtins.create_global_env()
  let fact_def =
    "
    (define fact
      (lambda (n)
        (if (< n 2)
            1
            (* n (fact (- n 1))))))
  "
  let assert Ok(Some(#(def_parsed, _))) = parser.parse(fact_def)
  let assert Ok(#(_, env1)) = eval.eval(env, def_parsed)

  let assert Ok(Some(#(call_parsed, _))) = parser.parse("(fact 5)")
  let assert Ok(#(result, _)) = eval.eval(env1, call_parsed)

  should.equal(result, value.Integer(120))
}

pub fn eval_higher_order_function_test() {
  let env = builtins.create_global_env()
  let input =
    "
    (begin
      (define apply-twice
        (lambda (f x)
          (f (f x))))
      (define add1
        (lambda (x)
          (+ x 1)))
      (apply-twice add1 5))
  "
  let assert Ok(Some(#(parsed, _))) = parser.parse(input)
  let assert Ok(#(result, _)) = eval.eval(env, parsed)

  should.equal(result, value.Integer(7))
}

pub fn eval_map_implementation_test() {
  let env = builtins.create_global_env()
  let input =
    "
    (begin
      (define map
        (lambda (f lst)
          (if (= lst nil)
              nil
              (cons (f (car lst))
                    (map f (cdr lst))))))
      (define double
        (lambda (x) (* x 2)))
      (map double '(1 2 3)))
  "
  let assert Ok(Some(#(parsed, _))) = parser.parse(input)
  let assert Ok(#(result, _)) = eval.eval(env, parsed)

  let expected =
    value.Cons(
      value.Integer(2),
      value.Cons(value.Integer(4), value.Cons(value.Integer(6), value.Nil)),
    )

  should.equal(result, expected)
}

// Error cases

pub fn eval_undefined_variable_error_test() {
  case eval_string("undefined-var") {
    Error(eval.UndefinedVariableError("undefined-var")) -> True
    _ -> False
  }
  |> should.be_true()
}

pub fn eval_arity_error_test() {
  case eval_string("(+ 1 2 3 4 5 6 7 8 9 10)") {
    Ok(_) -> True
    _ -> False
  }
  |> should.be_true()

  case eval_string("(/ 1)") {
    Error(eval.ArityError("/", 2, 1)) -> True
    _ -> False
  }
  |> should.be_true()
}

pub fn eval_type_error_test() {
  case eval_string("(+ 1 \"string\")") {
    Error(eval.TypeError(_)) -> True
    _ -> False
  }
  |> should.be_true()
}

pub fn eval_invalid_lambda_params_test() {
  case eval_string("((lambda (1 2) (+ 1 2)) 3 4)") {
    Error(eval.TypeError(_)) -> True
    _ -> False
  }
  |> should.be_true()
}

// Dict tests

pub fn eval_dict_literal_test() {
  let input = "{x 10 y 20}"
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

pub fn eval_dict_empty_test() {
  assert_eval("{}", value.Dict(dict.new()))
}

pub fn eval_dict_nested_test() {
  let input = "{a {b 1}}"
  let result = eval_string(input)

  case result {
    Ok(value.Dict(d)) -> {
      let a_val = dict.get(d, value.Symbol("a"))
      case a_val {
        Ok(value.Dict(inner)) -> {
          let b_val = dict.get(inner, value.Symbol("b"))
          should.equal(b_val, Ok(value.Integer(1)))
        }
        _ -> panic as "Expected nested dict"
      }
    }
    _ -> panic as "Expected Dict value"
  }
}
