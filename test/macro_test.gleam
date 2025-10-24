import gleam/option.{Some}
import gleeunit/should

import lispy/builtins
import lispy/error
import lispy/eval
import lispy/parser
import lispy/value

pub fn test_macro_simple_identity() {
  let env = builtins.create_global_env()

  let assert Ok(Some(#(def_expr, _))) =
    parser.parse("(defmacro my-macro (x) x))")
  let assert Ok(#(_, env1)) = eval.eval(env, def_expr)

  let assert Ok(Some(#(call_expr, _))) = parser.parse("(my-macro 42)")
  let assert Ok(#(result, _)) = eval.eval(env1, call_expr)

  should.equal(result, value.Integer(42))
}

pub fn test_macro_code_transformation() {
  let env = builtins.create_global_env()

  let assert Ok(Some(#(def_expr, _))) =
    parser.parse("(defmacro unless (cond then else) (if cond else then)))")
  let assert Ok(#(_, env1)) = eval.eval(env, def_expr)

  let assert Ok(Some(#(call_expr, _))) = parser.parse("(unless #f 1 2)")
  let assert Ok(#(result, _)) = eval.eval(env1, call_expr)

  should.equal(result, value.Integer(1))
}

pub fn test_macro_with_computation() {
  let env = builtins.create_global_env()

  let assert Ok(Some(#(def_expr, _))) =
    parser.parse("(defmacro add-one (x) (+ x 1)))")
  let assert Ok(#(_, env1)) = eval.eval(env, def_expr)

  let assert Ok(Some(#(call_expr, _))) = parser.parse("(add-one 5)")
  let assert Ok(#(result, _)) = eval.eval(env1, call_expr)

  should.equal(result, value.Integer(6))
}

pub fn test_macro_nested_expansion() {
  let env = builtins.create_global_env()

  let assert Ok(Some(#(def_id, _))) = parser.parse("(defmacro id (x) x))")
  let assert Ok(#(_, env1)) = eval.eval(env, def_id)

  let assert Ok(Some(#(def_wrap, _))) =
    parser.parse("(defmacro wrap (x) (id x)))")
  let assert Ok(#(_, env2)) = eval.eval(env1, def_wrap)

  let assert Ok(Some(#(call_expr, _))) = parser.parse("(wrap 42)")
  let assert Ok(#(result, _)) = eval.eval(env2, call_expr)

  should.equal(result, value.Integer(42))
}

pub fn test_macro_with_unevaluated_args() {
  let env = builtins.create_global_env()

  let assert Ok(Some(#(def_expr, _))) =
    parser.parse("(defmacro quote-it (x) (quote x)))")
  let assert Ok(#(_, env1)) = eval.eval(env, def_expr)

  let assert Ok(Some(#(call_expr, _))) =
    parser.parse("(quote-it undefined-symbol)")
  let assert Ok(#(result, _)) = eval.eval(env1, call_expr)

  should.equal(result, value.Symbol("undefined-symbol"))
}

pub fn test_expand_macro_simple() {
  let env = builtins.create_global_env()

  let assert Ok(Some(#(def_expr, _))) =
    parser.parse("(defmacro my-macro (x) x))")
  let assert Ok(#(_, env1)) = eval.eval(env, def_expr)

  let assert Ok(Some(#(expand_expr, _))) =
    parser.parse("(expand-macro (my-macro 42))")
  let assert Ok(#(result, _)) = eval.eval(env1, expand_expr)

  should.equal(result, value.Integer(42))
}

pub fn test_expand_macro_unless() {
  let env = builtins.create_global_env()

  let assert Ok(Some(#(def_expr, _))) =
    parser.parse("(defmacro unless (cond then else) (if cond else then)))")
  let assert Ok(#(_, env1)) = eval.eval(env, def_expr)

  let assert Ok(Some(#(expand_expr, _))) =
    parser.parse("(expand-macro (unless #f 1 2))")
  let assert Ok(#(result, _)) = eval.eval(env1, expand_expr)

  // Should expand to (if #f 2 1)
  let expected =
    value.Cons(
      value.Symbol("if"),
      value.Cons(
        value.Boolean(False),
        value.Cons(value.Integer(2), value.Cons(value.Integer(1), value.Nil)),
      ),
    )
  should.equal(result, expected)
}

pub fn test_expand_macro_nested() {
  let env = builtins.create_global_env()

  let assert Ok(Some(#(def_id, _))) = parser.parse("(defmacro id (x) x))")
  let assert Ok(#(_, env1)) = eval.eval(env, def_id)

  let assert Ok(Some(#(def_wrap, _))) =
    parser.parse("(defmacro wrap (x) (id x)))")
  let assert Ok(#(_, env2)) = eval.eval(env1, def_wrap)

  let assert Ok(Some(#(expand_expr, _))) =
    parser.parse("(expand-macro (wrap 42))")
  let assert Ok(#(result, _)) = eval.eval(env2, expand_expr)

  should.equal(result, value.Integer(42))
}

pub fn test_expand_macro_no_macros() {
  let env = builtins.create_global_env()

  let assert Ok(Some(#(expand_expr, _))) =
    parser.parse("(expand-macro (+ 1 2))")
  let assert Ok(#(result, _)) = eval.eval(env, expand_expr)

  let expected =
    value.Cons(
      value.Symbol("+"),
      value.Cons(value.Integer(1), value.Cons(value.Integer(2), value.Nil)),
    )
  should.equal(result, expected)
}

pub fn test_macro_multi_step() {
  let env = builtins.create_global_env()

  let assert Ok(Some(#(def_expr, _))) =
    parser.parse("(defmacro when (cond body) (if cond (begin body) nil)))")
  let assert Ok(#(_, env1)) = eval.eval(env, def_expr)

  let assert Ok(Some(#(call_expr, _))) = parser.parse("(when #t 42)")
  let assert Ok(#(result, _)) = eval.eval(env1, call_expr)

  should.equal(result, value.Integer(42))

  let assert Ok(Some(#(expand_expr, _))) =
    parser.parse("(expand-macro (when #t 42))")
  let assert Ok(#(expanded, _)) = eval.eval(env1, expand_expr)

  // Should expand to (if #t (begin 42) nil)
  let expected =
    value.Cons(
      value.Symbol("if"),
      value.Cons(
        value.Boolean(True),
        value.Cons(
          value.Cons(
            value.Symbol("begin"),
            value.Cons(value.Integer(42), value.Nil),
          ),
          value.Cons(value.Nil, value.Nil),
        ),
      ),
    )
  should.equal(expanded, expected)
}

pub fn test_macro_arity_error() {
  let env = builtins.create_global_env()

  let assert Ok(Some(#(def_expr, _))) =
    parser.parse("(defmacro my-macro (x y) x))")
  let assert Ok(#(_, env1)) = eval.eval(env, def_expr)

  let assert Ok(Some(#(call_expr, _))) = parser.parse("(my-macro 1)")

  case eval.eval(env1, call_expr) {
    Error(error.ArityError("macro", 2, 1)) -> True
    _ -> False
  }
  |> should.be_true()
}

pub fn test_defmacro_backwards_compat() {
  let env = builtins.create_global_env()

  let assert Ok(Some(#(def_expr, _))) =
    parser.parse("(defmacro old-style (x) x)")
  let assert Ok(#(_, env1)) = eval.eval(env, def_expr)

  let assert Ok(Some(#(call_expr, _))) = parser.parse("(old-style 42)")
  let assert Ok(#(result, _)) = eval.eval(env1, call_expr)

  should.equal(result, value.Integer(42))
}
