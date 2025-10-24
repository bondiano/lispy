import gleam/dict
import gleam/option.{Some}

import lispy/parser
import lispy/value

pub fn parse_simple_list_test() {
  assert parser.parse("(1 2 3)")
    == Ok(
      Some(#(
        value.Cons(
          value.Integer(1),
          value.Cons(value.Integer(2), value.Cons(value.Integer(3), value.Nil)),
        ),
        "",
      )),
    )
}

pub fn parse_quote_test() {
  assert parser.parse("'(1 2 3)")
    == Ok(
      Some(#(
        value.Cons(
          value.Symbol("quote"),
          value.Cons(
            value.Cons(
              value.Integer(1),
              value.Cons(
                value.Integer(2),
                value.Cons(value.Integer(3), value.Nil),
              ),
            ),
            value.Nil,
          ),
        ),
        "",
      )),
    )
}

pub fn parse_string_test() {
  assert parser.parse("\"Hello, world!\"")
    == Ok(Some(#(value.String("Hello, world!"), "")))
}

pub fn parse_boolean_test() {
  assert parser.parse("#t") == Ok(Some(#(value.Boolean(True), "")))
  assert parser.parse("#f") == Ok(Some(#(value.Boolean(False), "")))
}

pub fn parse_nil_test() {
  assert parser.parse("nil") == Ok(Some(#(value.Nil, "")))
}

pub fn parse_number_test() {
  assert parser.parse("1") == Ok(Some(#(value.Integer(1), "")))
}

pub fn parse_dict_test() {
  assert parser.parse("{a 1 b 2}")
    == Ok(
      Some(#(
        value.Dict(
          dict.from_list([
            #(value.Symbol("a"), value.Integer(1)),
            #(value.Symbol("b"), value.Integer(2)),
          ]),
        ),
        "",
      )),
    )
}

pub fn parse_lambda_test() {
  assert parser.parse("(lambda (x) x)")
    == Ok(
      Some(#(
        value.Cons(
          value.Symbol("lambda"),
          value.Cons(
            value.Cons(value.Symbol("x"), value.Nil),
            value.Cons(value.Symbol("x"), value.Nil),
          ),
        ),
        "",
      )),
    )
}
