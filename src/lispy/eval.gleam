import gleam/dict
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import lispy/builtins
import lispy/environment.{type Environment}
import lispy/parser
import lispy/value.{type Value}

pub type EvaluationError {
  ZeroDivisionError
  UndefinedVariableError(String)
  InvalidFormError(Value)
  ArityError(name: String, expected: Int, actual: Int)
  TypeError(String)
}

pub fn eval(
  environment: Environment(Value),
  form: Value,
) -> Result(#(Value, Environment(Value)), EvaluationError) {
  case form {
    value.Integer(_)
    | value.Double(_)
    | value.String(_)
    | value.Boolean(_)
    | value.Nil
    | value.Dict(_) -> Ok(#(form, environment))

    value.Lambda(_, _, _) | value.Macro(_, _) | value.Builtin(_) ->
      Ok(#(form, environment))

    value.Symbol(name) -> {
      case environment.get(environment, name) {
        Some(val) -> Ok(#(val, environment))
        None -> Error(UndefinedVariableError(name))
      }
    }

    value.Cons(value.Symbol(name), rest) -> {
      case name {
        "quote" -> eval_quote(environment, rest)
        "if" -> eval_if(environment, rest)
        "define" -> eval_define(environment, rest)
        "set!" -> eval_set(environment, rest)
        "lambda" | "λ" -> eval_lambda(environment, rest)
        "begin" -> eval_begin(environment, rest)
        "load" -> eval_load(environment, rest)

        _ -> eval_application(environment, form)
      }
    }

    value.Cons(_, _) -> eval_application(environment, form)
  }
}

fn eval_quote(
  environment: Environment(Value),
  args: Value,
) -> Result(#(Value, Environment(Value)), EvaluationError) {
  case args {
    value.Cons(quoted, value.Nil) -> Ok(#(quoted, environment))
    _ -> Error(ArityError("quote", 1, value.count_list(args)))
  }
}

fn eval_if(
  environment: Environment(Value),
  args: Value,
) -> Result(#(Value, Environment(Value)), EvaluationError) {
  case args {
    value.Cons(
      condition,
      value.Cons(consequent, value.Cons(alternate, value.Nil)),
    ) -> {
      use #(test_result, env1) <- result.try(eval(environment, condition))
      case test_result {
        value.Boolean(False) | value.Nil -> eval(env1, alternate)
        _ -> eval(env1, consequent)
      }
    }
    value.Cons(condition, value.Cons(consequent, value.Nil)) -> {
      use #(test_result, env1) <- result.try(eval(environment, condition))
      case test_result {
        value.Boolean(False) | value.Nil -> Ok(#(value.Nil, env1))
        _ -> eval(env1, consequent)
      }
    }
    _ -> Error(ArityError("if", 2, value.count_list(args)))
  }
}

fn eval_define(
  environment: Environment(Value),
  args: Value,
) -> Result(#(Value, Environment(Value)), EvaluationError) {
  case args {
    value.Cons(value.Symbol(name), value.Cons(expr, value.Nil)) -> {
      use #(val, env1) <- result.try(eval(environment, expr))
      let new_env = environment.define(env1, name, val)
      Ok(#(val, new_env))
    }
    _ -> Error(InvalidFormError(value.Cons(value.Symbol("define"), args)))
  }
}

fn eval_set(
  environment: Environment(Value),
  args: Value,
) -> Result(#(Value, Environment(Value)), EvaluationError) {
  case args {
    value.Cons(value.Symbol(name), value.Cons(expr, value.Nil)) -> {
      use #(val, env1) <- result.try(eval(environment, expr))
      case environment.set(env1, name, val) {
        Ok(new_env) -> Ok(#(val, new_env))
        Error(msg) -> Error(UndefinedVariableError(msg))
      }
    }
    _ -> Error(InvalidFormError(value.Cons(value.Symbol("set!"), args)))
  }
}

fn eval_lambda(
  environment: Environment(Value),
  args: Value,
) -> Result(#(Value, Environment(Value)), EvaluationError) {
  case args {
    value.Cons(params, body) -> {
      use param_list <- result.try(list_to_gleam_list(params))
      use body_list <- result.try(list_to_gleam_list(body))
      Ok(#(value.Lambda(param_list, body_list, environment), environment))
    }
    _ -> Error(InvalidFormError(value.Cons(value.Symbol("lambda"), args)))
  }
}

fn eval_begin(
  environment: Environment(Value),
  forms: Value,
) -> Result(#(Value, Environment(Value)), EvaluationError) {
  case forms {
    value.Nil -> Ok(#(value.Nil, environment))
    value.Cons(last, value.Nil) -> eval(environment, last)
    value.Cons(first, rest) -> {
      use #(_, env1) <- result.try(eval(environment, first))
      eval_begin(env1, rest)
    }
    _ -> Error(InvalidFormError(value.Cons(value.Symbol("begin"), forms)))
  }
}

fn eval_load(
  environment: Environment(Value),
  args: Value,
) -> Result(#(Value, Environment(Value)), EvaluationError) {
  case args {
    value.Cons(value.String(filepath), value.Nil) -> {
      case load_file_helper(filepath, environment) {
        Ok(new_env) -> Ok(#(value.Nil, new_env))
        Error(msg) -> Error(TypeError("Load error: " <> msg))
      }
    }
    _ -> Error(InvalidFormError(value.Cons(value.Symbol("load"), args)))
  }
}

fn load_file_helper(
  filepath: String,
  env: Environment(Value),
) -> Result(Environment(Value), String) {
  case simplifile_read(filepath) {
    Ok(content) -> load_string(content, env)
    Error(_) -> Error("Could not read file: " <> filepath)
  }
}

fn load_string(
  input: String,
  env: Environment(Value),
) -> Result(Environment(Value), String) {
  case parser_parse(input) {
    Ok(option.Some(#(parsed, rest))) -> {
      case eval(env, parsed) {
        Ok(#(_, new_env)) -> {
          case string.trim(rest) {
            "" -> Ok(new_env)
            remaining -> load_string(remaining, new_env)
          }
        }
        Error(err) -> Error(error_to_string_helper(err))
      }
    }
    Ok(option.None) -> Ok(env)
    Error(err) -> Error("Parse error: " <> err)
  }
}

@external(erlang, "simplifile", "read")
fn simplifile_read(filepath: String) -> Result(String, Nil)

fn parser_parse(
  input: String,
) -> Result(option.Option(#(Value, String)), String) {
  parser.parse(input)
}

fn error_to_string_helper(err: EvaluationError) -> String {
  case err {
    ZeroDivisionError -> "Division by zero"
    UndefinedVariableError(name) -> "Undefined variable: " <> name
    InvalidFormError(_) -> "Invalid form"
    ArityError(name, expected, got) ->
      "Arity error in "
      <> name
      <> ": expected "
      <> int_to_str(expected)
      <> " got "
      <> int_to_str(got)
    TypeError(msg) -> msg
  }
}

fn int_to_str(n: Int) -> String {
  int.to_string(n)
}

fn eval_application(
  environment: Environment(Value),
  form: Value,
) -> Result(#(Value, Environment(Value)), EvaluationError) {
  case form {
    value.Cons(func_expr, args_expr) -> {
      use #(func, env1) <- result.try(eval(environment, func_expr))
      use #(args, env2) <- result.try(eval_list(env1, args_expr))
      use result_val <- result.try(apply(env2, func, args))
      Ok(#(result_val, env2))
    }
    _ -> Error(InvalidFormError(form))
  }
}

fn eval_list(
  environment: Environment(Value),
  forms: Value,
) -> Result(#(List(Value), Environment(Value)), EvaluationError) {
  case forms {
    value.Nil -> Ok(#([], environment))
    value.Cons(first, rest) -> {
      use #(first_val, env1) <- result.try(eval(environment, first))
      use #(rest_vals, env2) <- result.try(eval_list(env1, rest))
      Ok(#([first_val, ..rest_vals], env2))
    }
    _ -> Error(TypeError("Expected list"))
  }
}

fn apply(
  current_env: Environment(Value),
  func: Value,
  args: List(Value),
) -> Result(Value, EvaluationError) {
  case func {
    value.Lambda(params, body, closure_env) -> {
      apply_lambda(current_env, params, body, closure_env, args)
    }

    value.Builtin(name) -> {
      apply_builtin(name, args)
    }

    _ -> Error(TypeError("Not a function: " <> value.to_string(func)))
  }
}

fn apply_lambda(
  current_env: Environment(Value),
  params: List(Value),
  body: List(Value),
  closure_env: Environment(Value),
  args: List(Value),
) -> Result(Value, EvaluationError) {
  let param_count = list.length(params)
  let arg_count = list.length(args)

  case param_count == arg_count {
    False -> Error(ArityError("lambda", param_count, arg_count))
    True -> {
      // Hybrid scoping: use closure_env for captured variables, but merge with
      // current_env to support recursive function definitions
      let base_env = environment.merge(closure_env, current_env)
      let new_env = environment.extend(base_env)
      use bound_env <- result.try(bind_params(new_env, params, args))
      eval_body(bound_env, body)
    }
  }
}

fn bind_params(
  env: Environment(Value),
  params: List(Value),
  args: List(Value),
) -> Result(Environment(Value), EvaluationError) {
  case params, args {
    [], [] -> Ok(env)
    [value.Symbol(name), ..rest_params], [arg, ..rest_args] -> {
      let new_env = environment.define(env, name, arg)
      bind_params(new_env, rest_params, rest_args)
    }
    _, _ -> Error(TypeError("Invalid parameter list"))
  }
}

fn eval_body(
  env: Environment(Value),
  body: List(Value),
) -> Result(Value, EvaluationError) {
  case body {
    [] -> Ok(value.Nil)
    [expr] -> {
      use #(val, _) <- result.try(eval(env, expr))
      Ok(val)
    }
    [expr, ..rest] -> {
      use #(_, env1) <- result.try(eval(env, expr))
      eval_body(env1, rest)
    }
  }
}

fn apply_builtin(
  name: String,
  args: List(Value),
) -> Result(Value, EvaluationError) {
  case name {
    // Arithmetic
    "+" -> builtin_add(args)
    "-" -> builtin_sub(args)
    "*" -> builtin_mul(args)
    "/" -> builtin_div(args)
    "mod" -> builtin_mod(args)

    // Comparison
    "=" -> builtin_eq(args)
    ">" -> builtin_gt(args)
    "<" -> builtin_lt(args)

    // String
    "++" -> builtin_concat(args)

    // List operations
    "cons" -> builtin_cons(args)
    "car" -> builtin_car(args)
    "cdr" -> builtin_cdr(args)
    "list" -> builtin_list(args)

    // Dict operations
    "dict-get" -> builtin_dict_get(args)
    "dict-set" -> builtin_dict_set(args)
    "dict-keys" -> builtin_dict_keys(args)
    "dict-values" -> builtin_dict_values(args)
    "dict-has?" -> builtin_dict_has(args)

    // I/O
    "print" -> builtin_print(args)
    "read" -> builtin_read(args)

    // Other
    "eval" -> builtin_eval(args)
    "symbol" -> builtin_symbol(args)

    _ -> Error(UndefinedVariableError(name))
  }
}

fn builtin_add(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [] -> Ok(value.Integer(0))
    _ -> {
      use sum <- result.try(
        fold_numbers(args, 0.0, fn(acc, n) { Ok(acc +. n) }),
      )
      number_to_value(sum)
    }
  }
}

fn builtin_sub(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [] -> Error(ArityError("-", 1, 0))
    [first, ..rest] -> {
      use first_num <- result.try(value_to_number(first))
      case rest {
        [] -> number_to_value(0.0 -. first_num)
        _ -> {
          use sum <- result.try(
            fold_numbers(rest, first_num, fn(acc, n) { Ok(acc -. n) }),
          )
          number_to_value(sum)
        }
      }
    }
  }
}

fn builtin_mul(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [] -> Ok(value.Integer(1))
    _ -> {
      use product <- result.try(
        fold_numbers(args, 1.0, fn(acc, n) { Ok(acc *. n) }),
      )
      number_to_value(product)
    }
  }
}

fn builtin_div(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [a, b] -> {
      use a_num <- result.try(value_to_number(a))
      use b_num <- result.try(value_to_number(b))
      case b_num {
        0.0 -> Error(ZeroDivisionError)
        _ -> Ok(value.Double(a_num /. b_num))
      }
    }
    _ -> Error(ArityError("/", 2, list.length(args)))
  }
}

fn builtin_mod(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [value.Integer(a), value.Integer(b)] -> {
      case b {
        0 -> Error(ZeroDivisionError)
        _ -> Ok(value.Integer(a % b))
      }
    }
    [_, _] -> Error(TypeError("mod requires integer arguments"))
    _ -> Error(ArityError("mod", 2, list.length(args)))
  }
}

fn builtin_eq(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [a, b] -> Ok(value.Boolean(value.equal(a, b)))
    _ -> Error(ArityError("=", 2, list.length(args)))
  }
}

fn builtin_gt(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [a, b] -> {
      use a_num <- result.try(value_to_number(a))
      use b_num <- result.try(value_to_number(b))
      Ok(value.Boolean(a_num >. b_num))
    }
    _ -> Error(ArityError(">", 2, list.length(args)))
  }
}

fn builtin_lt(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [a, b] -> {
      use a_num <- result.try(value_to_number(a))
      use b_num <- result.try(value_to_number(b))
      Ok(value.Boolean(a_num <. b_num))
    }
    _ -> Error(ArityError("<", 2, list.length(args)))
  }
}

fn builtin_concat(args: List(Value)) -> Result(Value, EvaluationError) {
  use strings <- result.try(
    list.try_map(args, fn(arg) {
      case arg {
        value.String(s) -> Ok(s)
        _ -> Error(TypeError("++ requires string arguments"))
      }
    }),
  )
  Ok(value.String(string.concat(strings)))
}

fn builtin_cons(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [car, cdr] -> Ok(value.Cons(car, cdr))
    _ -> Error(ArityError("cons", 2, list.length(args)))
  }
}

fn builtin_car(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [value.Cons(car, _)] -> Ok(car)
    [value.Nil] -> Error(TypeError("car of nil"))
    [_] -> Error(TypeError("car requires a cons cell"))
    _ -> Error(ArityError("car", 1, list.length(args)))
  }
}

fn builtin_cdr(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [value.Cons(_, cdr)] -> Ok(cdr)
    [value.Nil] -> Error(TypeError("cdr of nil"))
    [_] -> Error(TypeError("cdr requires a cons cell"))
    _ -> Error(ArityError("cdr", 1, list.length(args)))
  }
}

fn builtin_list(args: List(Value)) -> Result(Value, EvaluationError) {
  Ok(value.from_list(args))
}

fn builtin_print(args: List(Value)) -> Result(Value, EvaluationError) {
  list.each(args, fn(arg) { io.println(value.show(arg)) })
  Ok(value.Nil)
}

fn builtin_read(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [] -> {
      // TODO: реализовать чтение из stdin
      Error(TypeError("read not implemented yet"))
    }
    _ -> Error(ArityError("read", 0, list.length(args)))
  }
}

fn builtin_eval(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [expr] -> {
      use #(val, _) <- result.try(eval(builtins.create_global_env(), expr))
      Ok(val)
    }
    _ -> Error(ArityError("eval", 1, list.length(args)))
  }
}

fn builtin_symbol(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [value.String(s)] -> Ok(value.Symbol(s))
    [_] -> Error(TypeError("symbol requires a string argument"))
    _ -> Error(ArityError("symbol", 1, list.length(args)))
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

fn value_to_number(val: Value) -> Result(Float, EvaluationError) {
  case val {
    value.Integer(n) -> Ok(int.to_float(n))
    value.Double(f) -> Ok(f)
    _ -> Error(TypeError("Expected number, got: " <> value.to_string(val)))
  }
}

fn number_to_value(n: Float) -> Result(Value, EvaluationError) {
  let trunc = float.truncate(n)

  case int.to_float(trunc) == n {
    True -> Ok(value.Integer(trunc))
    False -> Ok(value.Double(n))
  }
}

fn fold_numbers(
  values: List(Value),
  init: Float,
  f: fn(Float, Float) -> Result(Float, EvaluationError),
) -> Result(Float, EvaluationError) {
  list.try_fold(values, init, fn(acc, val) {
    use n <- result.try(value_to_number(val))
    f(acc, n)
  })
}

// Dict operations

fn builtin_dict_get(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [value.Dict(d), key] -> {
      case dict.get(d, key) {
        Ok(val) -> Ok(val)
        Error(_) -> Ok(value.Nil)
      }
    }
    [_, _] -> Error(TypeError("dict-get requires a dict as first argument"))
    _ -> Error(ArityError("dict-get", 2, list.length(args)))
  }
}

fn builtin_dict_set(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [value.Dict(d), key, val] -> {
      Ok(value.Dict(dict.insert(d, key, val)))
    }
    [_, _, _] -> Error(TypeError("dict-set requires a dict as first argument"))
    _ -> Error(ArityError("dict-set", 3, list.length(args)))
  }
}

fn builtin_dict_keys(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [value.Dict(d)] -> {
      let keys = dict.keys(d)
      Ok(value.from_list(keys))
    }
    [_] -> Error(TypeError("dict-keys requires a dict argument"))
    _ -> Error(ArityError("dict-keys", 1, list.length(args)))
  }
}

fn builtin_dict_values(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [value.Dict(d)] -> {
      let values = dict.values(d)
      Ok(value.from_list(values))
    }
    [_] -> Error(TypeError("dict-values requires a dict argument"))
    _ -> Error(ArityError("dict-values", 1, list.length(args)))
  }
}

fn builtin_dict_has(args: List(Value)) -> Result(Value, EvaluationError) {
  case args {
    [value.Dict(d), key] -> {
      case dict.has_key(d, key) {
        True -> Ok(value.Boolean(True))
        False -> Ok(value.Boolean(False))
      }
    }
    [_, _] -> Error(TypeError("dict-has? requires a dict as first argument"))
    _ -> Error(ArityError("dict-has?", 2, list.length(args)))
  }
}
