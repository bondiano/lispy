import lispy/environment.{type Environment}
import lispy/value.{type Value}

pub fn create_global_env() -> Environment(Value) {
  let env =
    environment.new()
    // Arithmetic
    |> environment.define("+", value.Builtin("+"))
    |> environment.define("-", value.Builtin("-"))
    |> environment.define("*", value.Builtin("*"))
    |> environment.define("/", value.Builtin("/"))
    |> environment.define("mod", value.Builtin("mod"))
    // Comparison
    |> environment.define("=", value.Builtin("="))
    |> environment.define(">", value.Builtin(">"))
    |> environment.define("<", value.Builtin("<"))
    // String
    |> environment.define("++", value.Builtin("++"))
    // List operations
    |> environment.define("cons", value.Builtin("cons"))
    |> environment.define("car", value.Builtin("car"))
    |> environment.define("cdr", value.Builtin("cdr"))
    |> environment.define("list", value.Builtin("list"))
    |> environment.define("null?", value.Builtin("null?"))
    // Dict operations
    |> environment.define("dict-get", value.Builtin("dict-get"))
    |> environment.define("dict-set", value.Builtin("dict-set"))
    |> environment.define("dict-keys", value.Builtin("dict-keys"))
    |> environment.define("dict-values", value.Builtin("dict-values"))
    |> environment.define("dict-has?", value.Builtin("dict-has?"))
    // I/O
    |> environment.define("print", value.Builtin("print"))
    |> environment.define("read", value.Builtin("read"))
    // Other
    |> environment.define("eval", value.Builtin("eval"))
    |> environment.define("symbol", value.Builtin("symbol"))
    |> environment.define("apply", value.Builtin("apply"))

  env
}
