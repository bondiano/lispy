import lispy/value.{type Value}

pub type EvaluationError {
  ZeroDivisionError
  UndefinedVariableError(String)
  InvalidFormError(Value)
  ArityError(name: String, expected: Int, actual: Int)
  TypeError(String)
}
