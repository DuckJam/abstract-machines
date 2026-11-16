module type S = sig
  type state
  val inject : Expression.expr -> state
  val step : state -> state
  val is_final : state -> bool
  val string_state : state -> string