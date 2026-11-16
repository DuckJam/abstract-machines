module type S = sig
  type state
  val name : string
  val inject : Expression.expr -> state
  val step : state -> state
  val is_final : state -> bool
  val string_state : state -> string
  val view : state -> (string * string list) list 
end