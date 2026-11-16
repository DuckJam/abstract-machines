type op = Add | Mul | Sub | Div | Eq | Lt | Gt | LEq | GEq
type var = string
type addr = int

type expr = 
    EVar of var 
  | EAbs of lambda
  | EApp of (expr * expr)
  | EInt of int 
  | EBool of bool
  | EBinOp of (op * expr * expr)
  | EReturn of value
  | EIf of (expr * expr * expr)

and lambda = (var * expr)
and value = 
    VClosure of (lambda * env)
  | VInt of int
  | VBool of bool
(* domain of values / dentorable values , 
closure is lambda term paired with environment that defines the values of its free variables*)

and env = 
    ValEnv of (var * value) list
  | AddrEnv of (var * addr) list
(* store env = (var * addr) list *)