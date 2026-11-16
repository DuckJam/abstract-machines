include AST

type kont = Empty 
  | KAppArg of (expr * env * kont)
  | KAppFun of (lambda * env * kont)
  | KBinApp1 of (op * expr * env * kont)
  | KBinApp2 of (op * value * env * kont)
  | KIf of (expr * expr * env * kont)

type thunk = 
    TDelayed of (expr * env)
  | TComputed of (value * env)

type storable = 
    SValue of value 
  | SExpr of expr
  | SThunk of thunk

type stack = value list

type store = (addr * storable) list

let next_addr (s : store) : addr = 
  if List.is_empty s then 0 else List.length s

type ctrl = 
    CExpr of expr
  | CApp
  | COp of op
  | CIf

type dump = (stack * env * ctrl list) list 

type kframe = 
  | KFun of addr
  | KArg of addr
  | KOpLeft of op * expr * env
  | KOpRight of op * value
  | KIf of expr * expr * env

type kontstack = kframe list
