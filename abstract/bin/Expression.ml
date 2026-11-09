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


(*list.assoc key alist (kxV list _> k ->V)*)
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

type ctrl = 
    CExpr of expr
  | CApp
  | COp of op
  | CIf

type dump = (stack * env * ctrl list) list 


type kframe = 
  | KArg of addr
  | KOpLeft of op * expr * env
  | KOpRight of op * value
  | KIf of expr * expr

type kontstack = kframe list

let str_op (o: op): string =
  match o with
  | Add -> "+"
  | Mul -> "*"
  | Sub -> "-"
  | Div -> "/"
  | Eq -> "=="
  | Lt -> "<"
  | Gt -> ">"
  | LEq -> "<="
  | GEq -> ">="

let str_value (v: value): string  =
  match v with
  | VClosure ((v, e), _)-> "closure lam "^v
  | VInt n -> Format.sprintf "%d" n
  | VBool b -> Format.sprintf "%b" b

let rec str_expr (e : expr) : string = 
  match e with
  | EVar x -> Format.sprintf "var %s" x
  | EApp (e1, e2) -> Format.sprintf  "%s %s" (str_expr e1) (str_expr e2)
  | EAbs (v, e) -> Format.sprintf "lam %s %s" v (str_expr e)
  | EInt n -> Format.sprintf "Int %d" n
  | EBool b -> Format.sprintf  "Bool %b" b
  | EBinOp (op, e1, e2) -> Format.sprintf  "%s %s %s"  (str_expr e1) (str_op op) (str_expr e2)
  | EReturn v -> Format.sprintf "Return %s" (str_value v)
  | EIf (e1, e2, e3) -> Format.sprintf "if %s then %s else %s" (str_expr e1) (str_expr e2) (str_expr e3)
;;

let print_expr ppf (e: expr) = 
  Format.fprintf ppf "%s \n" (str_expr e)
;;

let rec str_stack (s:stack) = 
  match s with
  | v::vs -> str_value v ^", "^ str_stack vs
  | [] -> "]"

let print_stack ppf (s: stack) = Format.fprintf ppf "S: [%s \n" (str_stack s) 

let str_ctrl (c:ctrl) : string = 
  match c with 
  | CExpr exp -> str_expr exp
  | CApp -> "Apply"
  | COp op -> str_op op
  | CIf -> "If"

let rec prnt_ctrl (c: ctrl list): string =
  match c with
  | c::cs -> str_ctrl c ^", "^ prnt_ctrl cs
  | [] -> "]"

let print_ctrl ppf (c:ctrl list) = Format.fprintf ppf "C: [%s \n" (prnt_ctrl c)


let evalBinOp op v1 v2 : value =  
  match v1, v2 with 
  | VInt n1, VInt n2 -> (
    match op with 
    | Eq -> VBool(n1 == n2)
    | Lt -> VBool(n1 < n2)
    | Gt -> VBool(n1 > n2)
    | LEq -> VBool(n1 <= n2)
    | GEq -> VBool(n1 >= n2)
    | _ -> 
    let opFn op = 
      match op with
        | Add -> (+)
        | Mul -> ( * )
        | Sub -> (-)
        | Div -> (/)
        | _ -> invalid_arg "applying incorrect operator to two ints"
    in VInt((opFn op) n1 n2)
  )    
  | VBool b1, VBool b2 -> (
    match op with
      | Eq -> VBool(b1 == b2)
      | _ -> invalid_arg "trying to do anything other than eq with two bools"
  )
  | _ , _-> invalid_arg "mismatch of types in binop"
;;

