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
  | KFun of addr
  | KArg of addr
  | KOpLeft of op * expr * env
  | KOpRight of op * value
  | KIf of expr * expr * env

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
  | EApp (e1, e2) -> Format.sprintf  "%s (%s)" (str_expr e1) (str_expr e2)
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
  | v::vs -> str_value v ^",\n"^ str_stack vs
  | [] -> "]"

let print_stack ppf (s: stack) = Format.fprintf ppf "S: [%s \n" (str_stack s) 

let str_ctrl (c:ctrl) : string = 
  match c with 
  | CExpr exp -> str_expr exp
  | CApp -> "Apply"
  | COp op -> str_op op
  | CIf -> "If"

let rec str_ctrl_list (c: ctrl list): string =
  match c with
  | c::cs -> str_ctrl c ^",\n"^ str_ctrl_list cs
  | [] -> "]"

let print_ctrl ppf (c:ctrl list) = Format.fprintf ppf "C: [%s \n" (str_ctrl_list c)

let rec str_env (e: env) : string = 
  match e with
  | ValEnv ((var, v) :: xs) -> var^" = "^(str_value v)^",\n"^(str_env  (ValEnv xs))
  | ValEnv [] -> "]"
  | AddrEnv ((var, addr)::xs) -> var^(Format.sprintf " -> %d" addr)^",\n"^(str_env  (AddrEnv xs))
  | AddrEnv [] -> "]"

let print_env ppf (e:env) = Format.fprintf ppf "Env: [%s \n" (str_env e)

let str_storable (s: storable) : string = 
  match s with
  | SValue v-> str_value v
  | SExpr e -> "Expr"^(str_expr e)
  | SThunk t ->
    match t with
    | TComputed (v, _)-> "CThunk "^(str_value v)
    | TDelayed (e, _)-> "DThunk "^(str_expr e)


let rec str_store (s: store) : string = 
  match s with
  | (addr, v)::xs -> (Format.sprintf "%d ->" addr)^(str_storable v)^",\n"^(str_store xs)
  | [] -> "]"

let print_store ppf (s:store) = Format.fprintf ppf "Str: [%s \n" (str_store s)


let rec str_kont (k: kont) : string = 
  match k with
  | Empty -> "]"
  | KAppArg(e, _, k') -> "KArg "^(str_expr e)^",\n"^(str_kont k')
  | KAppFun((v,e), _, k') -> "KFun lam "^v^"."^(str_expr e)^",\n"^(str_kont k')
  | KBinApp1 (op,  e, _, k') ->"KopL "^(str_op op)^(str_expr e)^",\n"^(str_kont k')
  | KBinApp2 (op, v, _, k') -> "KopR "^(str_op op)^(str_value v)^",\n"^(str_kont k')
  | KIf (eThen, eElse, _, k') -> "Kif t: "^(str_expr eThen)^" e: "^(str_expr eElse)^",\n"^(str_kont k')

let print_kont ppf (k:kont) = Format.fprintf ppf "K: [%s \n" (str_kont k)

let str_kframe (k : kframe) : string = 
  match k with
  | KFun a -> "KFun"^(Format.sprintf "-> %d" a)
  | KArg a -> "KArg"^(Format.sprintf "-> %d" a)
  | KOpLeft (o, ex, en) -> "KOpL "^(str_op o)^" e:"^(str_expr ex)^" "^(str_env en)
  | KOpRight (o, v) -> "KOpR "^(str_value v)^" "^(str_op o)
  | KIf (eThen, eElse, _) -> "Kif t: "^(str_expr eThen)^" e: "^(str_expr eElse)

let rec str_kontstack (kstack : kontstack) : string =
  match kstack with
  | (k :: ks) -> (str_kframe k)^"\n"^(str_kontstack ks)
  | [] -> "]"

let rec str_dump (d : dump) : string =
  match d with
  | (s, e, c):: (ds) -> "S: ["^(str_stack s)^",\nE: ["^(str_env e)^",\nC: ["^(str_ctrl_list c)
  | [] -> "]"

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

