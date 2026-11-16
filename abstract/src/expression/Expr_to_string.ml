include AST
include Register

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

let rec str_value (v: value): string  =
  match v with
  | VClosure ((v, ex), en)-> "closure \\"^v^"."^(str_expr ex)^(Format.sprintf " env: [%s]" (String.concat ", " (str_env en)))
  | VInt n -> Format.sprintf "%d" n
  | VBool b -> Format.sprintf "%b" b

and str_expr (e : expr) : string = 
  match e with
  | EVar x -> Format.sprintf "var %s" x
  | EApp (e1, e2) -> Format.sprintf  "%s @ %s" (str_expr e1) (str_expr e2)
  | EAbs (v, e) -> Format.sprintf "\\%s. %s" v (str_expr e)
  | EInt n -> Format.sprintf "Int %d" n
  | EBool b -> Format.sprintf  "Bool %b" b
  | EBinOp (op, e1, e2) -> Format.sprintf  "%s %s %s"  (str_expr e1) (str_op op) (str_expr e2)
  | EReturn v -> Format.sprintf "Return %s" (str_value v)
  | EIf (e1, e2, e3) -> Format.sprintf "if %s then %s else %s" (str_expr e1) (str_expr e2) (str_expr e3)

and str_env (e: env) : string list = 
  match e with
  | ValEnv ((var, v) :: xs) -> (var^" = "^(str_value v)) :: (str_env  (ValEnv xs))
  | ValEnv [] -> []
  | AddrEnv ((var, addr)::xs) -> (var^(Format.sprintf " -> %d" addr)) :: (str_env  (AddrEnv xs))
  | AddrEnv [] -> []

let print_expr (e: expr) = 
  Format.sprintf "%s \n" (str_expr e)

let print_env (e:env) = 
  Format.sprintf "Env: [%s] \n" (String.concat ", " (str_env e))

let rec str_stack (s:stack) : string list = 
  match s with
  | v::vs -> (str_value v) :: (str_stack vs)
  | [] -> []

let print_stack (s: stack) = Format.sprintf "Stack: [%s] \n" (String.concat ", " (str_stack s) )

let str_ctrl (c:ctrl) : string = 
  match c with 
  | CExpr exp -> str_expr exp
  | CApp -> "Apply"
  | COp op -> str_op op
  | CIf -> "If"

let rec str_ctrl_list (c: ctrl list): string list =
  match c with
  | c::cs -> (str_ctrl c) :: (str_ctrl_list cs)
  | [] -> []

let print_ctrl (c:ctrl list) = Format.sprintf "C: [%s] \n" (String.concat ", " (str_ctrl_list c))


let str_storable (s: storable) : string = 
  match s with
  | SValue v-> str_value v
  | SExpr e -> "Expr"^(str_expr e)
  | SThunk t ->
    match t with
    | TComputed (v, _)-> "Computed Thunk "^(str_value v)
    | TDelayed (e, _)-> "Delayed Thunk "^(str_expr e)

let rec str_store (s: store) : string list = 
  match s with
  | (addr, v)::xs -> ((Format.sprintf "%d -> " addr)^(str_storable v)) :: (str_store xs)
  | [] -> []

let print_store (s:store) = Format.sprintf "Store: [%s] \n" (String.concat ", " (str_store s))

let rec str_kont (k: kont) : string list = 
  match k with
  | Empty -> []
  | KAppArg(e, _, k')         -> ("KArg "^(str_expr e)) :: (str_kont k')
  | KAppFun((v,e), _, k')     -> ("KFun lam "^v^"."^(str_expr e)) :: (str_kont k')
  | KBinApp1 (op,  e, _, k')  -> ("KopL "^(str_op op)^(str_expr e)) :: (str_kont k')
  | KBinApp2 (op, v, _, k')   -> ("KopR "^(str_value v)^(str_op op)) :: (str_kont k')
  | KIf (eThen, eElse, _, k') -> ("Kif t: "^(str_expr eThen)^" e: "^(str_expr eElse)) :: (str_kont k')

let print_kont (k:kont) = Format.sprintf "K: [%s] \n" (String.concat ", " (str_kont k))

let str_kframe (k : kframe) : string = 
  match k with
  | KFun a -> "KFun"^(Format.sprintf "-> %d" a)
  | KArg a -> "KArg"^(Format.sprintf "-> %d" a)
  | KOpLeft (o, ex, en) -> "KOpL "^(str_op o)^" "^(str_expr ex)^(print_env en)
  | KOpRight (o, v) -> "KOpR "^(str_value v)^" "^(str_op o)
  | KIf (eThen, eElse, _) -> "Kif t: "^(str_expr eThen)^" e: "^(str_expr eElse)

let rec str_kontstack (kstack : kontstack) : string list =
  match kstack with
  | (k :: ks) -> (str_kframe k) :: (str_kontstack ks)
  | [] -> []

let print_kontstack k = Format.sprintf "K: [%s]\n" (String.concat ", " (str_kontstack k))

let rec str_dump (d : dump) : string list =
  match d with
  | (s, e, c):: (ds) -> ((print_stack s)^(print_env e)^(print_ctrl c)) :: str_dump ds
  | [] -> []

let print_dump (d : dump) : string = 
  Format.sprintf "Dump: [%s]\n" (String.concat ", "(str_dump d))