type op = Add | Mul | Sub | Div | Eq
type var = string

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

and env = (var * value) list
(*list.assoc key alist (kxV list _> k ->V)*)
and kont = Empty 
  | KAppArg of (expr * env * kont)
  | KAppFun of (lambda * env * kont)
  | KBinApp1 of (op * expr * env * kont)
  | KBinApp2 of (op * value * env * kont)
  | KIf of (expr * expr * env * kont)
  | KReturn of (value * expr * env * kont)
;;


let str_op (o: op): string =
  match o with
  | Add -> "+"
  | Mul -> "*"
  | Sub -> "-"
  | Div -> "/"
  | Eq -> "=="

let str_value (v: value): string  =
  match v with
  | VClosure _-> "closure"
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



let eval v1 k : value =  
  match k with
  | KBinApp2(op, v2, e, k) -> (
      match v1, v2 with 
      | VInt n1, VInt n2 -> (
        match op with 
        | Eq -> VBool(n1 == n2)
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
  )
  | _ -> invalid_arg "trying to evaluate invalid kont"
;;

