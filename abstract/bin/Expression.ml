type op = Add | Mul | Sub | Div | Eq
type var = string

type expr = EUnit of ()
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
        | KReturn of (value * expr * env * kont)


let print_var (v: var): string = v

let print_expr ppf (e : expr) = 
    match e with
    | EVar x -> Format.fprintf ppf "var %s" x
    | EApp -> Format.fprintf ppf "App"
    | EAbs -> Format.fprintf ppf "lam"
    | EInt n -> Format.fprintf ppf "Int %d" n
    | EBool b -> Format.fprintf ppf "Bool %b" b
    | EBinOp -> Format.fprintf ppf "BinOp"
    | EReturn -> Format.fprintf ppf "Return"
 
let eval (v1: value) (k: kont): value =  
    match k with
        KBinApp2(op, v2, e, k) -> 
        match v1, v2 with 
        | VInt n1 VInt n2 -> 
                let opFn op = 
                        match op with
                        | Add -> (+)
                        | Mul -> ( * )
                        | Sub -> (-)
                        | Div -> (/)
                        | _ -> invalid_arg "applying incorrect operator to two ints"
                in VInt((opFn op) n n')
        | VBool b1 VBool b2 ->
                match op with
                | Eq -> VBool( b1 == b2)
                | _ -> invalid_arg "trying to do anything other than eq with two bools"

        | _ -> invalid_arg "mismatch of types in binop"

(*
    | EInt n -> n
    | EBinOp (op, e1, e2) -> 
        let opFn = 
            match op with
            | Add -> (+)
            | Mul -> ( * )
            | Sub -> (-)
            | Div -> (/)
        in
            (opFn op) (eval e1) (eval e2)
    | EApp (e1, e2) -> (eval e1) (eval e2)
    | EVar v1 -> 0
*) 
