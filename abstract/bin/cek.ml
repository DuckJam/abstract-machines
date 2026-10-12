type op = Add | Mul | Sub | Div
type var = string

type expr = EVar of var 
          | EAbs of lambda
          | EApp of (expr * expr)
          | EInt of int 
          | EBinOp of (op * expr * expr)
          | EReturn of value
and lambda = (var * expr)
and value = VClosure of (lambda * env)
        | VInt of int
and env = (var * value) list
(*list.assoc key alist (kxV list _> k ->V)*)
and kont = Empty 
          | KAppArg of (expr * env * kont)
          | KAppFun of (lambda * env * kont)
          | KBinApp1 of (op * expr * env * kont)
          | KBinApp2 of (op * value * env * kont)
          | KReturn of (value * expr * env * kont)

type state = (expr * env * kont)
(* domain of values / dentorable values , 
closure is lambda term paired with environment that defines the values of its free variables*)

let print_state ppf (s :state) = 
    match s with
    | (EVar x , _, _) -> Format.fprintf ppf "Evar of \n"
    | (EApp _, _, _) -> Format.fprintf ppf "EApp of\n"
    | (EAbs _, _, KAppArg _) -> Format.fprintf ppf "EAbs of arg\n"
    | (EAbs _, _, KAppFun _) -> Format.fprintf ppf "EAbs of fun\n"
    | (EAbs _, _, _) -> Format.fprintf ppf "EAbs of lam\n"
    | (EInt n, _, _ ) -> Format.fprintf ppf "EInt of\n"
    | (EBinOp _,_,_) -> Format.fprintf ppf "EBinOp of\n"
    | (EReturn _,_,_) -> Format.fprintf ppf "EReturn of\n"



let unwrapClosure v : lambda = 
    match v with
    | VClosure (lam, r) -> lam
    | _ -> invalid_arg "Trying to unwrap something that isn't a closure"


let step (s: state) : state = 
    match s with
    | (EVar x , r, k) -> 
        let v = List.assoc x r in (EReturn v, r, k)
    | (EApp (e1, e2), r , k ) -> (e1, r, KAppArg (e2, r, k))
    | (EAbs lam, r, KAppArg (e, r', k )) -> (e, r', KAppFun (lam, r, k))
    | (EAbs lam, r, KAppFun (lam', r', k )) -> (EReturn (VClosure(lam, r)), r', k)
    | (EAbs lam, r, k) -> (EReturn (VClosure(lam, r)), r, k)
    | (EInt n, r, k ) -> (EReturn (VInt n), r, k)
    | (EBinOp (op, e1, e2), r, k ) -> (e1, r, KBinApp1 (op, e2, r, k))
    | (EReturn v, r', k') ->
        let () = print_endline "returning value" in
        match v with
        | VClosure (lam, r) -> ((EInt 5), r', k')
        | VInt n' ->
            match k' with
            | KBinApp1 (op, e2, r, k) -> (e2, r, KBinApp2(op, VInt n', r, k))
            | KBinApp2 (op, VInt n, r, k) -> 
                let opFn op = 
                    match op with
                    | Add -> (+)
                    | Mul -> ( * )
                    | Sub -> (-)
                    | Div -> (/)
                in
                    (EReturn (VInt((opFn op) n n')), r, k)
            | KBinApp2(op, _, _, _) -> invalid_arg "Trying to apply binary operator to something that isnt a VInt"
            | KAppArg (e, r, k) -> (e, r, k)
            | KAppFun (lam, r, k)-> (EAbs lam,r,k)
            | _ -> let () = print_int n' in invalid_arg "trying to evaluate the empty kont"

let isFinal (s0 : state) : bool = 
    match s0 with
    |(EReturn(VInt n), env, Empty) -> let () = print_int n in true
    | _ -> false

let rec terminal (s0 : state) : state =
    if (isFinal s0) 
    then let () = print_state Format.std_formatter s0 in s0
    else let () = print_state Format.std_formatter s0 in terminal (step s0)


let inject (exp: expr) : state = let (r : env) = [] in (exp, r, Empty)


(*

let rec eval ( exp : expr) : int =  
    match exp with
    | EInt n -> n
    | EBin (op, e1, e2) -> 
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


