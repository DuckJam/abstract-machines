type op = Add | Mul | Sub | Div
type var = string
type lambda = (var * expr)
type expr = EVar of var 
          | EAbs of lambda
          | EApp of (expr * expr)
          | EInt of int 
          | EBinOp of (op * expr * expr)

type state = (expr * env * kont)
(* domain of values / dentorable values , 
closure is lambda term paired with environment that defines the values of its free variables*)
type d = closure
type closure = (lambda * env)
type env = var -> value
type kont = Mt 
          | Ar of (expr * env * kont)
          | Fn of (lambda * env * kont)

let step (before: state) : state = 
    match before with
    | (x , r, k) : evar * env * kont  -> 
        let unwrapClosure (lam , r') = r(x) 
        in (lam, r', k)
    | (EApp, env, kont) (EApp e1 e2, r, k) -> (e1, r, Ar (e2, r, k))
    | (EAbs, env, Ar) (lam, r, (e, r', k )) ->(e, r', Fn (lam, r, k))
    | (EAbs, env, Fn) (lam, r, (x :=> e, r', k)) -> (e, r' )

let rec terminal step (isFinal: bool) (s0 : state) : state
    | isFinal s0 = s0
    | _  = terminal step isFinal (step isFinal)

let isFinal (s0 : state) : bool = 
    match s0 with
    |(EAbs _, env, Mt) = True
    | _ = False

let inject (exp : expr) : state = 
    let r0 : env = fun x -> error ( "no binding for " ++ x )



let rec eval ( exp : expr) : int =  
    print_endline "evaluating expression" exp
    match exp with
    | EInt n -> n
    | EBin op e1 e2 -> 
        let opFn = 
            | Add = (+)
            | Mul = ( * )
            | Sub = (-)
            | Div = (/)
        in
            (opFn op) (eval e1) (eval e2)
    | EApp e1 e2 -> (eval e1) (eval e2)
    | EVar v1 