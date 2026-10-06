type op = Add | Mul | Sub | Div
type var = string
type lambda = (Var * Expr)
type expr = EVar of var 
          | EAbs of lambda
          | EApp of (expr * expr)
          | EInt of int 
          | EBinOp of (op * expr * expr)


type state = (expr * env * kont)
(* domain of values / dentorable values , 
closure is lambda term paired with environment that defines the values of its free variables*)
type data = Closure (lambda * env)
type env = Var -> D
type kont = Mt 
          | Ar of (expr * env * kont)
          | Fn of (lambda * env * kont)

let step (before: state) : state = 
    match before with
    | (evar * env * kont) (x , r, k) -> 
        let unwrapClosure (lam , r') = r(x) 
        in (lam, r', k)
    | (EApp, env, kont) (EApp e1 e2, r, k) -> (e1, r, Ar (e2, r, k))
    | (Lam, Env, Ar) (lam, r, (e, r', k )) ->(e, r', Fn (lam, r, k))
    | (Lam, Env, Fn) (lam, r, (x :=> e, r', k)) -> (e, r' )
(*
step (Ref x, ρ, κ)
   = (Lam lam,ρ',κ) where Clo (lam, ρ') = ρ(x)

step (f :@ e, ρ, κ)
   = (f, ρ,  Ar(e, ρ, κ))

step (Lam lam, ρ, Ar(e, ρ', κ))
   = (e, ρ', Fn(lam, ρ, κ))

step (Lam lam, ρ, Fn(x :=> e, ρ', κ))
   = (e, ρ' // [x ==> Clo (lam, ρ)], κ)
*)

(*
 A few auxiliary definitions handle function extension in this code:

(==>) :: a -> b -> (a,b)
(==>) x y = (x,y)


(//) :: Eq a => (a -> b) -> [(a,b)] -> (a -> b)
(//) f [(x,y)] = \ x' ->
                 if (x == x')
                 then y
                 else f(x')

*)
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

let () = print_endline "Hello, World!"
