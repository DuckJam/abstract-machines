type Op = Add | Mul | Sub | Div
type Var = String
type Lambda = Var :=> Expr
type Expr = EVar Var | EAbs Var Expr | EApp Expr Expr
          | EInt Int | EBinOp Op Expr Expr
          | Ref Var
          | Lam Lambda


type State = (Expr, Env, Kont)
type D = Closure (Lambda, Env)
type Env = Var -> D
type Kont = Mt 
          | Ar (Expr, Env, Kont)
          | Fn (Lambda, Env, Kont)

let step (before: State) : State = 
    match before with
    | (Ref, Env, Kont) = 
    | (EApp, Env, Kont)
    | (Lam, Env, Ar)
    | (Lam, )
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
let terminal step (isFinal: bool) (s0 : State) : State
    | isFinal s0 = s0
    | _  = terminal step isFinal (step isFinal)

let isFinal (s0 : State) : bool = 
    match s0 with
    |(Lam _, Env, Mt) = True
    | _ = False

let inject (exp : Expr) : State = 
    let r0 : Env = fun x -> error ( "no binding for " ++ x )


let eval ( exp : Expr) : int =  
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
