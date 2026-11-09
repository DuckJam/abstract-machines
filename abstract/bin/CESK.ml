
type state = (Expression.expr * Expression.env * Expression.store * Expression.kont)

let next_addr (p : Expression.store) : Expression.addr = 
  if List.is_empty p then 0 else List.length p + 1

let step (sigma : state) : state = 
  match sigma with
  | (EVar x , AddrEnv r, s, k) -> 
    begin try
    let a = List.assoc x r in
    let v = List.assoc a s in 
    match v with 
    | SValue value -> (EReturn value, AddrEnv r, s, k)
    | _ -> invalid_arg "Found storable other than value"
    with Not_found -> invalid_arg "unbound variable" end
  | (EApp (e1, e2), r , s, k ) -> (e1, r, s, KAppArg(e2, r, k))

  | (EAbs lam, r, s, KAppArg (e, r', k )) -> (e, r', s, KAppFun (lam, r, k))

  | (EAbs lam, r, s, KAppFun ((var, e), AddrEnv r', k )) -> 
    let a = next_addr s in
    let s' = (a, (Expression.SValue (VClosure(lam, r)))) :: s in 
    (e, AddrEnv ((var, a) :: r'), s' , k)

  | (EAbs lam, r, s, k) -> (EReturn(VClosure(lam, r)), r, s, k) 

  | (EInt n, r, s, k) -> (EReturn (VInt n), r, s, k)

  | (EBool b, r, s, k) -> (EReturn (VBool b), r, s, k)

  | (EBinOp  (op, e1, e2), r, s, k) -> (e1, r, s, KBinApp1 (op, e2, r, k))

  | (EIf (eIf, eThen, eElse), r, s, k) -> (eIf, r, s, KIf(eThen, eElse, r, k))

  | (EReturn v, r', s, k') -> (
    match v with
    | VClosure ((var, e), r) -> (e, r, s, k')
    | _ -> 
      match k' with 
      | KBinApp1 (op, e2, r, k) -> (e2, r, s, KBinApp2(op, v, r, k))
      | KBinApp2 (op, v2, r, k) -> (EReturn(Expression.evalBinOp op v v2), r, s, k)
      | KAppArg (e, r, k) -> (e, r, s, k)
      | KAppFun ((var, e), AddrEnv r, k)-> 
        let a = next_addr s in
        let s' = (a, Expression.SValue v)::s in
        (e, AddrEnv ((var, a)::r), s', k)
      | KIf(eThen, eElse, r, k) -> (
        match v with VBool b -> 
          if b
          then (eThen, r, s, k)
          else (eElse, r, s, k)
        | _ -> invalid_arg "if statement evaluated to something that isnt a bool" 
        )
      | _  -> invalid_arg "trying to evaluate empty kont"
      )
  | (_, _, _, _) -> invalid_arg "invalid state for CESK machine"

let isFinal (s : state) : bool = 
    match s with
    |(EReturn _, _, _, Empty) -> true
    | _ -> false

let rec terminal (sigma : state) : state =
    let () = let (e, _, _, _) = sigma in Expression.print_expr Format.std_formatter e in
    if (isFinal sigma) 
    then sigma
    else terminal (step sigma)


let inject (exp: Expression.expr) : state = 
  let (r : Expression.env) = AddrEnv[] in (exp, r, [], Empty)
