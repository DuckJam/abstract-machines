type state = (Expression.expr * Expression.env * Expression.kont)

let step (s: state) : state = 
  match s with
  | (EVar x , r, k) -> 
    begin try
    let v = List.assoc x r in (EReturn v, r, k)
    with Not_found -> invalid_arg "unbound variable" end
  | (EApp (e1, e2), r , k ) -> (e1, r, KAppArg (e2, r, k))
  | (EAbs lam, r, KAppArg (e, r', k )) -> (e, r', KAppFun (lam, r, k))
  | (EAbs lam, r, KAppFun ((var, e), r', k )) -> (e, [(var,  Expression.VClosure(lam, r))] @ r' , k)
  | (EAbs lam, r, k) -> (EReturn(VClosure(lam, r)), r, k) 
  | (EInt n, r, k ) -> (EReturn (VInt n), r, k)
  | (EBool b, r, k) -> (EReturn (VBool b), r, k)
  | (EBinOp (op, e1, e2), r, k ) -> (e1, r, KBinApp1 (op, e2, r, k))
  | (EIf (eIf, eThen, eElse), r, k) -> (eIf, r, KIf(eThen, eElse, r, k))
  | (EReturn v, r', k') ->
    match v with
    | VClosure ((var, e), r) -> (e, r, k')
    | _ -> match k' with
      | KBinApp1 (op, e2, r, k) -> (e2, r, KBinApp2(op, v, r, k))
      | KBinApp2 (op, v2, r, k) -> (EReturn(Expression.eval v k' ), r, k)
      | KAppArg (e, r, k) -> (e, r, k)
      | KAppFun ((var, e), r, k)-> (e,[(var, v)]@r,k)
      | KIf(eThen, eElse, r, k) -> (
        match v with VBool b -> 
          if b
          then (eThen, r, k)
          else (eElse, r, k)
        | _ -> invalid_arg "if statement evaluated to something that isnt a bool" 
        )
      | _  -> invalid_arg "trying to evaluate empty kont"

let isFinal (s0 : state) : bool = 
    match s0 with
    |(EReturn(VInt n), env, Empty) -> true
    | _ -> false

let rec terminal (s : state) : state =
    let () = let (e, r, k) = s in Expression.print_expr Format.std_formatter e in
    if (isFinal s) 
    then s
    else terminal (step s)


let inject (exp: Expression.expr) : state = let (r : Expression.env) = [] in (exp, r, Empty)
