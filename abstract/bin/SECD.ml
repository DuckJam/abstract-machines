
type state = (Expression.stack * Expression.env * (Expression.ctrl list) * Expression.dump)

let step (sigma: state) : state = 
  match sigma with
  | (v::[], e', [], (s,e,c)::d) -> (v::s, e, c, d)

  | (s, e, (CExpr (EInt n))::c, d) -> (VInt n :: s, e, c, d)

  | (s, ValEnv e, (CExpr (EVar x))::c, d) -> 
    begin try
      let v = List.assoc x e in (v::s, ValEnv e, c, d)
    with Not_found -> invalid_arg "unbound variable" end

  | (s, e, (CExpr (EAbs lam))::c, d) -> (VClosure (lam, e)::s, e, c, d)

  | (s, e, (CExpr (EApp (e1, e2)))::c, d) -> 
    (s, e, (CExpr e2):: (CExpr e1):: CApp :: c, d)

  | (s, e, (CExpr (EBinOp (op, e1, e2)))::c, d) -> 
    (s, e, (CExpr e2):: (CExpr e1):: (COp op):: c, d)

  | (v2 :: v1:: s, e, (COp op):: c, d) -> ((Expression.evalBinOp op v1 v2)::s, e, c, d)

  | (VClosure((var, exp), e'):: v':: s, ValEnv e, CApp::c, d) ->
    ([], ValEnv ((var, v')::e), (CExpr exp)::[], (s, ValEnv e, c):: d)
  
  | _ -> failwith "invalid state for SECD machine"


let isFinal (s : state) : bool = 
    match s with
    |(v::[], _, [], []) -> true
    | _ -> false

let rec terminal (sigma : state) : state =
  let (s, _, c, _) = sigma in 
    let () = 
        match c with 
        | (CExpr exp)::_->  Expression.print_expr Format.std_formatter exp
        | _ -> ()
    and () = Expression.print_stack Format.std_formatter s
  in
    if (isFinal sigma) 
    then sigma
    else terminal (step sigma)


let inject (exp: Expression.expr) : state = 
  let (e : Expression.env) = ValEnv [] 
  and c = Expression.CExpr exp
  in ([], e, c::[], [])
