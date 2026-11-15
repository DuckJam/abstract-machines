
type state = (Expression.stack * Expression.env * (Expression.ctrl list) * Expression.dump)

let step (sigma: state) : state = 
  match sigma with
  | (v::[], e', [], (s,e,c)::d) -> (v::s, e, c, d)

  | (s, e, (CExpr (EInt n))::c, d) -> (VInt n :: s, e, c, d)
  | (s, e, (CExpr (EBool b))::c, d) -> (VBool b :: s, e, c, d)

  | (s, ValEnv e, (CExpr (EVar x))::c, d) -> 
    begin try
      let v = List.assoc x e in (v::s, ValEnv e, c, d)
    with Not_found -> invalid_arg "unbound variable" end

  | (s, e, (CExpr (EAbs lam))::c, d) -> (VClosure (lam, e)::s, e, c, d)

  | (s, e, (CExpr (EApp (eFun, eArg)))::c, d) -> 
    (s, e, (CExpr eArg):: (CExpr eFun):: CApp :: c, d)
  
  | (VClosure((var, exp), e'):: v':: s, ValEnv e, CApp::c, d) ->
    ([], ValEnv ((var, v')::e), (CExpr exp)::[], (s, ValEnv e, c):: d)

  | (s, e, (CExpr (EBinOp (op, e1, e2)))::c, d) -> 
    (s, e, (CExpr e2):: (CExpr e1):: (COp op):: c, d)

  | (v2 :: v1:: s, e, (COp op):: c, d) -> ((Expression.evalBinOp op v1 v2)::s, e, c, d)

  | (s, e, (CExpr (EIf (eIf, eThen, eElse)))::c, d) -> 
    (s, e, (CExpr eIf)::CIf ::  (CExpr eThen):: (CExpr eElse) ::c, d)

  | (VBool b::s, e, CIf:: eThen:: eElse::c, d) ->
    if b 
      then (s, e, eThen::c, d)
      else (s, e, eElse::c, d)
  
  | _ -> failwith "invalid state for SECD machine"


let isFinal (s : state) : bool = 
    match s with
    |(v::[], _, [], []) -> true
    | _ -> false

let rec terminal (sigma : state) : state =
  let (s, _, c, _) = sigma in 
    let () = Expression.print_ctrl Format.std_formatter c
    and () = Expression.print_stack Format.std_formatter s
  in
    if (isFinal sigma) 
    then sigma
    else terminal (step sigma)


let inject (exp: Expression.expr) : state = 
  let (e : Expression.env) = ValEnv [] 
  and c = Expression.CExpr exp
  in ([], e, c::[], [])
