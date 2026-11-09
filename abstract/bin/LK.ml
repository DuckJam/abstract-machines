type state = (Expression.expr * Expression.env * Expression.store * Expression.kontstack)

let next_addr (p : Expression.store) : Expression.addr = 
  if List.is_empty p then 0 else List.length p + 1

let step (s: state): state = 
  match s with
  | (EVar x, AddrEnv r, p, k) ->    
    begin try
    let a = List.assoc x r in
    let v = List.assoc a p in
      match v with
      | SThunk t -> (
        match t with
          | TComputed (v, r')-> (EReturn v, r', p, k)
          | TDelayed (e, r') -> (e, r', p, k)
        )
      | _ -> invalid_arg "found storable other than thunk"
    with Not_found -> invalid_arg "unbound variable" end

  | (EInt n, r, p, k) -> (EReturn (VInt n), r, p, k)
  | (EBool b, r, p, k) -> (EReturn (VBool b), r, p, k)

  | (EApp (e1, e2), r, p, k) -> 
    let a = next_addr p in
    let p' = (a, Expression.SThunk (Expression.TDelayed(e2, r)))::p in
    (e1, r, p', Expression.KArg a :: k)

  | (EAbs (v, e), AddrEnv r, p, KArg a :: k) -> 
    let r' = (v, a) :: r in
    (e, AddrEnv r', p, k)

  | (EBinOp(op, e1, e2), r, p, k) -> (e1, r, p, Expression.KOpLeft(op, e2, r) :: k)

  | (EIf(eIf, eThen, eElse), r, p, k) -> (eIf, r, p, KIf(eThen, eElse)::k)

  | (EReturn v, r, p, k :: ktail) -> (

    match v with
    | VClosure ((var, e), r') -> (e, r', p, k::ktail)
    | _ -> (
      match k with
      | KOpLeft (op, e2, env) -> (e2, env, p, Expression.KOpRight(op, v) :: ktail)
      | KOpRight (op, v1) -> (EReturn(Expression.evalBinOp op v1 v), r, p, ktail)
      | KIf (eThen, eElse) -> (
        match v with
        | VBool b -> if b then (eThen, r, p, ktail) else (eElse, r, p, ktail)
        | _ -> invalid_arg "if statement evaluated to something that isnt a bool" 
      )
      | _ -> invalid_arg "trying to evaluate empty kont"
    )
  )
  | (_, ValEnv _, _, _) -> invalid_arg "incorrect Env for krivine"
  | _ -> invalid_arg "expression not implemented for krivine"


let isFinal (s : state) : bool = 
  match s with
  |(EReturn _, _, _, []) -> true
  | _ -> false

let rec terminal (s : state) : state =
  let () = let (e, _, _, _) = s in Expression.print_expr Format.std_formatter e in
  if (isFinal s) 
  then s
  else terminal (step s)


let inject (exp: Expression.expr) : state = 
  let (r : Expression.env) = AddrEnv [] 
  and (p : Expression.store) = []
  and (k : Expression.kontstack) = []
  in (exp, r, p, k)