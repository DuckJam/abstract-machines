open Expression

type state = (Expression.expr * Expression.env * Expression.store * Expression.kontstack)

let name = "LK"

let step (sigma: state): state = 
  match sigma with
  | (EVar x, AddrEnv r, s, k) ->    
    begin try
    let a = List.assoc x r in
    let v = List.assoc a s in
      match v with
      | SThunk t -> (
        match t with
          | TComputed (v, r')-> (EReturn v, r', s, k)
          | TDelayed (e, r') -> (e, r', s, KFun a :: k)
        )
      | _ -> invalid_arg "found storable other than thunk"
    with Not_found -> invalid_arg "unbound variable" end

  | (EInt n, r, s, k) -> (EReturn (VInt n), r, s, k)
  | (EBool b, r, s, k) -> (EReturn (VBool b), r, s, k)

  | (EApp (eFun, eArg), r, s, k) -> 
    let a = next_addr s in
    let s' = (a, Expression.SThunk (Expression.TDelayed(eArg, r)))::s in
    (eFun, r, s', Expression.KArg a :: k)

  | (EAbs (v, e), AddrEnv r, s, KArg a :: k) -> 
    let r' = (v, a) :: r in
    (e, AddrEnv r', s, k)

  | (EBinOp(op, e1, e2), r, s, k) -> (e1, r, s, Expression.KOpLeft(op, e2, r) :: k)

  | (EIf(eIf, eThen, eElse), r, s, k) -> (eIf, r, s, KIf(eThen, eElse, r)::k)

  | (EReturn v, r, s, KFun addr :: k) -> 
    begin try
      let s' = (addr, Expression.SThunk (TComputed(v, r))) :: (List.remove_assoc addr s) in (EReturn v, r, s', k)
    with Not_found -> invalid_arg "address not found" end

  | (EReturn v, r, s, k :: ktail) -> (
    match v with
    | VClosure ((var, e), r') -> (e, r', s, k::ktail)
    | _ -> (
      match k with
      | KOpLeft (op, e2, env) -> (e2, env, s, Expression.KOpRight(op, v) :: ktail)
      | KOpRight (op, v1) -> (EReturn(Expression.evalBinOp op v1 v), r, s, ktail)
      | KIf (eThen, eElse, r') -> (
        match v with
        | VBool b -> if b then (eThen, r', s, ktail) else (eElse, r', s, ktail)
        | _ -> invalid_arg "if statement evaluated to something that isnt a bool" 
      )
      | _ -> invalid_arg "trying to evaluate empty kont"
    )
  )
  | (_, ValEnv _, _, _) -> invalid_arg "incorrect Env for krivine"
  | _ -> invalid_arg "expression not implemented for krivine"


let is_final (s : state) : bool = 
  match s with
  |(EReturn _, _, _, []) -> true
  | _ -> false

let rec terminal (sigma : state) : state =
  let (e, r, s, k) = sigma in
    let () = Format.fprintf Format.std_formatter "%s" (print_expr e)
    and () = Format.fprintf Format.std_formatter "%s" (print_env r)
    and () = Format.fprintf Format.std_formatter "%s" (print_store s)
    and () = Format.fprintf Format.std_formatter "%s" (print_kontstack k)
  in
    if (is_final sigma) 
    then sigma
    else terminal (step sigma)


let inject (exp: Expression.expr) : state = 
  let (r : Expression.env) = AddrEnv [] 
  and (p : Expression.store) = []
  and (k : Expression.kontstack) = []
  in (exp, r, p, k)

let string_state (sigma : state) : string = 
  let (e, r, s, k) = sigma in 
  "C: "^(str_expr e)^"\n"^(print_env r)^"\n"^(print_store s)^"\n"^(print_kontstack k)

let view (sigma: state) : (string * string list) list  = 
  let (e, r, s, k) = sigma in 
    [("EXPR", [str_expr e]); ("ADDRENV", str_env r); ("STORE", str_store s);("KONT", str_kontstack k)]