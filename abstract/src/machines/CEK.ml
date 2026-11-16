open Expression
type state = (expr * env * kont)

let step (s: state) : state = 
  match s with
  | (_, AddrEnv _, _) -> invalid_arg "invalid environment for CEK machine"
  | (EVar x , ValEnv r, k) -> 
    begin try
    let v = List.assoc x r in (EReturn v, ValEnv r, k)
    with Not_found -> invalid_arg "unbound variable" end
  | (EApp (e1, e2), r , k ) -> (e1, r, KAppArg (e2, r, k))
  | (EAbs lam, r, KAppArg (e, r', k )) -> (e, r', KAppFun (lam, r, k))
  | (EAbs lam, ValEnv r, KAppFun ((var, e), ValEnv r', k )) -> 
    (e, ValEnv ((var,  (Expression.VClosure(lam, ValEnv r))) :: r') , k)
  | (EAbs lam, r, k) -> (EReturn(VClosure(lam, r)), r, k) 
  | (EInt n, r, k ) -> (EReturn (VInt n), r, k)
  | (EBool b, r, k) -> (EReturn (VBool b), r, k)
  | (EBinOp  (op, e1, e2), r, k ) -> (e1, r, KBinApp1 (op, e2, r, k))
  | (EIf (eIf, eThen, eElse), r, k) -> (eIf, r, KIf(eThen, eElse, r, k))
  | (EReturn v, r', k') ->
    match v with
    | VClosure ((var, e), r) -> (e, r, k')
    | _ -> match k' with
      | KBinApp1 (op, e2, r, k) -> (e2, r, KBinApp2(op, v, r, k))
      | KBinApp2 (op, v1, r, k) -> (EReturn(Expression.evalBinOp op v1 v), r, k)
      | KAppArg (e, r, k) -> (e, r, k)
      | KAppFun ((var, e), ValEnv r, k)-> (e, ValEnv ((var, v)::r) ,k)
      | KIf(eThen, eElse, r, k) -> (
        match v with VBool b -> 
          if b
          then (eThen, r, k)
          else (eElse, r, k)
        | _ -> invalid_arg "if statement evaluated to something that isnt a bool" 
        )
      | _  -> invalid_arg "trying to evaluate empty kont"


let is_final (s : state) : bool = 
    match s with
    |(EReturn _, _, Empty) -> true
    | _ -> false

let string_state (sigma : state) : string = 
  let (e, r, k) = sigma in 
  "C: "^(str_expr e)^"\nEnv: ["^(str_env r)^"\nKont: ["^(str_kont k)

let print_state (sigma : state) = 
  let (e, r, k) = sigma in
    let () = Expression.print_expr Format.std_formatter e
    and () = Expression.print_env Format.std_formatter r
    and () = Expression.print_kont Format.std_formatter k in ()

let rec terminal (sigma : state) : state =
  let () = print_state sigma
  in 
    if (is_final sigma) 
    then sigma
    else terminal (step sigma)


let inject (exp: Expression.expr) : state = 
  let (r : Expression.env) = ValEnv [] in (exp, r, Empty)
