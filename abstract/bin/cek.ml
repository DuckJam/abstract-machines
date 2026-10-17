with open Expression in
type state = (expr * env * kont)



let step (s: state) : state = 
    let () = print_state Format.std_formatter s in
    match s with
    | (EVar x , r, k) -> 
        begin try
        let v = List.assoc x r in (EReturn v, r, k)
        with Not_found -> invalid_arg "unbound variable" end
    | (EApp (e1, e2), r , k ) -> (e1, r, KAppArg (e2, r, k))
    | (EAbs lam, r, KAppArg (e, r', k )) -> (e, r', KAppFun (lam, r, k))
    | (EAbs lam, r, KAppFun ((var, e), r', k )) -> (e, [(var,  VClosure(lam, r))] @ r' , k)
    | (EAbs lam, r, k) -> (EReturn(VClosure(lam, r)), r, k) 
    | (EInt n, r, k ) -> (EReturn (VInt n), r, k)
    | (EBinOp (op, e1, e2), r, k ) -> (e1, r, KBinApp1 (op, e2, r, k))
    | (EReturn v, r', k') ->
        let () = print_endline "returning value" in
        match v with
        | VClosure ((var, e), r) -> (e, r, k')
        | VInt n' ->
            match k' with
            | KBinApp1 (op, e2, r, k) -> (e2, r, KBinApp2(op, VInt n', r, k))
            | KBinApp2 (op, VInt n, r, k) -> 
                let opFn op = 
                    match op with
                    | Add -> (+)
                    | Mul -> ( * )
                    | Sub -> (-)
                    | Div -> (/)
                in
                    (EReturn (VInt((opFn op) n n')), r, k)
            | KBinApp2(op, _, _, _) -> invalid_arg "Trying to apply binary operator to something that isnt a VInt"
            | KAppArg (e, r, k) -> (e, r, k)
            | KAppFun ((var, e), r, k)-> (e,[(var, v)]@r,k)
            | _ -> let () = print_int n' in invalid_arg "trying to evaluate the empty kont"
        

let isFinal (s0 : state) : bool = 
    match s0 with
    |(EReturn(VInt n), env, Empty) -> let () = print_int n in true
    | _ -> false

let rec terminal (s0 : state) : state =
    if (isFinal s0) 
    then s0
    else terminal (step s0)


let inject (exp: expr) : state = let (r : env) = [] in (exp, r, Empty)




