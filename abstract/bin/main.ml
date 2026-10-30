
(*Cek.eval EInt 2 *)
let open Expression in
  let expr = EApp (
      EAbs("x",
        EBinOp(Mul, EVar "x", EInt 9)
      ), 
      (EInt 5)
    ) in Cek.terminal (Cek.inject expr);;

let open Expression in
  let expr_If = 
    let expr = EApp (EAbs("x", EBinOp(Mul, EVar "x", EInt 9)), (EInt 5))
    and expr_cond = EBinOp(Eq, EInt 3, EInt 3)
    in
    EIf(expr_cond, expr_cond,expr)
  in Cek.terminal (Cek.inject expr_If);;
  
(*let open Expression in
  let expr = EApp (
      EAbs("x",
        EBinOp(Mul, EVar "x", EInt 9)
      ), 
      (EInt 5)
    ) in Cesk.terminal (Cesk.inject expr);;
*)
(*let open Expression in
  let expr = EApp (EAbs("x", EBinOp(Mul, EVar "x", EInt 9)), (EInt 5))
  and expr_cond = EBinOp(Eq, EInt 3, EInt 3)
  in EIf(expr_cond, expr_cond,expr)
  in Krivine.terminal (Krivine.inject expr);; *)
(*Cek.terminal (Cek.inject (EBinOp (Add, EInt 2, EInt 3)))*)
let () = print_endline "\n"

(*Cek.terminal (Cek.inject (EBinOp (Add, EInt 2, EInt 3)))*)
