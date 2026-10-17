
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
  and expr_cond = EBinOp(Eq, EInt 3, EInt 4)
  in
  EIf(expr, expr_cond,EInt 1)
in Cek.terminal (Cek.inject expr_If)
  

(*Cek.terminal (Cek.inject (EBinOp (Add, EInt 2, EInt 3)))*)
let () = print_endline "\n"

(*Cek.terminal (Cek.inject (EBinOp (Add, EInt 2, EInt 3)))*)
