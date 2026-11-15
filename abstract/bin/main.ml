open Expression
let () = Format.fprintf Format.std_formatter "\nCEK \n" ;;

let expr = EApp (
    EAbs("x",
      EBinOp(Mul, EVar "x", EInt 9)
    ), 
    (EInt 5)
  ) in Machines.CEK.terminal (Machines.CEK.inject expr);;

(*
let open Expression in
  let expr_If = 
    let expr = EApp (EAbs("x", EBinOp(Mul, EVar "x", EInt 9)), (EInt 5))
    and expr_cond = EBinOp(LEq, EInt 3, EInt 5)
    in
    EIf(expr_cond, expr_cond,expr)
  in CEK.terminal (CEK.inject expr_If);;
*)

let () = Format.fprintf Format.std_formatter "\nCESK \n" ;;

let expr = EApp (
    EAbs("x",
      EBinOp(Mul, EVar "x", EInt 9)
    ), 
    (EInt 5)
  ) in Machines.CESK.terminal (Machines.CESK.inject expr);;


let () = Format.fprintf Format.std_formatter "\nLK \n" ;;

let expr = EApp (
    EAbs("x",
      EBinOp(Mul, EVar "x", EInt 9)
    ), 
    (EInt 5)
  ) in Machines.LK.terminal (Machines.LK.inject expr);;
(*
let open Expression in
  let expr_If = 
    let expr = EApp (EAbs("x", EBinOp(Mul, EVar "x", EInt 9)), (EInt 5))
    and expr_cond = EBinOp(Eq, EInt 3, EInt 3)
    in
    EIf(expr_cond, expr_cond,expr)
  in LK.terminal (LK.inject expr_If);;
*)

let () = Format.fprintf Format.std_formatter "\nSECD\n" ;;

let expr = EApp (
    EAbs("x",
      EBinOp(Mul, EVar "x", EInt 9)
    ), 
    (EInt 5)
  ) in Machines.SECD.terminal (Machines.SECD.inject expr);;
(*
let open Expression in
  let expr_If = 
    let expr = EApp (EAbs("x", EBinOp(Mul, EVar "x", EInt 9)), (EInt 5))
    and expr_cond = EBinOp(Eq, EInt 3, EInt 3)
    in
    EIf(expr_cond, expr_cond,expr)
  in SECD.terminal (SECD.inject expr_If);;
*)