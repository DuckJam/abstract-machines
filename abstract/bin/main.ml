
let () = Format.fprintf Format.std_formatter "\nCEK \n" ;;

let open Expression in
  let expr = EApp (
      EAbs("x",
        EBinOp(Mul, EVar "x", EInt 9)
      ), 
      (EInt 5)
    ) in CEK.terminal (CEK.inject expr);;

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

let open Expression in
  let expr = EApp (
      EAbs("x",
        EBinOp(Mul, EVar "x", EInt 9)
      ), 
      (EInt 5)
    ) in CESK.terminal (CESK.inject expr);;


let () = Format.fprintf Format.std_formatter "\nLK \n" ;;
let open Expression in
  let expr = EApp (
      EAbs("x",
        EBinOp(Mul, EVar "x", EInt 9)
      ), 
      (EInt 5)
    ) in LK.terminal (LK.inject expr);;
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
let open Expression in
  let expr = EApp (
      EAbs("x",
        EBinOp(Mul, EVar "x", EInt 9)
      ), 
      (EInt 5)
    ) in SECD.terminal (SECD.inject expr);;
(*
let open Expression in
  let expr_If = 
    let expr = EApp (EAbs("x", EBinOp(Mul, EVar "x", EInt 9)), (EInt 5))
    and expr_cond = EBinOp(Eq, EInt 3, EInt 3)
    in
    EIf(expr_cond, expr_cond,expr)
  in SECD.terminal (SECD.inject expr_If);;
*)