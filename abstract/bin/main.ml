
(*Cek.eval EInt 2 *)
let open Cek in 
let binop = EBinOp( Mul, EVar "x", EInt 9) in(
  let abs = EAbs( "x", binop) in (
    terminal (inject (EApp (abs, EInt 5)))
  )
)
  

(*Cek.terminal (Cek.inject (EBinOp (Add, EInt 2, EInt 3)))*)


(*Cek.terminal (Cek.inject (EBinOp (Add, EInt 2, EInt 3)))*)
