
(*Cek.eval EInt 2 *)

let binop = Cek.EBinOp( Add, EVar "x", EInt 3) in
  let abs = Cek.EAbs( "x", binop) in
    Cek.terminal (Cek.inject (EApp (abs, EInt 2)))
(*Cek.terminal (Cek.inject (EBinOp (Add, EInt 2, EInt 3)))*)
let () = print_endline "Hello, World!"
