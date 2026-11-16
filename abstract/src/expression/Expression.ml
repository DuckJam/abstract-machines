include AST
include Register
include Expr_to_string
module Lexer = Lexer
module Parser = Parser

let parse (s: string) : expr =
  Parser.main Lexer.token (Lexing.from_string s)

let evalBinOp op v1 v2 : value =  
  match v1, v2 with 
  | VInt n1, VInt n2 -> (
    match op with 
    | Eq -> VBool(n1 == n2)
    | Lt -> VBool(n1 < n2)
    | Gt -> VBool(n1 > n2)
    | LEq -> VBool(n1 <= n2)
    | GEq -> VBool(n1 >= n2)
    | _ -> 
    let opFn op = 
      match op with
        | Add -> (+)
        | Mul -> ( * )
        | Sub -> (-)
        | Div -> (/)
        | _ -> invalid_arg "applying incorrect operator to two ints"
    in VInt((opFn op) n1 n2)
  )    
  | VBool b1, VBool b2 -> (
    match op with
      | Eq -> VBool(b1 == b2)
      | _ -> invalid_arg "trying to do anything other than eq with two bools"
  )
  | _ , _-> invalid_arg "mismatch of types in binop"
;;

let expr_If = 
  let expr = EApp (EAbs("x", EBinOp(Mul, EVar "x", EInt 9)), (EBinOp (Add, (EInt 5), (EInt 6))))
  and expr_cond = EBinOp(Gt, EInt 3, EInt 5) in
  EIf(expr_cond, expr_cond, expr)
