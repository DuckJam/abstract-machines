%{
  open AST
%}

%token <int> INT 
%token <string> IDENT
%token FUN ARROW PLUS MINUS STAR DIV LT GT GEQ LEQ EQ LPAREN RPAREN IF THEN ELSE TRUE FALSE AT EOF

%nonassoc LT GT GEQ LEQ
%left PLUS MINUS STAR DIV

%type <expr> expr binop atom

%start <expr> main
%%

main:
  | e = expr EOF { e }

expr:
  | FUN x = IDENT ARROW body = expr { EAbs (x, body)}
  | e = binop { e }
  | IF cond = expr THEN eThen = expr ELSE eElse = expr { EIf(cond, eThen, eElse) }
  | f = expr AT a = atom { EApp (f, a)}
  | a = atom { a }

binop: 
  | l = expr LT r = expr { EBinOp (Lt, l, r) }
  | l = expr GT r = expr { EBinOp (Gt, l, r) }
  | l = expr LEQ r = expr { EBinOp (LEq, l, r) }
  | l = expr GEQ r = expr { EBinOp (GEq, l, r) }
  | l = expr EQ r = expr { EBinOp (Eq, l, r) }
  | l = expr PLUS r = expr { EBinOp (Add, l, r) }
  | l = expr MINUS r = expr { EBinOp (Sub, l, r) }
  | l = expr STAR r = expr { EBinOp (Mul, l, r) }
  | l = expr DIV r = expr { EBinOp (Div, l, r) }


atom:
  | n = INT { EInt n }
  | x = IDENT { EVar x }
  | TRUE { EBool true }
  | FALSE { EBool false }
  | LPAREN e = expr RPAREN { e }
