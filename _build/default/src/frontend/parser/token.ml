type token =
  | LPAREN 
  | RPAREN  
  | LBRACE 
  | RBRACE 
  | LBRACK 
  | RBRACK 
  | DOT 
  | COLON 
  | SEMICOLON 
  | EQ 
  | ADD 
  | SUB 
  | MUL  
  | DIV 
  | MOD 
  | LANGLE 
  | RANGLE 
  |  AND 
  |  OR 
  | NEG 
  | LET 
  | CONST 
  | FUN 
  | TYPE_INT 
  | TYPE_FLOAT 
  | TYPE_BOOL  
  | TYPE_VOID 
  | TYPE_NULL 
  | TRUE 
  | FALSE 
  | WHILE 
  | IF 
  | ELSE 
  | FOR 
  | MAIN 
  | PRINT
  | RETURN
  | INT of int
  | FLOAT of float

