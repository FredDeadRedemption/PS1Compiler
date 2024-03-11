type token =
  | LPAREN 
  | RPAREN  
  | LBRACE 
  | RBRACE 
  | LBRACK 
  | RBRACK 
  | DOT 
  | COMMA
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
  | AND 
  | OR 
  | NOT 
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
  | EOF
  | INT of int
  | FLOAT of float
  | STRING of string
  | ID of string

