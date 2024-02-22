type token =
  | INT of int
  | FLOAT of float
  | ID of string
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  (* | LANGLE *)
  (* | RANGLE *)
  (* | COMMA *)
  | DOT
  | COLON
  | SEMICOLON
  | EQUAL
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | AND
  | OR
  | NEG
  (* COLONGEQ *)
  | LET
  (* NEW *)
  | CONST
  | FUN
  (* CLASS EXTENDS *)
  | TRUE
  | FALSE
  | IF 
  | ELSE 
  | FOR 
  | WHILE 
  | MAIN
  (* PRINT *)
  | STRING of string
  | EOF
