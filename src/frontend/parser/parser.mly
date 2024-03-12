%{
  open Ast
%}

%token LPAREN 
%token RPAREN  
%token LBRACE 
%token RBRACE 
%token LBRACK 
%token RBRACK 
%token DOT 
%token COMMA
%token COLON 
%token SEMICOLON 
%token EQ 
%token ADD 
%token SUB 
%token MUL  
%token DIV 
%token MOD 
%token LANGLE 
%token RANGLE 
%token AND 
%token OR 
%token NOT 
%token LET 
%token CONST 
%token FUN 
%token TYPE_INT 
%token TYPE_FLOAT 
%token TYPE_BOOL  
%token TYPE_VOID 
%token TYPE_NULL 
%token TRUE 
%token FALSE 
%token WHILE 
%token IF 
%token ELSE 
%token FOR 
%token MAIN 
%token PRINT
%token RETURN
%token EOF
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID

%left ADD SUB LESSER GREATER
%left MUL DIV MOD
%left AND OR
%nonassoc NOT

%start program

%type <Ast.program> program


%%

program:
  defs = def*
  main = stmt*
  EOF
    { { defs = defs;
        main = Sblock main; } }
;

def:
| FUN f = ID
  LPAREN formals = separated_list(COMMA, ID) RPAREN body = stmt
    { { name = f;
        formals = formals;
        body = body } }
;

stmt:
| IF e = expr s = stmt
  { Sif (e, s, Sblock []) }
;

expr:
| c = INT                     { Econst c }
| id = ID                     { Evar id }
| LPAREN e = expr RPAREN      { e }
;
















(*
%token LPAREN 
%token RPAREN  
%token LBRACE 
%token RBRACE 
%token LBRACK 
%token RBRACK 
%token DOT 
%token COMMA
%token COLON 
%token SEMICOLON 
%token EQ 
%token ADD 
%token SUB 
%token MUL  
%token DIV 
%token MOD 
%token LANGLE 
%token RANGLE 
%token AND 
%token OR 
%token NOT 
%token LET 
%token CONST 
%token FUN 
%token TYPE_INT 
%token TYPE_FLOAT 
%token TYPE_BOOL  
%token TYPE_VOID 
%token TYPE_NULL 
%token TRUE 
%token FALSE 
%token WHILE 
%token IF 
%token ELSE 
%token FOR 
%token MAIN 
%token PRINT
%token RETURN
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID

*)