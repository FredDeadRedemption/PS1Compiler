%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token ADD SUB MUL DIV
%token LPAREN RPAREN
%token SEMICOLON
%token PRINT
%token EOF

%left MINUS PLUS
%left MUL DIV 

%start program

%type <Ast.program> program


%%

program: 
  main = stmt* 
  EOF
  { { defs = [];
      main = Sblock main;} }
;

stmt:
| PRINT e = expr SEMICOLON
    {Sprint e}

expr:
| c = INT                        { Econst c }
| e1 = expr o = op e2 = expr     { Ebinop (o, e1, e2) }
;

%inline op:
| ADD  { Add }
| SUB { Sub }
| MUL { Mul }
| DIV   { Div }
















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