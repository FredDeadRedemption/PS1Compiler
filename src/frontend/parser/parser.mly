%{
  open Ast
%}

%token TYPE_INT
%token TYPE_FLOAT
%token VAR
%token <int> INT
%token <string> ID
%token ADD SUB MUL DIV 
%token MOD EQ EXCL
%token AND OR
%token TRUE FALSE
%token LANGLE RANGLE
%token LPAREN RPAREN
%token LSQBRACK RSQBRACK
%token LCURBRACK RCURBRACK
%token COMMA SEMICOLON
%token PRINT START
%token EOF

%right EQ 
%left ADD SUB LANGLE RANGLE
%left MUL DIV MOD
%left AND OR  
%nonassoc EXCL

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
| block = block         { Sblock block } // skal slettes
| vardef = vardef       { Svardef vardef }
| fundef = fundef       { Sfundef fundef }
| expr = expr SEMICOLON { Sexpr expr }
| PRINT LPAREN e = expr RPAREN SEMICOLON {Sprint e}
| START LPAREN RPAREN block = block { Sstart block }
;
/*
| fundef = fundef       { Sfundef fundef }
;

fundef:
| rtype = typespec name = ID formals = params body = block

params:
| LPAREN separated_list(COMMA, expr) RPAREN;
*/

vardef:
| typespec = typespec name = ID EQ value = expr SEMICOLON
    {{ typespec = typespec; name = name; value = value; }}

fundef:
| typespec = typespec name = ID LPAREN args = separated_list(COMMA, typedarg) RPAREN body = block
    {{ typespec = typespec; name = name; args = args; body = body }}

typedarg:
| typespec = typespec name = ID {{ typespec = typespec; name = name }}

typespec:
| TYPE_INT   { "int" }
| TYPE_FLOAT { "float" }
;

// Expressions

expr:
| LPAREN e = expr RPAREN         { e }
| c = INT                        { Econst c }
| id = ID                        { Evar id }
| TRUE                           { Ebool (true) }
| FALSE                          { Ebool (false) }
| o = unop e = expr              { Eunop (o, e) }
| e1 = expr o = binop e2 = expr  { Ebinop (o, e1, e2) }
;

block:
| LCURBRACK stmts = stmt* RCURBRACK { stmts }

%inline unop:
| EXCL      { UnopNot }
| SUB       { UnopNeg }

%inline binop:
| ADD       { BinopAdd }
| SUB       { BinopSub }
| MUL       { BinopMul }
| DIV       { BinopDiv }
| MOD       { BinopMod }
| AND       { BinopAnd }
| OR        { BinopOr }
| LANGLE    { BinopLessThan }
| RANGLE    { BinopGreaterThan }
| LANGLE EQ { BinopLessThanEq }
| RANGLE EQ { BinopGreaterThanEq }
| EQ EQ     { BinopEq }
| EXCL EQ   { BinopNotEq }














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