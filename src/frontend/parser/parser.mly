%{
  open Ast
%}

%token TYPE_INT TYPE_FLOAT TYPE_BOOL
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
%token IF ELSE 
%token PRINT START
%token EOF

%right ID // er lidt sketchy på den her?? men det virker
%right EQ 
%left ADD SUB LANGLE RANGLE
%left MUL DIV MOD
%left AND OR  
%nonassoc EXCL

%start program

%type <Ast.program> program

%%

// Main program
program: 
  main = stmt* 
  EOF
  { { defs = [];
      main = Sblock main;} }
;

// Statements
stmt:
| block = block         { Sblock block } // skal slettes
| vardef = vardef       { Svardef vardef }
| fundef = fundef       { Sfundef fundef }
| expr = expr SEMICOLON { Sexpr expr }
| PRINT LPAREN e = expr RPAREN SEMICOLON {Sprint e}
| START LPAREN RPAREN block = block { Sstart block } 
| IF LPAREN _cond = expr RPAREN _then = block ELSE _else = block { Sif(_cond, _then, _else) }
;

// Expressions
expr:
| LPAREN e = expr RPAREN                               { e }
| n = ID LPAREN a = separated_list(COMMA, expr) RPAREN { Efuncall (n, a) }
| c = INT                                              { Econst c }
| id = ID EQ e = expr                                  { Eassign (id, e) }
| id = ID                                              { Evar id }
| TRUE                                                 { Ebool (true) }
| FALSE                                                { Ebool (false) }
| o = unop e = expr                                    { Eunop (o, e) }
| e1 = expr o = binop e2 = expr                        { Ebinop (o, e1, e2) }
;

funcall:
| name = ID LPAREN args = separated_list(COMMA, expr) RPAREN
  {{ name; args; }}

// stmst
vardef:
| typespec = typespec name = ID EQ value = expr SEMICOLON
  {{ typespec; name; value; }}

fundef: 
| typespec = typespec name = ID LPAREN formals = separated_list(COMMA, formal) RPAREN body = block
  {{ typespec; name; formals; body; }}

formal:
| typespec = typespec name = ID 
  {{ typespec; name; }}

// Reusables
typespec:
| TYPE_INT   { Int }
| TYPE_FLOAT { Float }
| TYPE_BOOL  { Bool }
;

block:
| LCURBRACK stmts = stmt* RCURBRACK { stmts }

// Inline
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