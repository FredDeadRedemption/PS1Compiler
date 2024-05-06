%{
  open Ast
%}

%token TYPE_INT TYPE_FLOAT TYPE_BOOL 
%token CLASS
%token <int> INT
%token <string> ID
%token <string> TYPE_GENERIC
%token <float> FLOAT
%token ADD SUB MUL DIV 
%token MOD EQ EXCL
%token AND OR
%token TRUE FALSE
%token LANGLE RANGLE
%token LPAREN RPAREN
%token LSQBRACK RSQBRACK
%token LCURBRACK RCURBRACK
%token COMMA SEMICOLON
%token COLON
%token IF ELSE 
%token PRINT START
%token RETURN BREAK
%token EOF

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
  stmt* EOF { Main($1) }
;

// Expressions
expr:
| LPAREN expr RPAREN                                   { ParenExpr($2) }
| ID LPAREN separated_list(COMMA, expr) RPAREN         { FuncCall($1, $3) }
| ID LSQBRACK INT RSQBRACK                             { ArrayAccess($1, $3) }
| INT                                                  { ConstInt($1) }
| FLOAT                                                { ConstFloat($1) }
| ID                                                   { Var($1) }
| TRUE                                                 { Bool(true) }
| FALSE                                                { Bool(false) }
| unop expr                                            { UnaryOp($1, $2) }
| expr binop expr                                      { BinaryOp($2, $1, $3) }
;

// Statements
stmt:
| typespec ID EQ expr SEMICOLON                        { VarDef($1, $2, $4) }
| typespec ID LSQBRACK INT RSQBRACK SEMICOLON          { ArrayDef($1, $2, $4) }
| ID LSQBRACK INT RSQBRACK EQ expr SEMICOLON           { ArrayAssign($1, $3, $6) }
| typespec ID LPAREN separated_list(COMMA, formal) RPAREN block { FuncDef($1, $2, $4, $6) }
| ID EQ expr SEMICOLON                                 { Assign($1, $3) }
| PRINT LPAREN expr RPAREN SEMICOLON                   { PrintStmt($3) }
| START LPAREN RPAREN block                            { StartStmt($4) } 
| IF LPAREN expr RPAREN block                          { IfStmt($3, $5) }
| ELSE IF LPAREN expr RPAREN block                     { ElseIfStmt($4, $6) }
| ELSE block                                           { ElseStmt($2) }
| RETURN expr SEMICOLON                                { ReturnStmt($2) }
| BREAK SEMICOLON                                      { BreakStmt }
| CLASS TYPE_GENERIC classblock                        { ClassStmt($2, $3) }
//| TYPE_CLASS ID COLON ID block                    { ClassInherStmt($2, $4) }
;

// Reusables
typespec:
| TYPE_INT     { Int }
| TYPE_FLOAT   { Float }
| TYPE_BOOL    { Bool }
| TYPE_GENERIC { Generic($1) }
;

formal:
| typespec ID { ($1, $2) } 

block:
| LCURBRACK stmt* RCURBRACK { ($2) }

classblock:
| LCURBRACK fields start methods RCURBRACK { ($2, $3, $4) }
;

/*
member:
| field   { $1 }
| start   { $1 }
| _method { $1 }
;
*/

fields: 
| field* { ($1) }
;

field:
| typespec ID initialize? SEMICOLON { FieldDef($1, $2, $3) }
;

initialize:
| EQ expr { $2 }
;

start:
| START LPAREN RPAREN block { $4 }
;

methods:
| _method* { ($1) }
;

_method:
| typespec ID LPAREN separated_list(COMMA, formal) RPAREN block { MethodDef($1, $2, $4, $6) }
;




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