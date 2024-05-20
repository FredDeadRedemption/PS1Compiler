%{
  open Ast
%}

%token TYPE_INT TYPE_FLOAT TYPE_BOOL TYPE_VOID
%token CLASS
%token NEW
%token <string> THIS
%token <string> SUPER
%token <string> GAMEOBJECT
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
%token DOT
%token FOR
%token IF ELSE 
%token WHILE
%token PRINT 
%token START UPDATE
%token RETURN BREAK CONTINUE
%token INCR DECR INCRBYVAL DECRBYVAL
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
  gameClass _class* EOF { ($1, $2) }
;

// GameClass
gameClass:
| _class { $1 }
;

// Class
_class:
| CLASS TYPE_GENERIC classblock { ClassStmt($2, $3) }
| CLASS TYPE_GENERIC COLON GAMEOBJECT classblock { ClassInherStmt($2, $4, $5) }
;

classblock:
| LCURBRACK fields start update methods RCURBRACK { ($2, $3, $4, $5) }
;

fields: 
| field* { ($1) }
;

field:
| typespec ID EQ expr SEMICOLON { FieldDefI($1, $2, $4) }
| typespec ID SEMICOLON         { FieldDefU($1, $2) }
| typespec ID EQ NEW typespec LPAREN RPAREN SEMICOLON  { FieldClsInit($1, $2) }
;

start:
| START LPAREN RPAREN block { StartDef($4) }
;

update:
| UPDATE LPAREN RPAREN block { UpdateDef($4) }
;

methods:
| _method* { ($1) }
;

_method:
| typespec ID LPAREN separated_list(COMMA, formal) RPAREN block { MethodDef($1, $2, $4, $6) }
;

prop:
| DOT ID { ($2) }
;

// Expressions
expr:
| LPAREN expr RPAREN                                   { ParenExpr($2) }
| ID LSQBRACK INT RSQBRACK                             { ArrayAccess($1, $3) }
| INT                                                  { ConstInt($1) }
| FLOAT                                                { ConstFloat($1) }
| ID                                                   { Var($1) }
| ref prop+                                             { VarChain($1, $2) }
| TRUE                                                 { Bool(true) }
| FALSE                                                { Bool(false) }
| unop expr                                            { UnaryOp($1, $2) }
| expr binop expr                                      { BinaryOp($2, $1, $3) }
// TODO: der mangler, så man kan have variable i epxr
;

// Statements
stmt:
| typespec ID EQ expr SEMICOLON                        { VarDefI($1, $2, $4) }
| typespec ID SEMICOLON                                { VarDefU($1, $2) }
| typespec ID LSQBRACK INT RSQBRACK SEMICOLON          { ArrayDef($1, $2, $4) }
| ID LSQBRACK INT RSQBRACK EQ expr SEMICOLON           { ArrayAssign($1, $3, $6) }
| ID EQ expr SEMICOLON                                 { Assign($1, $3) }
| ref prop+ EQ expr SEMICOLON                           { ObjectPropAssign($1, $2, $4) }
| FOR LPAREN stmt expr SEMICOLON stmt RPAREN block     { ForStmt($3, $4, $6, $8) }
| IF LPAREN expr RPAREN block                          { IfStmt($3, $5) }
| ELSE IF LPAREN expr RPAREN block                     { ElseIfStmt($4, $6) }
| ELSE block                                           { ElseStmt($2) }
| WHILE LPAREN expr RPAREN block                       { WhileStmt($3, $5) }
| RETURN expr SEMICOLON                                { ReturnStmt($2) }
| BREAK SEMICOLON                                      { BreakStmt }
| CONTINUE SEMICOLON                                   { ContinueStmt }
| typespec ID EQ NEW typespec LPAREN RPAREN SEMICOLON  { ClassInit($1, $2) }
| ID DECR SEMICOLON                                    { Decrement($1) }
(*| expr INCR SEMICOLON                                  { Increment($1) }*)
| DECR ID SEMICOLON                                    { DecrementPre($2) }
| INCR ID SEMICOLON                                    { IncrementPre($2) }
| ID INCRBYVAL expr SEMICOLON                          { IncrementVal($1, $3) }
| ID DECRBYVAL expr SEMICOLON                          { DecrementVal($1, $3) }

| ID LPAREN separated_list(COMMA, expr) RPAREN SEMICOLON { MethodCallStmt(None, $1, $3) }
| ref DOT ID LPAREN separated_list(COMMA, expr) RPAREN SEMICOLON { MethodCallStmt(Some($1), $3, $5) }
;

ref: 
| ID       { $1 }
| THIS     { $1 }
| SUPER    { $1 }
;

// Reusables
typespec:
| TYPE_INT     { Int }
| TYPE_FLOAT   { Float }
| TYPE_BOOL    { Bool }
| TYPE_VOID    { Void }
| TYPE_GENERIC { Generic($1) }
;

formal:
| typespec ID { ($1, $2) } 

block:
| LCURBRACK stmt* RCURBRACK { ($2) }

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

