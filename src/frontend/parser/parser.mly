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
%token LANGLE RANGLE
%token LPAREN RPAREN
%token LBRACK RBRACK
%token SEMICOLON
%token PRINT START
%token EOF

%right EQ 
%left ADD SUB RANGLE LANGLE
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


type_specification:
| TYPE_INT { "int" }
| TYPE_FLOAT { "float" }
;

stmt:
| def = def
    {Svardef def}
| expr = expr SEMICOLON
    { Sexpr expr }
| PRINT LPAREN e = expr RPAREN SEMICOLON 
    {Sprint e}
| START LPAREN RPAREN LBRACK
    body = stmt*
  RBRACK
    {Sstart body}
;

def:
| variable_type = type_specification variable_name = ID EQ variable_value = expr SEMICOLON
    { { variable_type = variable_type;
        variable_name = variable_name;
        variable_value = variable_value; } }

type_specification:
| TYPE_INT {"int"}
| TYPE_FLOAT {"float"}
;

expr:
| c = INT                        { Econst c }
| id = ID                        { Evar id}
| e1 = expr o = op e2 = expr     { Ebinop (o, e1, e2) }
;

%inline op:
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
| EXCL EQ   { BinopNotEq}














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