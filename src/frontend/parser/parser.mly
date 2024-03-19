%{
  open Ast
%}

%token <string> TYPE_INT
%token <string> TYPE_FLOAT
%token VAR
%token <int> INT
%token <string> ID
%token ADD SUB MUL DIV
%token EQ LESSER GREATER
%token LPAREN RPAREN
%token LBRACK RBRACK
%token SEMICOLON
%token PRINT START
%token EOF

%left MINUS PLUS
%left MUL DIV 

%start program

%type <Ast.program> program
%type <Ast.variable_declaration> variable_declaration


%%

program: 
  main = stmt* 
  EOF
  { { defs = [];
      main = Sblock main;} }
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

expr:
| c = INT                        { Econst c }
| id = ID                        { Evar id}
| e1 = expr o = op e2 = expr     { Ebinop (o, e1, e2) }
;

%inline op:
| ADD  { Add }
| SUB { Sub }
| MUL { Mul }
| DIV   { Div }

variable_declaration:
| type_specification ID EQ expr
    { { variable_type = type_specification;
        variable_name = ID;
        variable_value = expr } }
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