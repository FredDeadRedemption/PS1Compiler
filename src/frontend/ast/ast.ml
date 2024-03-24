(* binary operations *)
type binop = 
| BinopAdd 
| BinopSub 
| BinopMul 
| BinopDiv
| BinopMod
| BinopAnd
| BinopOr 
| BinopLessThan 
| BinopGreaterThan 
| BinopLessThanEq
| BinopGreaterThanEq
| BinopEq
| BinopNotEq

(* unary operations *)
type unop =
| UnopNot
| UnopNeg

(* expression *)
type expr = 
  | Econst of int
  | Evar   of string
  | Ebool of bool
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Eparen of expr

(* variable declaration *)
type variable_declaration = {
  variable_type : string;
  variable_name : string;
  variable_value : expr; }

(* statement *)
type stmt =
  | Sprint of expr
  | Sstart of stmt list
  | Sblock of stmt list
  | Sexpr of expr
  | Svardef of variable_declaration  

(* function declaration *)
type def = {
  name    : string;
  formals : string list; (* arguments *)
  body    : stmt; }

(* program *)
type program = {
  defs : def list;
  main : stmt; }  



