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
(* expr and expre_block are mutually recursive types. 
The "and" keyword is used to define both types simultaneously.
and expr_block = 
  | expr list 
*)

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
  (*| Sfundef of def*)

(* function declaration *)
type def = {
  rtype   : string;
  name    : string;
  formals : string list; (* arguments *)
  body    : stmt; }

(* program *)
type program = {
  defs : def list;
  main : stmt; }  



