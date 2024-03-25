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
type vardef = {
  typespec : string;
  name     : string;
  value    : expr; 
}

type typedarg = {
  typespec : string;
  name : string;
}


(* statement *)
type stmt =
  | Sprint of expr
  | Sstart of stmt list
  | Sblock of stmt list
  | Sexpr of expr
  | Svardef of vardef 
  | Sfundef of fundef
  (*| Sfundef of def*)
(* function declaration *)
and fundef = {
  typespec : string;
  name     : string;
  args     : typedarg list; (* arguments *)
  body     : stmt list; 
}


(* program *)
type program = {
  defs : fundef list;
  main : stmt; 
}  



