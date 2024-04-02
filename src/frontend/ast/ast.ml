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

type typespec = 
| Int
| Float
| Bool

let string_of_typespec = function 
| Int -> "int" 
| Float -> "float"
| Bool -> "bool"

(* variable declaration *)
type vardef = {
  typespec : typespec;
  name     : string;
  value    : expr; 
}

(* formal i.e. a typed argument for function / method definiton *)
type formal = {
  typespec : typespec;
  name : string;
}

let map_formals params = 
  let get_info formal =
    (formal.typespec, formal.name) 
  in 
  List.map get_info params


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
  typespec : typespec;
  name     : string;
  args     : formal list; (* arguments *)
  body     : stmt list; 
}


(* program *)
type program = {
  defs : fundef list;
  main : stmt; 
}  



