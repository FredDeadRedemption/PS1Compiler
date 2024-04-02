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
  | Efuncall of funcall
  | Eassign of string * expr 
(* and is for mutually recursive types *)
and funcall = {
  name : string;
  args : expr list;
}


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

(* creates a map containing typespecification and name for each formal *)
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
  | Sif of expr * stmt list * stmt list
(* function declaration *)
and fundef = {
  typespec : typespec;
  name     : string;
  formals     : formal list; (* formal arguments *)
  body     : stmt list; 
}

(* program *)
type program = {
  defs : fundef list;
  main : stmt; 
}  



