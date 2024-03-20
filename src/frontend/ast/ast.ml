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

type expr =
  | Econst of int
  | Evar   of string
  | Ebinop of binop * expr * expr

  (*
  (2 * 2 + 4)

  (mul, 2, (add, 2, 4))
  *)

type variable_declaration = {
  variable_type : string;
  variable_name : string;
  variable_value : expr; }
(* statements *)

type stmt =
  | Sprint of expr
  | Sstart of stmt list
  | Sblock of stmt list
  | Sexpr of expr                          (* Expression statement *)
  | Svardef of variable_declaration   
  

  
(* funktion declaration *)

type def = {
  name    : string;
  formals : string list; (* arguments *)
  body    : stmt; }

(* program *)

type program = {
  defs : def list;
  main : stmt; }   

