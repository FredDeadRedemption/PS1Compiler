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
  (2 * 3 + 4)

  (mul, 2, (add, 2, 4))
  *)

(* statements *)

type stmt =
  | Sprint of expr
  | Sstart of stmt list
  | Sblock of stmt list
  
(* funktion declaration *)

type def = {
  name    : string;
  formals : string list; (* arguments *)
  body    : stmt; }

(* program *)

type program = {
  defs : def list;
  main : stmt; }   

