
(* integer expressions *)

type binop = Add | Sub | Mul | Div | Lesser | Greater | Mod | And | Or | Equal

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
  | Sblock   of stmt list
  
(* funktion declaration *)

type def = {
  name    : string;
  formals : string list; (* arguments *)
  body    : stmt; }

(* program *)

type program = {
  defs : def list;
  main : stmt; }   

