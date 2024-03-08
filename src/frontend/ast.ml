
(* integer expressions *)

type binop = Add | Sub | Mul | Div | Langle | Rangle | Mod | And | Or

type expr =
  | Econst of int
  | Evar   of string
  | Ebool  of bool
  | Ebinop of binop * expr * expr

(* statements *)

type stmt =
  | Sif      of expr * stmt * stmt
  | Sblock   of stmt list
  | Scall    of string * expr list

(* method definition *)

type def = {
  name    : string;
  formals : string list; (* arguments *)
  body    : stmt; }

(* program *)

type program = {
  defs : def list;
  main : stmt; }