
(* integer expressions *)

type binop = Add | Sub | Mul | Div | Lesser | Greater | Mod | And | Or | Equal

type expr =
  | Econst of int
  | Evar   of string
  | Ebool  of bool
  | Ebinop of binop * expr * expr

  (*
  (2 * 3 + 4)

  (mul, 2, (add, 2, 4))
  *)

(* statements *)

type stmt =
  | Sif      of expr * stmt * stmt
  | Sblock   of stmt list
  | Scall    of string * expr list

  (*
    Sif(expr: 2 > 1){
      stmt 1
    } else {
      stmt 2
    }

    Scall = (foo, [(add, 2, (minus, 2, 3))])

    foo(2 + 2 - 3)
    moveSprite(x, y, z, æ)

  *)

(* funktion declaration *)

type def = {
  name    : string;
  formals : string list; (* arguments *)
  body    : stmt; }

(* program *)

type program = {
  defs : def list;
  main : stmt; }