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
  | Efloat of float
  | Evar   of string
  | Ebool of bool
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Efuncall of string * expr list


type typespec = 
  | Int
  | Float
  | Bool

(* formal i.e. a typed argument for function / method definiton *)
type formal = typespec * string

(* statement *)
type stmt =
  | Sprint of expr 
  | Sstart of block
  | Sblock of block
  | Sexpr of expr
  | Svardef of typespec * string * expr 
  | Sif of expr * block * block option
  | Sassign of string * expr 
  | Sfundef of typespec * string * formal list * block
  | Sreturn of expr
  | Sbreak
and block = stmt list
(* function declaration *)

(* program *)
type program = {
  defs : stmt list;
  main : stmt; 
}  

(*                   *)
(* String formatting *)
(*                   *)
let string_of_typespec = function
  | Int -> "Int"
  | Float -> "Float"
  | Bool -> "Bool"

let string_of_unop = function
  | UnopNot -> "!"
  | UnopNeg -> "-"
    
let string_of_binop = function
  | BinopAdd -> "+"
  | BinopSub -> "-"
  | BinopMul -> "*"
  | BinopDiv -> "/" 
  | BinopMod -> "%" 
  | BinopAnd -> "&&" 
  | BinopOr -> "||"
  | BinopLessThan -> "<" 
  | BinopGreaterThan -> ">"
  | BinopLessThanEq -> "<=" 
  | BinopGreaterThanEq -> ">="
  | BinopEq -> "=="
  | BinopNotEq -> "!="
