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
  | ParenExpr   of expr
  | FuncCall    of string * params
  | ArrayAccess of string * int
  | ConstInt    of int
  | ConstFloat  of float
  | Var         of string
  | Bool        of bool
  | UnaryOp     of unop * expr
  | BinaryOp    of binop * expr * expr
and params = expr list
  
type typespec = 
  | Int
  | Float
  | Bool

(* formal i.e. a typed argument for function / method definiton *)
type formal = typespec * string
type formals = formal list

(* statement *)
type stmt =
  | VarDef      of typespec * string * expr 
  | ArrayDef    of typespec * string * int
  | ArrayAssign of string * int * expr
  | FuncDef     of typespec * string * formals * block
  | Assign      of string * expr 
  | PrintStmt   of expr 
  | StartStmt   of block
  | IfStmt      of expr * block
  | ElseIfStmt  of expr * block
  | ElseStmt    of block
  | ReturnStmt  of expr
  | BreakStmt
and block = stmt list

(* program *)
type program = 
  | Main of block

(*                   *)
(* String formatting *)
(*                   *)
let int_of_bool = function 
  | true -> "1"
  | false -> "0"

let string_of_typespec = function
  | Int -> "int"
  | Float -> "float"
  | Bool -> "int"

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
