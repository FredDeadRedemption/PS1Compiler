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
  | FuncCall    of string * expr list
  | ArrayAccess of string * int
  | ConstInt    of int
  | ConstFloat  of float
  | Var         of string
  | Bool        of bool
  | UnaryOp     of unop * expr
  | BinaryOp    of binop * expr * expr
  

type typespec = 
  | Int
  | Float
  | Bool

(* formal i.e. a typed argument for function / method definiton *)
type formal = typespec * string

(* statement *)
type stmt =
  | VarDef      of typespec * string * expr 
  | ArrayDef    of typespec * string * int
  | ArrayAssign of string * int * expr
  | FuncDef     of typespec * string * formal list * block
  | Assign      of string * expr 
  | PrintStmt   of expr 
  | StartStmt   of block
  | IfStmt      of expr * block
  | ElseIfStmt  of expr * block
  | ElseStmt    of block
  | ReturnStmt  of expr
  | BreakStmt
and block = stmt list
(* function declaration *)

(* program *)
type program = 
  | Defs of block
  | Main of block

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
