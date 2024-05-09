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
  | Void
  | Generic of string

(* formal i.e. a typed argument for function / method definiton *)
type formal = typespec * string
type formals = formal list

(* statement *)
type stmt =
  | VarDef      of typespec * string * expr 
  | ArrayDef    of typespec * string * int
  | ArrayAssign of string * int * expr
  | FuncDef     of typespec * string * formals * block
  | FuncProto   of typespec * string * formals
  | Assign      of string * expr 
  | PrintStmt   of expr 
  | IfStmt      of expr * block
  | ElseIfStmt  of expr * block
  | ElseStmt    of block
  | ReturnStmt  of expr
  | BreakStmt
  | StructDef   of string * stmt list
  | StructProto of string 
  | Update of block (*måske gøre dette ved "type update" istedet*)

and block = stmt list

type ptypes =
  stmt list

type funcs = 
  stmt list



type structs =
  stmt list

type start = 
  stmt list

type update = 
  stmt list

type main =
 start * update 

type program = 
  ptypes * funcs * structs * main