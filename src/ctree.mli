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
  | Assign      of string * expr 
  | IfStmt      of expr * block
  | ElseIfStmt  of expr * block
  | ElseStmt    of block
  | ReturnStmt  of expr
  | BreakStmt
and block = stmt list

type ptype = 
  | StructProto of string 
  | FuncProto   of typespec * string * formals

type ptypes =
  ptype list

type func = FuncDef of typespec * string * formals * block
type funcs = 
  func list


type _struct = StructDef of string * stmt list
type structs =
  _struct list

type start = Start of block
  
type update = Update of block 

type main =
 start * update 

type program = 
  ptypes * structs * funcs * main