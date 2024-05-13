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

(* class fields*)
and field = FieldDef of typespec * string * expr option

(* class start *)
type start = StartDef of block

(* class update *)
type update = UpdateDef of block

(* class methods*)
type _method = MethodDef of typespec * string * formals * block

type classblock = field list * start * update * _method list

type _class =
  | ClassStmt of string * classblock 
  | ClassInherStmt of string * string * classblock

(* program *)
type program = 
  _class * _class list

