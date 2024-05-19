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
  | VarDefI      of typespec * string * expr
  | VarDefU      of typespec * string 
  | ArrayDef     of typespec * string * int
  | ArrayAssign  of string * int * expr
  | Assign       of string * expr 
  | IfStmt       of expr * block
  | ElseIfStmt   of expr * block
  | ElseStmt     of block
  | WhileStmt    of expr * block
  | ReturnStmt   of expr
  | BreakStmt
  | ContinueStmt
  | ClassInit    of typespec * string
  | Increment    of string
  | Decrement    of string
  | IncrementPre of string
  | DecrementPre of string
  | IncrementVal of string * expr
  | DecrementVal of string * expr
  | MethodCall  of string option * string * params
and block = stmt list

(* class fields*)
type field = 
  | FieldDefI    of typespec * string * expr 
  | FieldDefU    of typespec * string
  | FieldClsInit of typespec * string
  

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

