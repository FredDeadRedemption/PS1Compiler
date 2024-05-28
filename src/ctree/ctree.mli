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
  | VarChain    of string list
  | VarAddress  of string
  | Bool        of bool
  | UnaryOp     of unop * expr
  | BinaryOp    of binop * expr * expr  
  | AssignToStructExpr of string * expr
  | FuncCallExpr    of  string * params
  | StringExpr  of string

  
  
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
  | VarDefI     of typespec * string * expr 
  | VarDefU     of typespec * string
  | StructInit  of typespec * string
  | AssignStructInit  of typespec
  | AssignToStructStmt of string * stmt
  | ArrayDef    of typespec * string * int
  | ArrayAssign of string * int * expr
  | Assign      of string * expr 
  | ForStmt     of stmt * expr * stmt * block
  | ObjectPropAssign of string * string list * expr
  | IfStmt      of expr * block
  | ElseIfStmt  of expr * block
  | ElseStmt    of block
  | WhileStmt   of expr * block
  | ReturnStmt  of expr
  | BreakStmt 
  | ContinueStmt
  | Increment    of string
  | Decrement    of string
  | IncrementPre of string
  | DecrementPre of string
  | IncrementVal of string * expr
  | DecrementVal of string * expr
  | FuncCallStmt    of  string * params
  | Render       of string
and block = stmt list


type ptype = 
  | FuncProto   of typespec * string * formals

type ptypes =
  ptype list

type func = FuncDef of typespec * string * formals * block
type funcs = 
  func list


type _struct = StructDef of string * stmt list
type structs =
  _struct list

type constructor = Constructor of typespec * string * stmt list * stmt
type constructors =
  constructor list

type start = Start of block
  
type update = Update of block 

type main =
 start * update 

type program = 
  structs * ptypes * constructors * funcs * main