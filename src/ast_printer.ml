open Ast

open Printf

(* Typespec Printing *)
let string_of_typespec = function
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Void -> "void"
  | Generic g -> g

(* Unary Operation Printing *)
let string_of_unop = function
  | UnopNot -> "!"
  | UnopNeg -> "-"

(* Binary Operation Printing *)
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

(* Expression Printing *)
let rec print_expr expr =
  match expr with
  | ParenExpr ex ->
    printf "ParenExpr("; 
    print_expr ex; 
    printf ")"
  | ArrayAccess (name, index) ->
    printf "ArrayAccess(name: %s, index: %d)" name index
  | ConstInt i ->
    printf "ConstInt(%d)" i
  | ConstFloat f ->
    printf "ConstFloat(%f)" f
  | Var v ->
    printf "Var(%s)" v
  | Bool b ->
    printf "Bool(%b)" b
  | UnaryOp (op, ex) ->
    let opStr = string_of_unop op in
    printf "UnaryOp(%s, " opStr; 
    print_expr ex; 
    printf ")"  
  | BinaryOp (op, ex1, ex2) ->
    let opStr = string_of_binop op in
    printf "BinaryOp(%s, " opStr; 
    print_expr ex1; 
    printf ", "; 
    print_expr ex2; 
    printf ")"

(* Statement Printing *)
let rec print_stmt stmt =
  match stmt with
  | VarDefI (ts, name, expr) ->
    printf "\nVarDef{type: %s, name: %s, value: " (string_of_typespec ts) name;
    print_expr expr;
    printf "}"
  | VarDefU (ts, name) ->
    printf "\nVarDef{type: %s, name: %s" (string_of_typespec ts) name;
    printf "}"
  | ArrayDef (ts, name, size) ->
    printf "\nArrayDef{type: %s, name: %s, size: %d}" (string_of_typespec ts) name size
  | ArrayAssign (name, index, expr) ->
    printf "\nArrayAssign{name: %s, index: %d, value: " name index;
    print_expr expr;
    printf "}"
  | Assign (name, expr) ->
    printf "\nAssign{name: %s, value: " name;
    print_expr expr;
    printf "}"
  | IfStmt (cond, block) ->
    printf "\nIfStmt{condition: "; 
    print_expr cond;
    printf ", then: ";
    List.iter print_stmt block;
    printf "}"
  | ElseIfStmt (cond, block) ->
    printf "\nElseIfStmt{condition: "; 
    print_expr cond;
    printf ", then: ";
    List.iter print_stmt block;
    printf "}"
  | ElseStmt block ->
    printf "\nElseStmt{then: ";
    List.iter print_stmt block;
    printf "}"
  | WhileStmt (cond, block) ->
    printf "\n WhileStmt {condition: ";
    print_expr cond;
    printf ", then: ";
    List.iter print_stmt block;
    printf "}"
  | ReturnStmt expr ->
    printf "\nReturnStmt{";
    print_expr expr;
    printf "}"
  | BreakStmt ->
    printf "\nBreakStmt"
  | ContinueStmt ->
    printf "\nContinueStmt"
  | ClassInit (typespec, id) ->
    printf "\nClassInit{ts: %s, id: %s}" (string_of_typespec typespec) id
  | Increment (id) ->
    printf "\nIncrementSuffix{id: %s}" id
  | Decrement (id) ->
    printf "\nDecrementSuffix{id: %s}" id
  | IncrementPre (id) ->
    printf "\nIncrementPrefix{id: %s}" id
  | DecrementPre (id) ->
    printf "\nDecrementPrefix{id: %s}" id
  | IncrementVal (id, expr) ->
    printf "\nIncrementVal{id: %s}" id;
    print_expr expr
  | DecrementVal (id, expr) ->
    printf "\nDecrementVal{id: %s}" id;
    print_expr expr
  | MethodCall (ref_opt, mthd, args) ->
    let current_ref = match ref_opt with 
      | Some "this" -> "this"
      | Some "super" -> "super"   
      | Some id -> id
      | None -> "None"
    in
    printf "MethodCall(ref: %s, method: %s" current_ref mthd;
    List.iter (fun arg -> 
      printf ", arg: "; 
      print_expr arg
    ) args;
    printf ")"

(* Field Printing *)
let print_field field =
  match field with
  | FieldDefI (ts, name, e) ->
    printf "(type: %s, name: %s, default: " (string_of_typespec ts) name;
    print_expr e;
    printf ")"
  | FieldDefU (ts, name) ->
    printf "(type: %s, name: %s) " (string_of_typespec ts) name
  | FieldClsInit (typespec, id) ->
    printf "\nClassInit{ts: %s, id: %s}" (string_of_typespec typespec) id




(* Start Definition Printing *)
let print_start (StartDef block) =
  List.iter print_stmt block

(* Update Definition Printing *)
let print_update (UpdateDef block) =
  List.iter print_stmt block

(* Method Printing *)
let print_method (MethodDef (ts, name, formals, block)) =
  printf "(type: %s, name: %s, formals: [" (string_of_typespec ts) name;
  List.iter (fun (ftype, fname) -> 
    printf "(type: %s, name: %s) " (string_of_typespec ftype) fname
  ) formals;
  printf "], body: ";
  List.iter print_stmt block;
  printf ")"

(* Class Block Printing *)
let print_classblock (fields, start, update, methods) =
  printf " Fields: [";
  List.iter print_field fields;
  printf "]\n Start: [";
  print_start start;
  printf "]\n Update: [";
  print_update update;
  printf "]\n Methods: [";
  List.iter print_method methods;
  printf "]"

(* Class Statement Printing *)
let print_class cls index =
  match cls with
  | ClassStmt (name, classblock) -> 
    printf "Class%d: %s {\n" index name;
    print_classblock classblock;
    printf "}\n"
  | ClassInherStmt (name, inher, classblock) -> 
    printf "Class%d: %s" index name;
    printf ", extends %s {\n" inher;
    print_classblock classblock;
    printf "}\n"

(* Program Printing *)
let print_program (gcls, clss) =
  let index = ref 0 in
  Printf.printf "AST:";
  Printf.printf "Game Class = ";
  index := !index + 1;
  print_class gcls !index;

  List.iter (fun cls ->
    index := !index + 1; (* Increment the index *)
    print_class cls !index
  ) clss;
  printf "\n"


