open Printf
open Ctree

(* Print functions for Ctree *)

let string_of_typespec = function
  | Int -> "int"
  | Float -> "float"
  | Bool -> "int"  (* Boolean values often use integers *)
  | Void -> "void"
  | Generic g -> g

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

let rec print_expr expr =
  match expr with
  | ParenExpr ex ->
    printf "ParenExpr("; 
    print_expr ex; 
    printf ")"
  | FuncCall (name, args) ->
    printf "FuncCall(name: %s" name;
    List.iter (fun arg -> 
      printf " arg: "; 
      print_expr arg
    ) args;
    printf ")"
  | ArrayAccess (name, index) ->
    printf "ArrayAccess(name: %s index: %d)" name index
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

let rec print_stmt stmt =
  match stmt with
  | VarDef (ts, name, expr) ->
    printf "\nVarDef{type: %s, name: %s, value: " (string_of_typespec ts) name;
    print_expr expr;
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
  | ReturnStmt expr ->
    printf "\nReturnStmt{";
    print_expr expr;
    printf "}"
  | BreakStmt ->
    printf "\nBreakStmt"

let print_ptype = function
  | StructProto name ->
    printf "StructProto(%s)" name
  | FuncProto (ts, name, formals) ->
    printf "FuncProto(type: %s, name: %s, formals: [" (string_of_typespec ts) name;
    List.iter (fun (t, s) -> printf "(%s, %s), " (string_of_typespec t) s) formals;
    printf "])"
  
  let print_func (FuncDef (ts, name, formals, body)) =
    printf "FuncDef(type: %s, name: %s, formals: [" (string_of_typespec ts) name;
    List.iter (fun (t, s) -> printf "(%s, %s), " (string_of_typespec t) s) formals;
    printf "], body: [";
    List.iter print_stmt body;
    printf "])"
  
  let print_struct (StructDef (name, fields)) =
    printf "StructDef(name: %s, fields: [" name;
    List.iter print_stmt fields;
    printf "])"
  
  (* Main printing functions for program structure *)
  let print_ptypes ptypes =
    List.iter print_ptype ptypes
  
  let print_structs structs =
    List.iter print_struct structs
  
  let print_funcs funcs =
    List.iter print_func funcs
  
  let print_main (start, update) =
    printf "Main(start: [";
    List.iter print_stmt start;
    printf "], update: [";
    List.iter print_stmt update;
    printf "])"
  
  let print_program (ptypes, structs, funcs, main) =
    printf "Ctree Program:";
    printf "PTypes: [";
    print_ptypes ptypes;
    printf "]\nStructs: [";
    print_structs structs;
    printf "]\nFuncs: [";
    print_funcs funcs;
    printf "]\nMain: ";
    print_main main;
    printf "\n"