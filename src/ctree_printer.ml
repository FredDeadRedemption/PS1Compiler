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
  | VarDefI (ts, id, expr) ->
    printf "\nVarDef{type: %s, name: %s, value: " (string_of_typespec ts) id;
    print_expr expr;
    printf "}"
  | VarDefU (ts, id) ->
    printf "\nVarDef{type: %s, name: %s" (string_of_typespec ts) id;
    printf "}"
  | StructInit (ts, id) ->
    printf "\nStructInit{type: %s, name: %s" (string_of_typespec ts) id;
    printf "}"
  | AssignStructInit (ts) ->
    printf "\nAssignStructInit{type: %s" (string_of_typespec ts);
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
  | ForStmt (stmt, cond, incr, block) ->
    printf "\nForStmt{expr: }";
    print_stmt stmt;
    printf ", condition: ";
    print_expr cond;
    printf ", increment: ";
    print_stmt incr;
    printf ", then: ";
    List.iter print_stmt block;
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
    printf "\nWhileStmt{condition: ";
    print_expr cond;
    printf ", then: ";
    List.iter print_stmt block;
  | ReturnStmt expr ->
    printf "\nReturnStmt{";
    print_expr expr;
    printf "}"
  | BreakStmt ->
    printf "\nBreakStmt"
  | ContinueStmt ->
    printf "\nContinueStmt"
  | AssignToStruct (id, stmt) ->
    printf "\nAssignToStruct{ Stmt to assign: {";
    print_stmt stmt;
    printf "}, to obj: %s" id;
    printf "}"
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
  | FuncCall (_, name, args) ->
    printf "FuncCall(name: %s" name;
    List.iter (fun arg -> 
      printf " arg: "; 
      print_expr arg
    ) args;
    printf ")"
 


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
  
  let print_struct (StructDef (id, fields)) =
    printf "StructDef(name: %s, fields: [" id;
    List.iter print_stmt fields;
    printf "])"

  let print_constructor (Constructor(ts, id, stmts, return_stmt)) =
    printf "Constructor(Typespec: %s, FuncName: %s, Fields: [" (string_of_typespec ts) id;
    List.iter print_stmt stmts;
    printf "], Return: ";
    print_stmt return_stmt;
    printf ")"
    
  (* Main printing functions for program structure *)
  let print_ptypes ptypes =
    List.iter print_ptype ptypes
  
  let print_structs structs =
    List.iter print_struct structs
  
  let print_constructors constructors =
    List.iter print_constructor constructors
  
  let print_funcs funcs =
    List.iter print_func funcs
  
  let print_main (start, update) =

      match start with
      | Start start_block -> 
        Printf.printf "Main(start: [";
        List.iter print_stmt start_block;
        Printf.printf "]";
    
      match update with
      | Update update_block ->
        Printf.printf ", update: [";
        List.iter print_stmt update_block;
        Printf.printf "])"
   
  
  let print_program (ptypes, structs, constructors, funcs, main) =
    printf "Ctree Program:";
    printf "PTypes: [";
    print_ptypes ptypes;
    printf "]\nStructs: [";
    print_structs structs;
    printf "]\nConstructers: [";
    print_constructors constructors;
    printf "]\nFuncs: [";
    print_funcs funcs;
    printf "]\nMain: ";
    print_main main;
    printf "\n"