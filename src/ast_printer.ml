open Ast

(*                   *)
(* String formatting *)
(*                   *)
(*
let int_of_bool = function 
  | true -> "1"
  | false -> "0"

let string_of_typespec = function
  | Int -> "int"
  | Float -> "float"
  | Bool -> "int"
  | Void -> "void"
  | Generic (g) -> g

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


  let rec generate_expr = function
  | ParenExpr ex ->
    "ParenExpr(" ^ generate_expr ex ^ ")"
  | FuncCall (id, args) ->
    let args_str = String.concat ", " (List.map generate_expr args) in
    "FuncCall(name: " ^ id ^ ", args: [" ^ args_str ^ "])"
  | ArrayAccess (id, ind) ->
    "ArrayAccess(id: " ^ id ^ ", index: " ^ string_of_int ind ^ ")"
  | ConstInt i ->
    "ConstInt(" ^ string_of_int i ^ ")"
  | ConstFloat f ->
    "ConstFloat(" ^ string_of_float f ^ ")"
  | Var v ->
    "Var(" ^ v ^ ")"
  | Bool b ->
    "Bool(" ^ string_of_bool b ^ ")"
  | UnaryOp (op, ex) ->
    let opStr = string_of_unop op in
    "UnaryOp(" ^ opStr ^ ", " ^ generate_expr ex ^ ")"
  | BinaryOp (op, ex1, ex2) ->
    let opStr = string_of_binop op in
    "BinaryOp(" ^ opStr ^ ", " ^ generate_expr ex1 ^ ", " ^ generate_expr ex2 ^ ")"

let print_typespec ts =
  let tsStr = string_of_typespec ts in
  Printf.printf "%s" tsStr

let rec print_stmt stmt =
  match stmt with
  | VarDef (ts, id, va) ->
    Printf.printf "\n\tVarDef{type: %s, name: %s, value: %s}\"" (string_of_typespec ts) id (generate_expr va)
  | ArrayDef (ts, id, size) ->
    Printf.printf "\n\tArrayDef{type: %s, name: %s, size: %d}\"" (string_of_typespec ts) id size
  | ArrayAssign (id, ind, ex) ->
    Printf.printf "\n\tArrayAssign{name: %s, index: %d, value: %s}\"" id ind (generate_expr ex)
  | Assign (id, ex) ->
    Printf.printf "\nAssign{name: %s, value: %s}" id (generate_expr ex)
  | PrintStmt ex ->
    Printf.printf "\tPrintStmt: \"%s\"" (generate_expr ex)
  | IfStmt (con, body) ->
      Printf.printf "\tIfStmt [\ncondition: %s\nthen: \n" (generate_expr con);
      List.iter print_stmt body;
      Printf.printf "]"
  | ElseIfStmt (con, body) ->
      Printf.printf "\tElseIfStmt [\ncondition: %s\nthen: \n" (generate_expr con);
      List.iter print_stmt body;
      Printf.printf "]"
  | ElseStmt (body) ->
    Printf.printf "\tElseStmt [\nthen: \n";
    List.iter print_stmt body;
    Printf.printf "]"
  | ReturnStmt ex -> 
      Printf.printf "\tReturn Statement: %s" (generate_expr ex)
  | BreakStmt -> 
    Printf.printf "Break"

let generate_field = function
| FieldDef (ts, name, None) ->
    Printf.sprintf "%s %s" (string_of_typespec ts) name
| FieldDef (ts, name, Some expr) ->
    Printf.sprintf "%s %s = %s" (string_of_typespec ts) name (generate_expr expr)

let print_fields fields = 
  Printf.printf "Fields:\n";
  let field_strs = List.map generate_field fields in
  List.iter (Printf.printf "%s\n") field_strs

let print_block block = 
  List.iter print_stmt block

let print_start (StartDef block) =
  Printf.printf "Start block: \n";
  print_block block;
  Printf.printf "\n"

let print_update (UpdateDef block) =
  Printf.printf "Update block: \n";
  print_block block;
  Printf.printf "\n"

let string_of_formals formals =
  String.concat ", " (List.map (fun (ts, name) -> Printf.sprintf "%s %s" (string_of_typespec ts) name) formals)

let print_methods (MethodDef (ts, id, formals, block)) =
  Printf.printf "Method %s: \n" id;
  Printf.printf "Return type: %s\n" (string_of_typespec ts);
  Printf.printf "Parameters: (%s)\n" (string_of_formals formals);
  print_block block;
  Printf.printf "\n"


let rec print_class (id, (field_list, start, update, method_list)) index = 
  Printf.printf "Class%d: %s\n" index id;
  Printf.printf "Fields:\n";
  print_fields field_list;
  print_start start;
  print_update update;
  Printf.printf "Methods:\n";
  List.iter print_methods method_list;

  
let print_program (gameClass, classes) =
  let index = ref 0 in
  Printf.printf "AST:";
  Printf.printf "Game Class = ";
  index := !index + 1;
  print_class gameClass !index;
  (* Iterate through the remaining classes *)
  List.iter (fun clas ->
    index := !index + 1; (* Increment the index *)
    print_class clas !index
  ) classes;
  Printf.printf "\n}\n"

*)

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
  | FuncCall (name, args) ->
    printf "FuncCall(name: %s" name;
    List.iter (fun arg -> 
      printf ", arg: "; 
      print_expr arg
    ) args;
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

(* Field Printing *)
let print_field (FieldDef (ts, name, expr_opt)) =
  match expr_opt with
  | Some expr ->
    printf "(type: %s, name: %s, default: " (string_of_typespec ts) name;
    print_expr expr;
    printf ")"
  | None ->
    printf "(type: %s, name: %s) " (string_of_typespec ts) name



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
  printf "Fields: [";
  List.iter print_field fields;
  printf "] Start: ";
  print_start start;
  printf " Update: ";
  print_update update;
  printf " Methods: [";
  List.iter print_method methods;
  printf "]"

(* Class Statement Printing *)
let print_class cls index =
  match cls with
  | ClassStmt (name, classblock) -> 
    printf "Class%d: %s {" index name;
    print_classblock classblock;
    printf "}"
  | ClassInherStmt (name, inher, classblock) -> 
    printf "Class%d: %s" index name;
    printf ", extends %s {" inher;
    print_classblock classblock;
    printf "}"

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


