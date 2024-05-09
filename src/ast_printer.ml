open Ast

(*                   *)
(* String formatting *)
(*                   *)

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


let rec print_expr expr =
  match expr with
  | ParenExpr ex -> 
    Printf.printf "ParenExpr("; 
    print_expr ex; 
    Printf.printf ")"
  | FuncCall (id, args) -> 
    Printf.printf "FuncCall(name: %s" id;
    List.iter (fun arg -> Printf.printf " arg: "; print_expr arg) args
  | ArrayAccess (id, ind) -> 
    Printf.printf "ArrayAcces(id: %s index: %d)" id ind
  | ConstInt i -> 
    Printf.printf "ConstInt(%d)" i
  | ConstFloat f -> 
    Printf.printf "ConstFloat(%f)" f
  | Var v -> 
    Printf.printf "Var(%s)" v
  | Bool b -> 
    Printf.printf "Bool(%s)" (string_of_bool b);
  | UnaryOp (op, ex) ->
    let opStr = string_of_unop op in
    Printf.printf "UnaryOp(%s, " opStr; 
    print_expr ex; 
    Printf.printf ")"  
  | BinaryOp (op, ex1, ex2) -> 
    let opStr = string_of_binop op in
    Printf.printf "BinaryOp(%s, " opStr; 
    print_expr ex1; 
    Printf.printf ", "; 
    print_expr ex2; 
    Printf.printf ")"

let print_typespec ts =
  let tsStr = string_of_typespec ts in
  Printf.printf "%s" tsStr

let rec print_stmt stmt =
  match stmt with
  | VarDef (ts, id, va) ->
    Printf.printf "\n\tVarDef{type: "; 
    print_typespec ts;
    Printf.printf ", name: %s, " id;
    Printf.printf "value: "; print_expr va; 
    Printf.printf "}\""
  | ArrayDef (ts, id, size) ->
    Printf.printf "\n\tArrayDef{type: ";
    print_typespec ts;
    Printf.printf ", name: %s, " id;
    Printf.printf "size: %d" size;
    Printf.printf "}\""
  | ArrayAssign (id, ind, ex) ->
    Printf.printf "\n\tArrayAssign{name: %s" id;
    Printf.printf ", index: %d, " ind;
    Printf.printf "value: ";
    print_expr ex;
    Printf.printf "}\""
  | FuncDef (ts, id, args, body) ->
   Printf.printf "\n\tFunction[type : "; print_typespec ts;
    Printf.printf ", name : %s, " id;
    List.iter (fun (ts, id) ->
      Printf.printf "arg(Type: %s, Name: %s) " (string_of_typespec ts) id
    ) args;
    Printf.printf "body : " ;
    List.iter (fun stmt ->  print_stmt stmt; Printf.printf ";\n") body;
    Printf.printf "]\""
  | FuncProto (_, _, _) ->
    
    Printf.printf " ";
    
    Printf.printf "(";
    Printf.printf ");\n"
  | Assign (id, ex) ->
    Printf.printf "\nAssign{name: %s, value: " id;
    print_expr ex;
    Printf.printf "}"
  | PrintStmt stmt ->
    Printf.printf "\tPrintStmt: \"";
    print_expr stmt;
    Printf.printf "\""
  | IfStmt (con, body) ->
      Printf.printf "\tIfStmt [\n";
      Printf.printf "condition: "; print_expr con;
      Printf.printf "then: "; 
      List.iter (fun stmt -> print_stmt stmt) body;
      Printf.printf "]\""
  | ElseIfStmt (con, body) ->
      Printf.printf "\tElseIfStmt [\n";
      Printf.printf "condition: "; print_expr con;
      Printf.printf "then: "; 
      List.iter (fun stmt -> print_stmt stmt) body;
      Printf.printf "]\""
  | ElseStmt (body) ->
    Printf.printf "\tElseStmt [\n";
    Printf.printf "then: "; 
    List.iter (fun stmt -> print_stmt stmt) body;
    Printf.printf "]\""          
  | ReturnStmt ex -> 
      Printf.printf "\tReturn Statement: "; 
      print_expr ex
  | BreakStmt -> Printf.printf "Break"
  

let print_class clas index =
  Printf.printf "Class%d: " !index; 
  match clas with
  | _ -> 
    Printf.printf "Suck a fat one"
  (*| Field (ts, id, va) ->
    Printf.printf "\n\tVarDef{type: "; 
    print_typespec ts;
    Printf.printf ", name: %s, " id;
    Printf.printf "value: "; print_expr va; 
    Printf.printf "}\""*)
     
  
let print_program program =
  let index = ref 0 in
  match program with
  | (gameClass, classes) ->
  Printf.printf "Game Class = ";
  index := !index + 1;
  print_class gameClass index;
  (* Iterate through the remaining classes *)
  List.iter (fun clas ->
    index := !index + 1; (* Increment the index *)
    print_class clas index
  ) classes;
  Printf.printf "\n}\n"