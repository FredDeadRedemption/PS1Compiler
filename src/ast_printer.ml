open Ast

let rec print_expr expr =
  match expr with
  | ParenExpr ex -> 
    Printf.printf "ParenExpr("; 
    print_expr ex; 
    Printf.printf ")"
  | FuncCall (id, args) -> 
    Printf.printf "FuncCall(name: %s" id;
    List.iter (fun arg -> Printf.printf " arg: "; print_expr arg) args
  | ArrayAccess (id, i) -> 
    Printf.printf "ArrayAcces(id: %s index: %d)" id i
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
  | Assign (id, ex) ->
    Printf.printf "\nAssign{name: %s, value: " id;
    print_expr ex;
    Printf.printf "}"
  | PrintStmt stmt ->
    Printf.printf "\tPrintStmt: \"";
    print_expr stmt;
    Printf.printf "\""
  | StartStmt stmts ->
    Printf.printf "\tStartStmt [\n";
    List.iter (fun stmt -> Printf.printf "\t"; print_stmt stmt; Printf.printf ";\n") stmts;
    Printf.printf "\t]"
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
  
let print_program program =
  match program with
  | Main m ->
    Printf.printf "];\n\tMAIN = ";
    List.iter (fun stmt -> print_stmt stmt) m;
    Printf.printf "\n}\n"
  | Defs x ->
    Printf.printf "{\n\tdefs = [";
    List.iter (fun def -> print_stmt def) x;
    Printf.printf "\n}\n"