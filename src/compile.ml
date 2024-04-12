open Ast

let rec generate_expr expr =
  let buffer = Buffer.create 128 in 
  let add_str str = Buffer.add_string buffer str in
  let rec generate_args args =
    match args with
    | [] -> ()
    | [x] -> add_str (generate_expr x)
    | hd :: tl ->
        add_str (generate_expr hd);
        add_str ", ";
        generate_args tl
  in
  match expr with
  | ParenExpr ex -> 
    add_str ("("); 
    add_str (generate_expr ex); 
    add_str(")"); 
    Buffer.contents buffer
  | FuncCall (id, args) ->
    add_str (id);
    add_str ("(");
    generate_args args;
    add_str ")"; 
    Buffer.contents buffer 
  | ArrayAccess (id, ind) ->
    add_str (id);
    add_str ("[");
    add_str (string_of_int ind);
    add_str ("]");
    Buffer.contents buffer
  | ConstInt c -> 
    add_str (string_of_int c); 
    Buffer.contents buffer
  | ConstFloat f -> 
    add_str (string_of_float f); 
    Buffer.contents buffer
  | Var v -> 
    add_str (v); 
    Buffer.contents buffer 
  | Bool b -> 
    add_str (int_of_bool b);
    Buffer.contents buffer
  | UnaryOp (op, ex) ->
    add_str (string_of_unop op);
    add_str (generate_expr ex);
    Buffer.contents buffer
  | BinaryOp (op, ex1, ex2) ->
    add_str (generate_expr ex1);
    add_str (string_of_binop op);
    add_str (generate_expr ex2);
    Buffer.contents buffer

let rec generate_stmt stmt =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  let add_stmt stmt =
    add_str (generate_stmt stmt);
  in
  let rec generate_formals formals =
    match formals with
    | [] -> 
      add_str "void"
    | [(ts, id)] -> 
      add_str (string_of_typespec ts);
      add_str " ";
      add_str id
    | (ts, id) :: tl ->
      add_str (string_of_typespec ts);
      add_str " ";
      add_str id;
      add_str ", ";
      generate_formals tl
  in
  match stmt with
  | VarDef (ts, id, va) ->
    add_str (string_of_typespec ts);
    add_str " ";
    add_str id;
    add_str " = ";
    add_str (generate_expr va);
    add_str ";\n"; 
    Buffer.contents buffer
  | ArrayDef (ts, id, size) ->
    add_str (string_of_typespec ts);
    add_str " ";
    add_str id;
    add_str "[";
    add_str (string_of_int size);
    add_str "];\n";
    Buffer.contents buffer
  | ArrayAssign (id, ind, ex) ->
    add_str id;
    add_str "[";
    add_str (string_of_int ind);
    add_str "]";
    add_str " = ";
    add_str (generate_expr ex);
    add_str ";\n";
    Buffer.contents buffer
  | FuncDef (ts, id, args, body) ->
    add_str (string_of_typespec ts);
    add_str " ";
    add_str id;
    add_str "(";
    generate_formals args;
    add_str "){\n";
    List.iter (fun stmt -> add_stmt stmt) body;
    add_str "\n}\n";
    Buffer.contents buffer
  | Assign (id, ex) ->
    add_str id;
    add_str " = ";
    add_str (generate_expr ex);
    add_str ";\n"; 
    Buffer.contents buffer
  | PrintStmt expr ->
    add_str "printf(";
    add_str (generate_expr expr);
    add_str ");\n";
    Buffer.contents buffer
  | StartStmt body ->
    add_str "\tSTART [\n";
    List.iter (fun stmt -> add_str "\t"; add_stmt stmt) body;
    add_str "\t]";
    Buffer.contents buffer
  | IfStmt (con, body) ->
      add_str "if(";
      add_str (generate_expr con);
      add_str "){\n";
      List.iter (fun stmt -> add_str "\t"; add_stmt stmt) body;
      add_str "\n}";
      Buffer.contents buffer
  | ElseIfStmt (con, body) ->
      add_str "else if(";
      add_str (generate_expr con);
      add_str "){\n";
      List.iter (fun stmt -> add_str "\t"; add_stmt stmt) body;
      add_str "\n}";
      Buffer.contents buffer
  | ElseStmt (body) ->
      add_str "else{\n";
      List.iter (fun stmt -> add_str "\t"; add_stmt stmt) body;
      add_str "\n}";
      Buffer.contents buffer
  | ReturnStmt ex ->
      add_str "return"; 
      add_str " ";
      add_str (generate_expr ex);
      add_str ";";
      Buffer.contents buffer
  | BreakStmt -> 
    add_str "break";
    add_str ";"; 
    Buffer.contents buffer
 

let generate_imports =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  add_str "#include <stdio.h>\n"; Buffer.contents buffer

let hoist_functions program =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  let add_stmt stmt =
    add_str "\n";
    add_str (generate_stmt stmt)
  in
  match program with
  | Main stmts ->
    List.iter (fun stmt ->
      match stmt with
      | FuncDef _ -> add_stmt stmt
      | _ -> ()) stmts;
    Buffer.contents buffer

let generate_program program =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  let add_stmt stmt =
    add_str "\n\t";
    add_str (generate_stmt stmt)
  in
  (*  Compile and generate imports  *)
  add_str generate_imports;

  (*  Compile and generate function declarations  *)
  add_str (hoist_functions program);

  (*  Compile and print main  *)
  match program with
  | Main stmts ->
    add_str "\nint main(void){\n";
    List.iter (fun stmt -> 
      match stmt with 
      | FuncDef _ -> ()
      | _ -> add_stmt stmt) stmts;
    add_str "\n\treturn 0;\n}";
    Buffer.contents buffer

    
let split_string str =
  Str.split (Str.regexp "\\.") str

let get_output_filename input_filename =
  let parts = split_string input_filename in
  match parts with
  | first :: _ -> "output_" ^ first ^ ".c"
  | _ -> failwith "File has no name."


let print_to_file filename prog =
  let prog_string = generate_program prog in
  let output_filename = get_output_filename filename in
  let out_channel = open_out output_filename in
  output_string out_channel prog_string;
  close_out out_channel;;


