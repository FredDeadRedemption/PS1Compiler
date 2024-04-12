open Ast

let rec string_of_expr expr =
  let buffer = Buffer.create 128 in 
  let add_str s = Buffer.add_string buffer s in
  let rec print_args args =
    match args with
    | [] -> ()
    | [x] -> add_str (string_of_expr x)
    | hd :: tl ->
        add_str (string_of_expr hd);
        add_str ", ";
        print_args tl
  in
  match expr with
  | ParenExpr ex -> 
    add_str ("("); 
    add_str (string_of_expr ex); 
    add_str(")"); 
    Buffer.contents buffer
  | FuncCall (id, args) ->
    add_str (id);
    add_str ("(");
    print_args args;
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
    add_str (string_of_expr ex);
    Buffer.contents buffer
  | BinaryOp (op, ex1, ex2) ->
    add_str (string_of_expr ex1);
    add_str (string_of_binop op);
    add_str (string_of_expr ex2);
    Buffer.contents buffer

let print_typespec ts =
  let str = string_of_typespec ts in
  Printf.printf "%s" str

let rec string_of_stmt stmt =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  let add_stmt stmt =
    add_str (string_of_stmt stmt);
    add_str "\n"
  in
  let rec print_formals formals =
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
      print_formals tl
  in
  match stmt with
  | StartStmt body ->
    add_str "\tSTART [\n";
    List.iter (fun stmt -> add_str "\t"; add_stmt stmt) body;
    add_str "\t]";
    Buffer.contents buffer
  
  | ArrayAssign (id, ind, ex) ->
    add_str id;
    add_str "[";
    add_str (string_of_int ind);
    add_str "]";
    add_str " = ";
    add_str (string_of_expr ex);
    add_str ";";
    Buffer.contents buffer
  
  | ArrayDef (ts, id, size) ->
    add_str (string_of_typespec ts);
    add_str " ";
    add_str id;
    add_str "[";
    add_str (string_of_int size);
    add_str "];";
    Buffer.contents buffer
  
  | PrintStmt expr ->
      add_str "printf(";
      add_str (string_of_expr expr);
      add_str ");";
      Buffer.contents buffer

  | IfStmt (con, body) ->
      add_str "if(";
      add_str (string_of_expr con);
      add_str "){\n";
      List.iter (fun stmt -> add_str "\t"; add_stmt stmt) body;
      add_str "\n}";
      Buffer.contents buffer
  | ElseIfStmt (con, body) ->
      add_str "else if(";
      add_str (string_of_expr con);
      add_str "){\n";
      List.iter (fun stmt -> add_str "\t"; add_stmt stmt) body;
      add_str "\n}";
      Buffer.contents buffer
  | ElseStmt (body) ->
      add_str "else{\n";
      List.iter (fun stmt -> add_str "\t"; add_stmt stmt) body;
      add_str "\n}";
      Buffer.contents buffer
  | Assign (id, ex) ->
      add_str id;
      add_str " = ";
      add_str (string_of_expr ex);
      add_str ";"; 
      Buffer.contents buffer
  | VarDef (ts, id, va) ->
      add_str (string_of_typespec ts);
      add_str " ";
      add_str id;
      add_str " = ";
      add_str (string_of_expr va);
      add_str ";"; 
      Buffer.contents buffer
  | ReturnStmt ex ->
      add_str "return"; 
      add_str " ";
      add_str (string_of_expr ex);
      add_str ";";
      Buffer.contents buffer
  | BreakStmt -> 
    add_str "break";
    add_str ";"; 
    Buffer.contents buffer
  | FuncDef (ts, id, args, body) ->
    add_str (string_of_typespec ts);
    add_str " ";
    add_str id;
    add_str "(";
    print_formals args;
    add_str "){\n";
    List.iter (fun stmt -> add_str "\t"; add_stmt stmt) body;
    add_str "\n}";
    Buffer.contents buffer

let add_imports =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  add_str "#include <stdio.h>\n"; Buffer.contents buffer

(*let extract_funcs program =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  let add_stmt stmt =
    add_str (string_of_stmt stmt);
    add_str "\n"
  in
  let rec print_formals formals =
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
      print_formals tl
  in
  match program with 
  | Main m ->
    List.iter (fun stmt -> 
      match stmt with
      | FuncDef (ts, id, args, body) ->
        add_str (string_of_typespec ts);
        add_str " ";
        add_str id;
        add_str "(";
        print_formals args;
        add_str "){\n";
        List.iter (fun stmt -> add_str "\t"; add_stmt stmt) body;
        add_str "\n}";
        Buffer.contents buffer
      | _ -> ()
    ) m*)

    let extract_funcs program =
      let buffer = Buffer.create 128 in
      let add_str s = Buffer.add_string buffer s in
      let add_stmt stmt =
        add_str "\n\t";
        add_str (string_of_stmt stmt)
      in
      match program with
      | Main stmts ->
        List.iter (fun stmt ->
          match stmt with
          | FuncDef _ -> add_stmt stmt
          | _ -> ()) stmts;
        Buffer.contents buffer

let string_of_program program =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  let add_stmt stmt =
    add_str "\n\t";
    add_str (string_of_stmt stmt)
  in
  (*  Compile and print libaries  *)
  add_str add_imports;
  add_str "\n";

  (*  Compile and print function declarations  *)
  add_str (extract_funcs program);
  (*List.iter (fun stmt -> add_str "\t"; add_stmt stmt) funcs;*)
 

  (*  Compile and print main  *)
  match program with
  | Main m ->
    add_str "int main(void){\n";
    List.iter (fun stmt -> 
      match stmt with 
      | FuncDef _ -> ()
      | _ -> add_str "\t"; add_stmt stmt) m;
    add_str "\nreturn 0;\n}";
    Buffer.contents buffer

    
let split_string str =
  Str.split (Str.regexp "\\.") str

let get_output_filename input_filename =
  let parts = split_string input_filename in
  match parts with
  | first :: _ -> "output_" ^ first ^ ".c"
  | _ -> failwith "File has no name."


let print_to_file filename prog =
  let prog_string = string_of_program prog in
  let output_filename = get_output_filename filename in
  let out_channel = open_out output_filename in
  output_string out_channel prog_string;
  close_out out_channel;;


