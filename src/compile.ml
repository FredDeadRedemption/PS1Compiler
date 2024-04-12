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
    add_str (string_of_bool b); 
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
  match stmt with
  | StartStmt body ->
    add_str "\tSTART [\n";
    add_block body;
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
      add_str "\tSprint: \"";
      add_str (string_of_expr expr);
      add_str "\"";
      Buffer.contents buffer

  | IfStmt (e, b) ->
      add_str "\tSif [\n";
      add_str "expr: "; add_str (string_of_expr e);
      add_str "then: ";
      List.iter (fun stmt -> add_stmt stmt) b;
      add_str "\t]";
      Buffer.contents buffer
  | ElseIfStmt (e, b) ->
      add_str "\tSelseif [\n";
      add_str "expr: "; add_str (string_of_expr e);
      add_str "then: ";
      List.iter (fun stmt -> add_stmt stmt) b;
      add_str "\t]";
      Buffer.contents buffer
  | ElseStmt (b) ->
      add_str "\tSelse [\n";
      add_str "then: ";
      List.iter (fun stmt -> add_stmt stmt) b;
      add_str "\t]";
      Buffer.contents buffer
  | Assign (id, e) ->
      add_str "\nassign: ";
      add_str "varname: "; add_str id;
      add_str "newValue: "; add_str (string_of_expr e);
      Buffer.contents buffer
  | VarDef (ts, id, v) ->
      add_str "\tvariable: type : "; add_str (string_of_typespec ts);
      add_str ", name : "; add_str id;
      add_str ", value : "; add_str (string_of_expr v);
      Buffer.contents buffer
  | ReturnStmt v ->
      add_str "\tReturn Statement: "; add_str (string_of_expr v);
      Buffer.contents buffer
  | BreakStmt -> 
    add_str "break"; 
    Buffer.contents buffer
  | FuncDef (ts, id, f, b) ->
      add_str "\tfunction: type : "; add_str (string_of_typespec ts);
      add_str ", name : "; add_str id;
      List.iter (fun (ts, id) ->
        add_str "\nargument: "; add_str (string_of_typespec ts); add_str ", Name: "; add_str id
      ) f;
      add_str "\nbody : ";
      List.iter (fun stmt -> add_stmt stmt) b;
      Buffer.contents buffer
and add_block block =
  let buffer = Buffer.create 128 in 
  let add_str s = Buffer.add_string buffer s in
  let add_stmt stmt =
    add_str "\n\t";
    add_str (string_of_stmt stmt)
  in
  List.iter (fun stmt -> add_str "\t"; add_stmt stmt) block    

let string_of_program program =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  let add_stmt stmt =
    add_str "\n\t";
    add_str (string_of_stmt stmt)
  in
  match program with
  | Main m ->
    List.iter (fun stmt -> add_str "\t"; add_stmt stmt) m;
    Buffer.contents buffer
  | Defs x ->
    List.iter (fun stmt -> add_str "\t"; add_stmt stmt) x;
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


