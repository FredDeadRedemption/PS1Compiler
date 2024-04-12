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
  | Econst c -> add_str (string_of_int c); Buffer.contents buffer
  | Efloat f -> add_str (string_of_float f); Buffer.contents buffer 
  | Evar v -> add_str (v); Buffer.contents buffer 
  | Ebool b -> add_str (string_of_bool b); Buffer.contents buffer 
  | Efuncall (n, a) ->
      add_str (n^" (");
      
      print_args a;
      add_str ", ";
      add_str ")"; Buffer.contents buffer 
  | Eunop (o, e) ->
      let opii = string_of_unop o in
      add_str ("Eunop(" ^ opii ^ ", ");
      add_str (string_of_expr e);
      add_str ")"; Buffer.contents buffer
  | Ebinop (op, e1, e2) ->
      let opii = string_of_binop op in
      add_str ("Ebinop(" ^ opii ^ ", ");
      add_str (string_of_expr e1);
      add_str ", ";
      add_str (string_of_expr e2);
      add_str ")"; Buffer.contents buffer

let generate_typespec ts =
  match ts with
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"

(* For print statments*)
let string_of_print_expr expr =
  let buffer = Buffer.create 128 in 
  let add_str s = Buffer.add_string buffer s in
  match expr with
  | Econst c -> add_str ("\"%d\", " ^ string_of_int c); Buffer.contents buffer
  | _ -> failwith "This expression is not implemented"


let rec string_of_stmt stmt =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  let add_stmt stmt =
    add_str (string_of_stmt stmt);
    add_str "\n" 
  in
  match stmt with
  | Sblock block ->
    add_str " {\n";
    List.iter (fun stmt -> add_stmt stmt) block;
    add_str "\n\t}";
    Buffer.contents buffer
  | Sprint expr ->
      add_str "\tprintf(";
      add_str (string_of_print_expr expr);
      add_str ")"; add_str ";";
      Buffer.contents buffer
  | Sstart stmts ->
      add_str "\tSstart [\n";
      List.iter (fun stmt -> add_str "\t"; add_stmt stmt) stmts;
      add_str "\t]";
      Buffer.contents buffer
  | Sif (e, b) ->
      add_str "\tif (";
      add_str (string_of_expr e);
      add_str ")";
      add_str (string_of_stmt (Sblock b));
      Buffer.contents buffer
  | Selseif (e, b) ->
      add_str "\telse if (";
      add_str (string_of_expr e);
      add_str ")";
      add_str (string_of_stmt (Sblock b));
      Buffer.contents buffer
  | Selse (b) ->
      add_str "\telse";
      add_str (string_of_stmt (Sblock b));
      Buffer.contents buffer
  | Sexpr expr ->
      add_str "\tSexpr: ";
      add_str (string_of_expr expr);
      Buffer.contents buffer
  | Sassign (id, e) ->
      add_str id;
      add_str " = ";
      add_str (string_of_expr e); add_str ";";
      Buffer.contents buffer
  | Svardef (ts, id, v) ->
      add_str (generate_typespec ts); add_str " ";
      add_str id;
      add_str " = "; add_str (string_of_expr v); add_str ";";
      Buffer.contents buffer
  | Sreturn v ->
      add_str "\treturn "; add_str (string_of_expr v); add_str ";";
      Buffer.contents buffer
  | Sbreak -> add_str "break;"; Buffer.contents buffer
  | Sfundef (ts, id, f, b) ->
      add_str "\t"; add_str (generate_typespec ts); add_str " ";
      add_str id; 
      add_str "(";
      List.iter (fun (ts, id) ->
        add_str (generate_typespec ts); add_str " "; add_str id
      ) f;
      add_str ")";
      add_str (string_of_stmt (Sblock b));
      Buffer.contents buffer




let string_of_program program =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  let add_stmt stmt =
    add_str "\n\t";
    add_str (string_of_stmt stmt)
  in
  (*add_str "{\n\tdefs = [";*)
  (*List.iter (fun (name, expr) -> add_str (Printf.sprintf "(%s, %s);" name (string_of_expr expr))) program.defs;*)
  add_str Libary_importer.import;
  add_str "\nint main(void) ";
  add_stmt program.main;
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


