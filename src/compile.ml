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
  | Econst c -> add_str ("Econst(" ^ string_of_int c ^ ")"); Buffer.contents buffer
  | Efloat f -> add_str ("Efloat(" ^ string_of_float f ^ ")"); Buffer.contents buffer 
  | Evar v -> add_str ("Evar(" ^ v ^ ")"); Buffer.contents buffer 
  | Ebool b -> add_str ("Ebool(" ^ string_of_bool b ^ ")"); Buffer.contents buffer 
  | Efuncall (n, a) ->
      add_str ("Efuncall(name: " ^ n);
      add_str ", ";
      print_args a;
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

let print_typespec ts =
  let str = string_of_typespec ts in
  Printf.printf "%s" str

let rec string_of_stmt stmt =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  let add_stmt stmt =
    add_str (string_of_stmt stmt);
    add_str ";\n"
  in
  match stmt with
  | Sblock block ->
      add_str "\tSblock [\n";
      List.iter (fun stmt -> add_stmt stmt) block;
      add_str "\t]";
      Buffer.contents buffer
  | Sprint expr ->
      add_str "\tSprint: \"";
      add_str (string_of_expr expr);
      add_str "\"";
      Buffer.contents buffer
  | Sstart stmts ->
      add_str "\tSstart [\n";
      List.iter (fun stmt -> add_str "\t"; add_stmt stmt) stmts;
      add_str "\t]";
      Buffer.contents buffer
  | Sif (e, b) ->
      add_str "\tSif [\n";
      add_str "expr: "; add_str (string_of_expr e);
      add_str "then: ";
      List.iter (fun stmt -> add_stmt stmt) b;
      add_str "\t]";
      Buffer.contents buffer
  | Selseif (e, b) ->
      add_str "\tSelseif [\n";
      add_str "expr: "; add_str (string_of_expr e);
      add_str "then: ";
      List.iter (fun stmt -> add_stmt stmt) b;
      add_str "\t]";
      Buffer.contents buffer
  | Selse (b) ->
      add_str "\tSelse [\n";
      add_str "then: ";
      List.iter (fun stmt -> add_stmt stmt) b;
      add_str "\t]";
      Buffer.contents buffer
  | Sexpr expr ->
      add_str "\tSexpr: ";
      add_str (string_of_expr expr);
      Buffer.contents buffer
  | Sassign (id, e) ->
      add_str "\nassign: ";
      add_str "varname: "; add_str id;
      add_str "newValue: "; add_str (string_of_expr e);
      Buffer.contents buffer
  | Svardef (ts, id, v) ->
      add_str "\tvariable: type : "; add_str (string_of_typespec ts);
      add_str ", name : "; add_str id;
      add_str ", value : "; add_str (string_of_expr v);
      Buffer.contents buffer
  | Sreturn v ->
      add_str "\tReturn Statement: "; add_str (string_of_expr v);
      Buffer.contents buffer
  | Sbreak -> add_str "Break"; Buffer.contents buffer
  | Sfundef (ts, id, f, b) ->
      add_str "\tfunction: type : "; add_str (string_of_typespec ts);
      add_str ", name : "; add_str id;
      List.iter (fun (ts, id) ->
        add_str "\nargument: "; add_str (string_of_typespec ts); add_str ", Name: "; add_str id
      ) f;
      add_str "\nbody : ";
      List.iter (fun stmt -> add_stmt stmt) b;
      Buffer.contents buffer
  

let string_of_program program =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  let add_stmt stmt =
    add_str "\n\t";
    add_str (string_of_stmt stmt)
  in
  add_str "{\n\tdefs = [";
  (*List.iter (fun (name, expr) -> add_str (Printf.sprintf "(%s, %s);" name (string_of_expr expr))) program.defs;*)
  add_str "\n\t];\n\tmain = ";
  add_stmt program.main;
  add_str "\n}\n";
  Buffer.contents buffer


let print_to_file prog =
  let prog_string = string_of_program prog in
  let filename = "output.txt" in
  let out_channel = open_out filename in
  output_string out_channel prog_string;
  close_out out_channel;;


