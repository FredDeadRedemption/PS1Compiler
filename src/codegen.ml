open Ctree


let string_of_typespec = function
  | Int -> "int"
  | Float -> "float"
  | Bool -> "int"
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

let int_of_bool = function 
| true -> "1"
| false -> "0"


let rec generate_expr expr =
  match expr with
  | ParenExpr ex ->
    "(" ^ (generate_expr ex) ^ ")"
  | FuncCall (name, args) ->
    let args_str = String.concat ", " (List.map generate_expr args) in
    name ^ " (" ^ args_str ^ ")"
  | ArrayAccess (name, index) ->
    "ArrayAccess(name: "^ name ^" index:"^ string_of_int index ^")"  
  | ConstInt i ->
    string_of_int i
  | ConstFloat f ->
    string_of_float f
  | Var _ ->
    "?"
  | Bool b ->
    int_of_bool b
  | UnaryOp (op, ex) ->
    let opStr = string_of_unop op in
    opStr ^ generate_expr ex
  | BinaryOp (op, ex1, ex2) ->
    let opStr = string_of_binop op in
    generate_expr ex1 ^ opStr ^ generate_expr ex2



let rec generate_stmt stmt =
  let generate_arg (ts, id) =
    (string_of_typespec ts) ^" "^ id 
  in
  match stmt with
  | VarDef (ts, id, expr) ->
    (string_of_typespec ts) ^ " " ^ id ^ " " ^ generate_expr expr ^ ";"
    
  (*
  | ArrayDef (ts, id, size) ->
    printf "\nArrayDef{type: %s, id: %s, size: %d}" (string_of_typespec ts) id size
  | ArrayAssign (id, index, expr) ->
    printf "\nArrayAssign{id: %s, index: %d, value: " id index;
    print_expr expr;
    printf "}"
  *)
  | FuncDef (ts, id, formals, body) ->
    let args_str = String.concat "," (List.map generate_arg formals) in
    let body_stmts_str = List.map generate_stmt body in
    let body_str = String.concat "\n" body_stmts_str in
    (string_of_typespec ts) ^ " " ^ id ^
    " (" ^ args_str ^ ")" ^
    "{" ^
    body_str ^
    "}"
  (*

  | FuncProto (ts, name, formals) ->
    printf "\nFuncProto{type: %s, name: %s, formals: [" (string_of_typespec ts) name;
    List.iter (fun (ftype, fname) -> 
      printf "(type: %s, name: %s) " (string_of_typespec ftype) fname
    ) formals;
    printf "]}"
  | Assign (name, expr) ->
    printf "\nAssign{name: %s, value: " name;
    print_expr expr;
    printf "}"
  | PrintStmt expr ->
    printf "\nPrintStmt{";
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
  | StructDef (name, fields) ->
    printf "\nStructDef{name: %s" name;
    printf ", fields:";
    List.iter(print_stmt )fields;
    printf "\n}"
  | StructProto name ->
    printf "\nStructProto{name: %s}" name
  | Update block ->
    printf "\nUpdate{";
    List.iter print_stmt block;
    printf "}"
*)
    | _ -> ""

let generate_ptypes (structs, funcs) = 
  let struct_code = List.map (function
    | StructProto _ -> "sp" 
    | _ -> "OY"
  ) structs in
  let func_code = List.map (function
    | FuncProto _ -> "fp" 
    | _ -> "YO"
  ) funcs in
  String.concat "\n" (struct_code @ func_code)

  
  

let generate_structs = function
  | StructDef _ -> "struct"
  | _ -> ""

let generate_arg (ts, id) =
  (string_of_typespec ts) ^" "^ id 

let generate_funcs = function
  
  | FuncDef (ts, name, formals, body) ->
    let args_str = String.concat ", " (List.map generate_arg formals) in
    let body_stmts_str = List.map generate_stmt body in
    let body_str = String.concat "\n" body_stmts_str in

    
    (string_of_typespec ts) ^ " " ^ name ^
    " (" ^ args_str ^ ") " ^
    "{\n" ^
    body_str ^
    "\n}"
  | _ -> ""

(*let generate_main = function
  "main"*)

let generate_program program =
  let (ptypes, structs, funcs, _) = program in
  let ptypes_code = generate_ptypes ptypes in
  let structs_code = String.concat "\n\n" (List.map generate_structs structs) in
  let funcs_code = String.concat "\n\n" (List.map generate_funcs funcs) in
  (*let main_code = generate_main main in*)

  String.concat "\n\n" [ptypes_code; structs_code; funcs_code(*; main_code*)]




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


