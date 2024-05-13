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
  | Var id ->
    id
  | Bool b ->
    int_of_bool b
  | UnaryOp (op, ex) ->
    let opStr = string_of_unop op in
    opStr ^ generate_expr ex
  | BinaryOp (op, ex1, ex2) ->
    let opStr = string_of_binop op in
    generate_expr ex1 ^ opStr ^ generate_expr ex2



let rec generate_stmt stmt =
  match stmt with
  | VarDef (ts, id, expr) ->
    (string_of_typespec ts) ^ " " ^ id ^ " " ^ generate_expr expr ^ ";"
  | ArrayDef (ts, id, size) ->
    (string_of_typespec ts) ^ id ^ "[" ^ string_of_int size ^ "];"
  | ArrayAssign (id, index, expr) ->
    id ^ "[" ^ string_of_int index ^"] = " ^ generate_expr expr ^ ";"
  | Assign (name, expr) ->
    name ^ " = " ^ generate_expr expr ^ ";"
  | IfStmt (cond, block) ->
    "if (" ^
    generate_expr cond ^
    ") {\n" ^
    String.concat "\n" (List.map generate_stmt block) ^
    "}"
  | ElseIfStmt (cond, block) ->
    "else if (" ^
    generate_expr cond ^
    ") {\n" ^
    String.concat "\n" (List.map generate_stmt block) ^
    "}"
  | ElseStmt block ->
    "else {\n" ^
    String.concat "\n" (List.map generate_stmt block) ^
    "}"
  | ReturnStmt expr ->
    "return " ^
    generate_expr expr ^
    ";\n" 
  | BreakStmt ->
    "break;"
  
let generate_arg (ts, id) =
  (string_of_typespec ts) ^" "^ id 
  

let generate_ptype = function
  | StructProto name -> "struct " ^ name ^ ";"
  | FuncProto (ts, name, formals) -> 
    let formals_code = String.concat ", " (List.map generate_arg formals) in
    (string_of_typespec ts) ^ " " ^ name ^ "(" ^ formals_code ^ ")" ^ ";"

let generate_ptypes ptypes = 
  let ptype_code = String.concat "\n" (List.map generate_ptype ptypes) in
  ptype_code


let generate_structs = function
  | StructDef (name, fields) ->
    let field_str = String.concat "\n" (List.map generate_stmt fields) in 
    "struct " ^ name ^  "{" ^
    field_str ^
    "}"


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

let generate_main (start, update) =
  match start with
      | Start start_block ->
        let start_code = String.concat "\n" (List.map generate_stmt start_block) in
  match update with
  | Update update_block ->
      let update_code = String.concat "\n" (List.map generate_stmt update_block) in
  
  (* Return string*)
  "int main(void) {\n" ^
  start_code ^
  "\n" ^
  "while(1) {\n" ^
  update_code ^
  "\n}\n}"
 

let generate_program program =
  let (ptypes, structs, funcs, main) = program in
  let ptypes_code = generate_ptypes ptypes in
  let structs_code = String.concat "\n\n" (List.map generate_structs structs) in
  let funcs_code = String.concat "\n\n" (List.map generate_funcs funcs) in
  let main_code = generate_main main in

  String.concat "\n\n" [ptypes_code; structs_code; funcs_code; main_code]




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


