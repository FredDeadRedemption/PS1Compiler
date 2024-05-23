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
  | ArrayAccess (name, index) ->
    "ArrayAccess(name: "^ name ^" index:"^ string_of_int index ^")"  
  | ConstInt i ->
    string_of_int i
  | ConstFloat f ->
    string_of_float f
  | Var id ->
    id
  | VarChain props ->
    let propstring = String.concat "." props in 
    propstring
  | VarAddress id ->
    "&"^id
  | Bool b ->
    int_of_bool b
  | UnaryOp (op, ex) ->
    let opStr = string_of_unop op in
    opStr ^ generate_expr ex
  | BinaryOp (op, ex1, ex2) ->
    let opStr = string_of_binop op in
    generate_expr ex1 ^ opStr ^ generate_expr ex2
  | AssignToStructExpr(id, expr) ->
    id ^ "." ^ generate_expr expr
  | FuncCallExpr (name, args) ->
    let args_str = String.concat ", " (List.map generate_expr args) in
    name ^ "(" ^ args_str ^ ")"


let rec generate_stmt stmt =
  match stmt with
  | VarDefI (ts, id, expr) ->
    (string_of_typespec ts) ^ " " ^ id ^ " = " ^ generate_expr expr ^ ";\n"
  | VarDefU (ts, id) ->
    (string_of_typespec ts) ^ " " ^ id ^ ";\n"
  | StructInit (ts, id) ->
    (string_of_typespec ts) ^ " " ^ id ^ ";\n"
  | AssignStructInit (ts) ->
    " = initialize" ^ (string_of_typespec ts) ^ "();\n"
  | ArrayDef (ts, id, size) ->
    (string_of_typespec ts) ^ id ^ "[" ^ string_of_int size ^ "];\n"
  | ArrayAssign (id, index, expr) ->
    id ^ "[" ^ string_of_int index ^"] = " ^ generate_expr expr ^ ";\n"
  | Assign (name, expr) ->
    name ^ " = " ^ generate_expr expr ^ ";\n"
  | ForStmt (stmt, cond, incr, block) ->
    "for (" ^
    generate_stmt stmt ^
    generate_expr cond ^ 
    "; " ^
    generate_stmt incr ^
    ") { \n" ^ 
    String.concat "\n" (List.map generate_stmt block) ^ 
    "}\n"
  | ObjectPropAssign (name, props, expr) ->
    name ^ "." ^ String.concat "." props ^ " = " ^ generate_expr expr ^ ";"
  | IfStmt (cond, block) ->
    "if (" ^
    generate_expr cond ^
    ") {\n" ^
    String.concat "\n" (List.map generate_stmt block) ^
    "}\n"
  | ElseIfStmt (cond, block) ->
    "else if (" ^
    generate_expr cond ^
    ") {\n" ^
    String.concat "\n" (List.map generate_stmt block) ^
    "}\n"
  | ElseStmt block ->
    "else {\n" ^
    String.concat "\n" (List.map generate_stmt block) ^
    "}\n"
  | WhileStmt (cond, block) ->
    "while (" ^
    generate_expr cond ^
    ") {\n" ^
    String.concat "\n" (List.map generate_stmt block) ^
    "} \n"
  | ReturnStmt expr ->
    "return " ^
    generate_expr expr ^
    ";\n" 
  | BreakStmt ->
    "break;\n"
  | ContinueStmt ->
    "continue;\n"
  | AssignToStructStmt (id, stmt) ->
    let dot_opt = match stmt with
      | AssignStructInit _ -> "" (*We don't want a dot for this stmt*)
      | _ -> "."
    in
    id ^ dot_opt ^ generate_stmt stmt
  | FuncCallStmt (name, args) ->
    let args_str = String.concat ", " (List.map generate_expr args) in
    name ^ "(" ^ args_str ^ ");"
  | Increment id -> 
    id ^ "++;\n"
  | Decrement id -> 
    id ^ "--;\n"
  | IncrementPre id -> 
    "++" ^ id ^ ";\n"
  | DecrementPre id -> 
    "--" ^ id ^ ";\n"
  | IncrementVal (id, expr) ->
    id ^ "+=" ^ generate_expr expr ^ ";\n"
  | DecrementVal (id, expr) ->
    id ^ "-=" ^ generate_expr expr ^ ";\n"


let generate_arg (ts, id) =
  (string_of_typespec ts) ^" "^ id 
  

let generate_ptype = function
  | FuncProto (ts, name, formals) -> 
    let formals_code = String.concat ", " (List.map generate_arg formals) in
    (string_of_typespec ts) ^ " " ^ name ^ "(" ^ formals_code ^ ")" ^ ";"

let generate_ptypes ptypes = 
  let ptype_code = String.concat "\n" (List.map generate_ptype ptypes) in
  ptype_code


let generate_structs = function
  | StructDef (name, fields) ->
    let field_str = String.concat "\n" (List.map generate_stmt fields) in 
    "typedef struct " ^ name ^  "{\n" ^
    field_str ^
    "}" ^ name ^ ";\n"

  let generate_constructs = function
  | Constructor (ts, id, stmts, return_stmt) ->
    let ts_str = string_of_typespec ts in 
    let stmt_str = String.concat ("\nvar" ^ ts_str ^ ".")  (List.map generate_stmt stmts) in 
    ts_str ^ " " ^ id ^  "() {\n" ^
    stmt_str ^
    generate_stmt return_stmt ^
    "}\n"

let generate_funcs = function
  | FuncDef (ts, name, formals, body) ->
    let args_str = String.concat ", " (List.map generate_arg formals) in
    let body_stmts_str = List.map generate_stmt body in
    let body_str = String.concat "\n" body_stmts_str in
    (string_of_typespec ts) ^ " " ^ name ^
    " (" ^ args_str ^ ") " ^
    "{\n" ^
    body_str ^
    "\n}\n"

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
  let (structs, ptypes, constructors, funcs, main) = program in
  let psx_code = Psx_imports.generate_psx_code in
  let structs_code = String.concat "\n\n" (List.map generate_structs structs) in
  let ptypes_code = generate_ptypes ptypes in
  let constructs_code = String.concat "\n\n" (List.map generate_constructs constructors) in
  let funcs_code = String.concat "\n\n" (List.map generate_funcs funcs) in
  let main_code = generate_main main in

  String.concat "\n\n" [psx_code; structs_code; ptypes_code; constructs_code; funcs_code; main_code]



  let split_string str =
    Str.split (Str.regexp "/") str
  
  
  let get_output_filename input_filename =
    let parts = split_string input_filename in
    let filename = List.hd (List.rev parts) in
    "../../../output_" ^ filename ^ ".c"

  let print_to_file filename prog =
    let prog_string = generate_program prog in
    print_endline ("Input balls filename: " ^ filename); (* Print the output filename *)
    let output_filename = get_output_filename filename in
    print_endline ("Output mah balls filename: " ^ output_filename); (* Print the output filename *)
    let out_channel = open_out output_filename in
    output_string out_channel prog_string;
    close_out out_channel;;
  

