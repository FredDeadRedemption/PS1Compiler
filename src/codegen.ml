open Ctree



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

let generate_funcs = function
  | FuncDef _ -> "funcs"
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


