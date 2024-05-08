open Ast

let assemble_struct id fields =
  StructDef(id, fields)


let collect_components id (fields, start_block, update_block, methods) id_list struct_list field_list start_list update_list method_list =

  id_list := id :: ! id_list;

  struct_list := assemble_struct id fields :: ! struct_list;
  
  field_list := List.rev_append fields !field_list;
  
  start_list := List.rev_append start_block !start_list;
  
  update_list := List.rev_append update_block !update_list;
  
  (* Add methods to the global method list *)
  method_list := List.rev_append methods !method_list



let rec generate_struct_pt id_list =
  match id_list with
    | [] -> () 
    | [id] -> 
      Ctree.StructProto(id)
    | (id) :: tl ->
      Ctree.StructProto(id);
      generate_struct_pt tl
      
let rec generate_func_pt method_list =
    match method_list with
    | [] -> () 
    | [(ts, id, formals, _)] -> 
      Ctree.FuncProto(ts, id, formals)
    | (ts, id, formals, _) :: tl ->
      Ctree.FuncProto(ts, id, formals);
      generate_func_pt tl
      
let construct_ptypes id_list method_list =
  let func_pt = generate_func_pt method_list in
  let struct_pt = generate_struct_pt id_list in
  (func_pt, struct_pt)

let rec construct_funcs method_list =
  match method_list with
  | [] -> () 
  | [(ts, id, formals, body)] -> 
    Ctree.FuncDef(ts, id, formals, body)
  | (ts, id, formals, body) :: tl ->
    Ctree.FuncDef(ts, id, formals, body);
    generate_func_pt tl

let rec construct_update update_list =
  Ctree.Update(update_list)

let construct_main start_list update_list =
  let start = start_list in 
  let update = construct_update update_list in
  (start, update)

let construct_program id_list struct_list field_list start_list update_list method_list = 
  let ptypes = construct_ptypes id_list method_list  in
  let funcs = construct_funcs method_list in
  let structs = struct_list in
  let main = construct_main start_list update_list in
  (ptypes, funcs, structs, main)


let create_tree (gameClass, classes) = 
  match gameClass with
  | ClassStmt _ ->
    Printf.printf "Game\n";

  (* Initialize lists to accumulate results*) (* TODO: Skal måske også bruges i gameClass *)
  let id_list     = ref [] in
  let struct_list = ref [] in
  let field_list  = ref [] in
  let start_list  = ref [] in
  let update_list = ref [] in
  let method_list = ref [] in

  List.iter (fun clas ->
    match clas with
    | ClassStmt (id, (fl, StartDef sb, UpdateDef ub, ml)) -> 
      collect_components id (fl, sb, ub, ml) id_list struct_list field_list start_list update_list method_list
  ) classes;

  let ctree = construct_program id_list struct_list field_list start_list update_list method_list in
  ctree


let format_to_c ast = 
  let formatted_tree = create_tree ast in
  formatted_tree