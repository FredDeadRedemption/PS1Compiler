open Ast

let collect_components id (fields, start_block, update_block, methods) field_list start_list update_list method_list =
  
  field_list := List.rev_append fields !field_list;
  
  start_list := List.rev_append start_block !start_list;
  
  update_list := List.rev_append update_block !update_list;
  
  (* Add methods to the global method list *)
  method_list := List.rev_append methods !method_list

let construct_program = 


let create_tree (gameClass, classes) = 
  match gameClass with
  | ClassStmt _ ->
    Printf.printf "Game\n";

  (* Initialize lists to accumulate results*) (* TODO: Skal måske også bruges i gameClass *)
  let field_list = ref [] in
  let start_list = ref [] in
  let update_list = ref [] in
  let method_list = ref [] in

  List.iter (fun clas ->
    match clas with
    | ClassStmt (id, (fl, StartDef bl, UpdateDef ul, ml)) -> 
      collect_components id (fl, bl, ul, ml) field_list start_list update_list method_list
  ) classes

  



let format_to_c ast = 
  let formatted_tree = create_tree ast in
  formatted_tree