(* made global *)
let inst_classes_list = ref []
let class_list = ref []
let id_list = ref []
let struct_list = ref []
let construct_list = ref []
let start_list = ref []
let update_list = ref []
let method_list = ref []

let convert_ast_binop_to_ctree_binop (op : Ast.binop) : Ctree.binop =
  match op with
  | Ast.BinopAdd -> Ctree.BinopAdd
  | Ast.BinopSub -> Ctree.BinopSub
  | Ast.BinopMul -> Ctree.BinopMul
  | Ast.BinopDiv -> Ctree.BinopDiv
  | Ast.BinopMod -> Ctree.BinopMod
  | Ast.BinopAnd -> Ctree.BinopAnd
  | Ast.BinopOr -> Ctree.BinopOr
  | Ast.BinopLessThan -> Ctree.BinopLessThan
  | Ast.BinopGreaterThan -> Ctree.BinopGreaterThan
  | Ast.BinopLessThanEq -> Ctree.BinopLessThanEq
  | Ast.BinopGreaterThanEq -> Ctree.BinopGreaterThanEq
  | Ast.BinopEq -> Ctree.BinopEq
  | Ast.BinopNotEq -> Ctree.BinopNotEq

let convert_ast_unop_to_ctree_unop (op : Ast.unop) : Ctree.unop =
  match op with
  | Ast.UnopNot -> Ctree.UnopNot
  | Ast.UnopNeg -> Ctree.UnopNeg

let rec convert_ast_expr_to_ctree_expr (expr : Ast.expr) : Ctree.expr =
  match expr with
  | Ast.ParenExpr inner -> Ctree.ParenExpr (convert_ast_expr_to_ctree_expr inner)
  | Ast.FuncCall (name, params) -> Ctree.FuncCall (name, List.map convert_ast_expr_to_ctree_expr params)
  | Ast.ArrayAccess (name, index) -> Ctree.ArrayAccess (name, index)
  | Ast.ConstInt value -> Ctree.ConstInt value
  | Ast.ConstFloat value ->
    let int_value = int_of_float (value *. 4096.0) in
    Ctree.ConstInt int_value
  | Ast.Var name -> Ctree.Var name
  | Ast.Bool value -> Ctree.Bool value
  | Ast.UnaryOp (op, expr) ->
    let converted_expr = convert_ast_expr_to_ctree_expr expr in
    let converted_op = convert_ast_unop_to_ctree_unop op in
    Ctree.UnaryOp (converted_op, converted_expr)
  | Ast.BinaryOp (op, lhs, rhs) ->
    let converted_lhs = convert_ast_expr_to_ctree_expr lhs in
    let converted_rhs = convert_ast_expr_to_ctree_expr rhs in
    let converted_op = convert_ast_binop_to_ctree_binop op in
    Ctree.BinaryOp (converted_op, converted_lhs, converted_rhs)

let convert_ast_typespec_to_ctree_typespec (ts : Ast.typespec) : Ctree.typespec =
  match ts with
  | Ast.Int -> Ctree.Int
  | Ast.Float -> Ctree.Int
  | Ast.Bool -> Ctree.Bool
  | Ast.Void -> Ctree.Void
  | Ast.Generic s -> Ctree.Generic s

let rec convert_ast_stmt_to_ctree_stmt (stmt : Ast.stmt) : Ctree.stmt =
  match stmt with
  | Ast.VarDefI (ts, name, expr) ->
    let converted_ts = convert_ast_typespec_to_ctree_typespec ts in
    let converted_expr = convert_ast_expr_to_ctree_expr expr in
    Ctree.VarDefI (converted_ts, name, converted_expr)
  | Ast.VarDefU (ts, name) ->
    let converted_ts = convert_ast_typespec_to_ctree_typespec ts in
    Ctree.VarDefU (converted_ts, name)
  | Ast.ArrayDef (ts, name, size) ->
    let converted_ts = convert_ast_typespec_to_ctree_typespec ts in
    Ctree.ArrayDef (converted_ts, name, size)
  | Ast.ArrayAssign (name, index, expr) ->
    let converted_expr = convert_ast_expr_to_ctree_expr expr in
    Ctree.ArrayAssign (name, index, converted_expr)
  | Ast.Assign (name, expr) ->
    let converted_expr = convert_ast_expr_to_ctree_expr expr in
    Ctree.Assign (name, converted_expr)
  | Ast.IfStmt (cond, block) ->
    let converted_cond = convert_ast_expr_to_ctree_expr cond in
    let converted_block = List.map convert_ast_stmt_to_ctree_stmt block in
    Ctree.IfStmt (converted_cond, converted_block)
  | Ast.ElseIfStmt (cond, block) ->
    let converted_cond = convert_ast_expr_to_ctree_expr cond in
    let converted_block = List.map convert_ast_stmt_to_ctree_stmt block in
    Ctree.ElseIfStmt (converted_cond, converted_block)
  | Ast.ElseStmt block ->
    let converted_block = List.map convert_ast_stmt_to_ctree_stmt block in
    Ctree.ElseStmt converted_block
  | Ast.ReturnStmt expr ->
    let converted_expr = convert_ast_expr_to_ctree_expr expr in
    Ctree.ReturnStmt converted_expr
  | Ast.BreakStmt -> Ctree.BreakStmt
  | Ast.ContinueStmt -> Ctree.ContinueStmt
  | Ast.ClassInit (ts, name) ->
    inst_classes_list := (ts, name) :: !inst_classes_list;
    let converted_ts = convert_ast_typespec_to_ctree_typespec ts in
    Ctree.StructInit (converted_ts, name)
  | Ast.Increment id -> Ctree.Increment id
  | Ast.Decrement id -> Ctree.Decrement id
  | Ast.IncrementPre id -> Ctree.IncrementPre id
  | Ast.IncrementVal (id, expr) -> Ctree.IncrementVal (id, convert_ast_expr_to_ctree_expr expr)
  | Ast.DecrementPre id -> Ctree.DecrementPre id
  | Ast.DecrementVal (id, expr) -> Ctree.DecrementVal (id, convert_ast_expr_to_ctree_expr expr)


let convert_ast_field_to_ctree field =
  match field with
  | Ast.FieldDefI (ts, name, e) ->
    let converted_ts = convert_ast_typespec_to_ctree_typespec ts in
    let converted_expr = convert_ast_expr_to_ctree_expr e in
    Ctree.VarDefI (converted_ts, name, converted_expr)
  | Ast.FieldDefU (ts, name) ->
    let converted_ts = convert_ast_typespec_to_ctree_typespec ts in
    Ctree.VarDefU (converted_ts, name)
  | Ast.FieldClsInit (ts, name) ->
    inst_classes_list := (ts, name) :: !inst_classes_list;
    let converted_ts = convert_ast_typespec_to_ctree_typespec ts in
    Ctree.StructInit (converted_ts, name)

let convert_ast_field_init_to_uinit field =
  match field with
  | Ast.FieldDefI (ts, name, _) -> Ast.FieldDefU (ts, name)
  | Ast.FieldDefU (ts, name) -> Ast.FieldDefU (ts, name)
  | Ast.FieldClsInit (ts, name) -> Ast.FieldDefU(ts, name)

let assemble_struct id fields =
  let converted_fields = List.map convert_ast_field_to_ctree (List.map convert_ast_field_init_to_uinit fields) in
  Ctree.StructDef (id, converted_fields)

let assemble_inher_struct id inher fields =
  let inher_field = Ast.FieldDefU (Ast.Generic inher, "gameobject") in
  let fields_with_inher = inher_field :: fields in
  let converted_fields = List.map convert_ast_field_to_ctree (List.map convert_ast_field_init_to_uinit fields_with_inher) in
  Ctree.StructDef (id, converted_fields)

let assemble_constructer id fields =
  let ts = Ctree.Generic id in
  let func_id = "initialize" ^ id in
  let initialized_fields = List.fold_right (fun field acc ->
    match field with
    | Ast.FieldDefI (_, id, e) -> Ast.Assign (id, e) :: acc
    | Ast.FieldDefU _ -> acc
    | Ast.FieldClsInit _ -> acc
  ) fields [] in
  let struct_field = Ast.VarDefU (Ast.Generic id, "var" ^ id) in
  let fields_with_struct = struct_field :: initialized_fields in
  let converted_constructor = List.map convert_ast_stmt_to_ctree_stmt fields_with_struct in
  let return_stmt = Ctree.ReturnStmt (Ctree.Var ("var" ^ id)) in
  Ctree.Constructor (ts, func_id, converted_constructor, return_stmt)

let convert_ast_method_to_ctree (Ast.MethodDef (ts, name, formals, body)) : (Ctree.typespec * string * Ctree.formals * Ctree.block) =
  let converted_ts = convert_ast_typespec_to_ctree_typespec ts in
  let converted_formals = List.map (fun (t, s) ->
    match t with
    | Ast.Int -> (Ctree.Int, s)
    | Ast.Float -> (Ctree.Float, s)
    | Ast.Bool -> (Ctree.Bool, s)
    | Ast.Void -> (Ctree.Void, s)
    | Ast.Generic st -> (Ctree.Generic st, s)
  ) formals in
  let converted_body = List.map convert_ast_stmt_to_ctree_stmt body in
  (converted_ts, name, converted_formals, converted_body)

let convert_ast_field_to_stmt field =
  match field with
  | Ast.FieldDefI (ts, id, e) -> Ast.VarDefI (ts, id, e)
  | Ast.FieldDefU (ts, id) -> Ast.VarDefU (ts, id)
  | Ast.FieldClsInit (ts, id) -> Ast.ClassInit (ts, id)

let collect_main_components (fields, start_block, update_block, methods) =
  let converted_fields = List.map convert_ast_stmt_to_ctree_stmt (List.map convert_ast_field_to_stmt fields) in
  let converted_start = List.map convert_ast_stmt_to_ctree_stmt start_block in
  start_list := !start_list @ converted_fields @ converted_start;
  let converted_update = List.map convert_ast_stmt_to_ctree_stmt update_block in
  update_list := !update_list @ converted_update;
  let converted_methods = List.map convert_ast_method_to_ctree methods in
  method_list := List.rev_append converted_methods !method_list

let collect_components id (fields, start_block, update_block, methods) =
  id_list := id :: !id_list;
  struct_list := assemble_struct id fields :: !struct_list;
  construct_list := assemble_constructer id fields :: !construct_list;
  let converted_start = List.map convert_ast_stmt_to_ctree_stmt start_block in
  start_list := !start_list @ converted_start;
  let converted_update = List.map convert_ast_stmt_to_ctree_stmt update_block in
  update_list := !update_list @ converted_update;
  let converted_methods = List.map convert_ast_method_to_ctree methods in
  method_list := List.rev_append converted_methods !method_list

let collect_components_inher id inher (fields, start_block, update_block, methods) =
  id_list := id :: !id_list;
  struct_list := assemble_inher_struct id inher fields :: !struct_list;
  construct_list := assemble_constructer id fields :: !construct_list;
  let converted_start = List.map convert_ast_stmt_to_ctree_stmt start_block in
  start_list := !start_list @ converted_start;
  let converted_update = List.map convert_ast_stmt_to_ctree_stmt update_block in
  update_list := !update_list @ converted_update;
  let converted_methods = List.map convert_ast_method_to_ctree methods in
  method_list := List.rev_append converted_methods !method_list

let collect_class_components id (fields, methods) =
  id_list := id :: !id_list;
  struct_list := assemble_struct id fields :: !struct_list;
  construct_list := assemble_constructer id fields :: !construct_list;
  let methods = List.map convert_ast_method_to_ctree methods in
  let methods_with_class = List.map (fun (ts, name, formals, body) ->
    let updated_formals = (Ctree.Generic id, "var" ^ id) :: formals in
    (ts, name, updated_formals, body)
  ) methods in
  method_list := List.rev_append methods_with_class !method_list

let collect_class_inher_components id inher (fields, methods) =
  id_list := id :: !id_list;
  struct_list := assemble_inher_struct id inher fields :: !struct_list;
  construct_list := assemble_constructer id fields :: !construct_list;
  let converted_methods = List.map convert_ast_method_to_ctree methods in
  method_list := List.rev_append converted_methods !method_list

let rec convert_ctree_stmt_to_struct_assignment ids stmt =
  match ids with 
  | [] -> stmt
  | id :: tl ->  
  Ctree.AssignToStruct (id, convert_ctree_stmt_to_struct_assignment tl stmt)


let collect_object_components parent_ids (current_ts, current_id) start_block update_block =
  let struct_inits =
    
    let lowest_child_class = Ctree.AssignStructInit ((convert_ast_typespec_to_ctree_typespec current_ts), current_id) in
    List.map (convert_ctree_stmt_to_struct_assignment parent_ids) [lowest_child_class]
  in
  let converted_start = List.map (convert_ctree_stmt_to_struct_assignment parent_ids) (List.map convert_ast_stmt_to_ctree_stmt start_block) in
  start_list := !start_list @ struct_inits @ converted_start;
  let converted_update = List.map (convert_ctree_stmt_to_struct_assignment parent_ids) (List.map convert_ast_stmt_to_ctree_stmt update_block) in
  update_list := !update_list @ converted_update

let rec instantiate_class (ts, ids) classes =
  let string_of_typespec = function
  | Ast.Int -> "int"
  | Ast.Float -> "float"
  | Ast.Bool -> "int"
  | Ast.Void -> "void"
  | Ast.Generic g -> g
  in
  let ts_str = string_of_typespec ts in
  let rec find_class_by_name name classes =
    match classes with
    | [] -> None
    | Ast.ClassStmt (class_name, class_block) :: rest ->
      if class_name = name then Some (Ast.ClassStmt (class_name, class_block))
      else find_class_by_name name rest
    | Ast.ClassInherStmt (class_name, inher, class_block) :: rest ->
      if class_name = name then Some (Ast.ClassInherStmt (class_name, inher, class_block))
      else find_class_by_name name rest
  in
  match find_class_by_name ts_str classes with
  | None -> failwith ("Class " ^ ts_str ^ " not found.")
  | Some class_def ->
    begin
      match class_def with
      | Ast.ClassStmt (class_name, (fields, StartDef start, UpdateDef update, _)) ->
        let new_ids = ids in
        let class_info_ref = ref None in 
        List.iter (function
          | Ast.FieldClsInit (ts, id) -> 
            instantiate_class (ts, new_ids @ [id]) classes;
            class_info_ref := Some (ts, id)
          | _ -> ()
        ) fields;
        (match !class_info_ref with
        | Some class_info -> collect_object_components new_ids class_info start update
        | None -> ());
        print_endline ("Found class: " ^ class_name ^ " with id " ^ (String.concat "." new_ids))
      | Ast.ClassInherStmt (class_name, inher, (_, StartDef _, UpdateDef _, _)) ->
        let new_ids = ids in
        (*collect_object_components new_ids start update;*)
        print_endline ("Found inherited class: " ^ class_name ^ " inheriting from " ^ inher ^ " with id " ^ (List.hd new_ids))
    end

let rec instantiate_classes inst_classes_list classes =
  match inst_classes_list with
  | [] -> ()
  | (ts, id) :: tl ->
    instantiate_class (ts, [id]) classes;
    instantiate_classes tl classes

let rec generate_func_pt method_list =
  match method_list with
  | [] -> []
  | (ts, id, formals, _) :: tl ->
    let current_func_proto = Ctree.FuncProto (ts, id, formals) in
    current_func_proto :: generate_func_pt tl

let rec generate_struct_pt id_list =
  match id_list with
  | [] -> []
  | id :: tl ->
    let current_struct = Ctree.StructProto (id) in
    current_struct :: generate_struct_pt tl

let construct_ptypes id_list method_list =
  let struct_pt = generate_struct_pt id_list in
  let func_pt = generate_func_pt method_list in
  (struct_pt @ func_pt)

let rec construct_funcs method_list =
  match method_list with
  | [] -> []
  | (ts, id, formals, body) :: tl ->
    let current_func_def = Ctree.FuncDef (ts, id, formals, body) in
    current_func_def :: construct_funcs tl

let construct_main start_list update_list =
  let start = Ctree.Start (start_list) in
  let update = Ctree.Update (update_list) in
  (start, update)

let construct_program id_list struct_list construct_list start_list update_list method_list =
  let ptypes = construct_ptypes id_list method_list in
  let structs = struct_list in
  let constructors = construct_list in
  let funcs = construct_funcs method_list in
  let main = construct_main start_list update_list in
  (ptypes, structs, constructors, funcs, main)

let create_tree (gameClass, classes) : Ctree.program =
  class_list := classes;
  begin
    match gameClass with
    | Ast.ClassStmt (_, (fl, StartDef sb, UpdateDef ub, ml)) ->
      collect_main_components (fl, sb, ub, ml)
    | Ast.ClassInherStmt (id, inher, (fl, StartDef sb, UpdateDef ub, ml)) ->
      collect_components_inher id inher (fl, sb, ub, ml)
  end;
  List.iter (fun clas ->
    match clas with
    | Ast.ClassStmt (id, (fl,_, _, ml)) ->
      collect_class_components id (fl , ml)
    | Ast.ClassInherStmt (id, inher, (fl, _, _, ml)) ->
      collect_class_inher_components id inher (fl, ml)
  ) classes;
  instantiate_classes !inst_classes_list classes;
  construct_program !id_list !struct_list !construct_list !start_list !update_list !method_list

let format_to_c ast =
  let formatted_tree = create_tree ast in
  formatted_tree
