(* made global *)
let inst_classes_list = ref []
let class_list = ref []
let id_list = ref []
let struct_list = ref []
let construct_list = ref []
let start_list = ref []
let struct_init_list = ref []
let game_field_list = ref []
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
  | Ast.ArrayAccess (name, index) -> Ctree.ArrayAccess (name, index)
  | Ast.ConstInt value -> Ctree.ConstInt value
  | Ast.ConstFloat value ->
    let int_value = int_of_float (value *. 4096.0) in
    Ctree.ConstInt int_value
  | Ast.Var name -> Ctree.Var name
  | Ast.VarChain (var, props) -> Ctree.VarChain(var :: props)
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
  | Ast.MethodCallExpr (ref_opt, mthd_id, args) ->
    let converted_args = List.map convert_ast_expr_to_ctree_expr args in
    begin
    match ref_opt with
    | None -> Ctree.FuncCallExpr (mthd_id, converted_args)
    | Some(id) -> 
      match id with
      | "this" -> Ctree.FuncCallExpr (mthd_id, converted_args)
      | "super" -> Ctree.FuncCallExpr (mthd_id, converted_args)
      | id -> Ctree.FuncCallExpr (mthd_id, Ctree.Var(id) :: converted_args)
    end;
  | Ast.StringExpr s -> Ctree.StringExpr s
    

    
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
  | Ast.ForStmt (stmt, cond, incr, block) ->
    let converted_stmt = convert_ast_stmt_to_ctree_stmt stmt in 
    let converted_cond = convert_ast_expr_to_ctree_expr cond in 
    let converted_incr = convert_ast_stmt_to_ctree_stmt incr in 
    let converted_block = List.map convert_ast_stmt_to_ctree_stmt block in 
    Ctree.ForStmt (converted_stmt, converted_cond, converted_incr, converted_block)
  | Ast.ObjectPropAssign (name, props, expr) -> 
    let converted_expr = convert_ast_expr_to_ctree_expr expr in 
    Ctree.ObjectPropAssign(name, props, converted_expr)
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
  | Ast.WhileStmt (cond, block) ->
    let converted_cond = convert_ast_expr_to_ctree_expr cond in
    let converted_block = List.map convert_ast_stmt_to_ctree_stmt block in 
    Ctree.WhileStmt (converted_cond, converted_block)
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
  | Ast.MethodCallStmt (ref_opt, mthd_id, args) ->
    let converted_args = List.map convert_ast_expr_to_ctree_expr args in
    
    match ref_opt with
    | None -> Ctree.FuncCallStmt (mthd_id, converted_args)
    | Some(id) -> 
      match id with
      | "this"
      | "super" -> Ctree.FuncCallStmt (mthd_id, converted_args)
      | id -> Ctree.FuncCallStmt (mthd_id, Ctree.Var("&" ^ id) :: converted_args)
    
    
    
    

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

let decapitalize str =
  if str = "" then
    ""
  else
    let first_char = String.get str 0 in
    let rest_str = String.sub str 1 (String.length str - 1) in
    let decapitalized_first_char = Char.lowercase_ascii first_char in
    String.make 1 decapitalized_first_char ^ rest_str

let assemble_struct id inher_opt fields =
  let updated_fields = match inher_opt with
    | Some inher -> 
      let inher_field = Ast.FieldDefU (Ast.Generic inher, decapitalize inher) in
      inher_field :: fields
    | None -> fields
  in
  let converted_fields = List.map convert_ast_field_to_ctree (List.map convert_ast_field_init_to_uinit updated_fields) in
  Ctree.StructDef (id, converted_fields)



let generate_empty_game_object inher =
  let gameObject = decapitalize inher in
  [
    Ctree.Assign (gameObject ^ ".x", ConstInt(0));
    Ctree.Assign (gameObject ^ ".y", ConstInt(0));
    Ctree.Assign (gameObject ^ ".width", ConstInt(0));
    Ctree.Assign (gameObject ^ ".height", ConstInt(0));
    Ctree.Assign (gameObject ^ ".color", Var("WHITE"))
  ]


let assemble_constructor id inher_opt fields =
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
  let handle_inher = match inher_opt with
  | None -> converted_constructor
  | Some(inher) -> converted_constructor @ (generate_empty_game_object inher)
  in
  let return_stmt = Ctree.ReturnStmt (Ctree.Var ("var" ^ id)) in
  Ctree.Constructor (ts, func_id, handle_inher, return_stmt)

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


let rec handle_main_expr expr = 
  match expr with
  | Ctree.VarChain props -> 
    let updated_props = List.map (fun prop -> 
      match prop with
      | "super" -> "gameObject"
      | other -> other
    ) props in
    Ctree.VarChain updated_props
  | Ctree.FuncCallExpr (mthd_id, args) -> 
    let handled_args = List.map handle_main_expr args in
    Ctree.FuncCallExpr (mthd_id, handled_args)
  | Ctree.BinaryOp (binop, e1, e2) -> Ctree.BinaryOp (binop, handle_main_expr e1, handle_main_expr e2)
  | _ -> expr

let rec handle_main_stmt stmt =
  match stmt with
  | Ctree.ObjectPropAssign (id, props, e) -> 
    let props_with_gameobject = List.map (fun prop -> 
      match prop with 
      | "super" -> "gameObject"
      | other -> other
    ) props in
    Ctree.ObjectPropAssign (id, props_with_gameobject, handle_main_expr e)
  | Ctree.IfStmt (e, blk) ->  Ctree.IfStmt (handle_main_expr e, List.map handle_main_stmt blk)
  | _ -> stmt

let collect_main_components (fields, start_block, update_block, methods) =
  let converted_fields = List.map convert_ast_stmt_to_ctree_stmt (List.map convert_ast_field_to_stmt fields) in
  game_field_list := converted_fields @ !game_field_list;
  let converted_start = List.map convert_ast_stmt_to_ctree_stmt start_block in
  let handled_start = List.map handle_main_stmt converted_start in
  start_list := !start_list @ handled_start;
  let converted_update = List.map convert_ast_stmt_to_ctree_stmt update_block in
  let handled_update = List.map handle_main_stmt converted_update in
  update_list := !update_list @ handled_update;
  let converted_methods = List.map convert_ast_method_to_ctree methods in
  method_list := List.rev_append converted_methods !method_list



let is_field fields id =
  List.exists (fun field -> 
    match field with
    | Ast.FieldDefI (_, field_name, _)
    | Ast.FieldDefU (_, field_name)
    | Ast.FieldClsInit (_,  field_name) -> field_name = id
    ) fields

let handle_method_stmt obj fields stmt =
  let id_of_obj obj = 
    match obj with
    | (_, id) -> id
    in
  match stmt with
  | Ctree.Assign (id, expr) when is_field fields id ->
    Ctree.Assign ((id_of_obj obj) ^ "->" ^ id, expr)
  | Ctree.ObjectPropAssign (id, props, expr) when is_field fields id ->
    
    Ctree.ObjectPropAssign ((id_of_obj obj) ^ "->" ^ id, props, expr)
  | Ctree.ObjectPropAssign (id, props, expr) ->
    let id_with_game_object = 
      match id with
      |"super" -> "gameObject"
      | other -> other
    in
    let props_with_game_object = List.map (fun prop -> 
      match prop with
      | "super" -> "gameObject"
      | other -> other
    ) props in
    
    Ctree.ObjectPropAssign ((id_of_obj obj) ^ "->" ^ id_with_game_object, props_with_game_object, expr)
  | _ -> stmt
  

let handle_method_body obj fields stmts =
  List.map (fun stmt -> handle_method_stmt obj fields stmt) stmts

let collect_class_components id inher_opt (fields, methods) =
  id_list := id :: !id_list;
  struct_list := assemble_struct id inher_opt fields :: !struct_list;
  let class_constructor = assemble_constructor id inher_opt fields in (*TODO: Her skal gameObject init i*)

  construct_list := class_constructor :: !construct_list;
  
  let converted_methods = List.map convert_ast_method_to_ctree methods in
  let methods_with_class = List.map (fun (ts, name, formals, body) ->
    let object_param = (Ctree.Generic (id ^ "*"), "var" ^ id) in
    let updated_formals = object_param :: formals in
    let handled_body = handle_method_body object_param fields body in
    (ts, name, updated_formals, handled_body)
  ) converted_methods in
  method_list := List.rev_append methods_with_class !method_list




let rec expr_to_struct_assignment ids expr =
  match ids with 
  | [] -> expr
  | id :: tl ->  
  Ctree.AssignToStructExpr (id, expr_to_struct_assignment tl expr)

let rec handle_object_expr ids expr = 
  match expr with
  (*These will be changed*)
  | Ctree.Var name -> expr_to_struct_assignment ids (Ctree.Var (name))
  | Ctree.UnaryOp (op, expr) -> Ctree.UnaryOp (op, handle_object_expr ids expr) 
  | Ctree.BinaryOp (op, lhs, rhs) -> Ctree.BinaryOp (op, handle_object_expr ids lhs, handle_object_expr ids rhs)
  | FuncCallExpr (id, params) -> FuncCallExpr (id, (handle_params ids params))
  
  (*These are the same*)
  | Ctree.VarChain props -> 
    let props_with_game_object = List.map (fun prop -> 
      match prop with
      | "super" -> "gameObject"
      | other -> other
    ) props in
    expr_to_struct_assignment ids (Ctree.Var(String.concat "." props_with_game_object)) (* HVIS DER SKER EN ERROR SÅ ER DET HER*)
  | Ctree.ParenExpr inner -> Ctree.ParenExpr inner
  | Ctree.ArrayAccess (name, index) -> Ctree.ArrayAccess (name, index)
  | Ctree.ConstInt value -> Ctree.ConstInt value
  | Ctree.ConstFloat value -> Ctree.ConstFloat value
  | Ctree.Bool value -> Ctree.Bool value
  | AssignToStructExpr (id, expr) -> AssignToStructExpr (id, expr)
  | Ctree.VarAddress name -> Ctree.VarAddress (name)
  | Ctree.StringExpr s -> Ctree.StringExpr s
  

and handle_params ids params =
  let opt_id = List.hd params in
  let opt_id_str = match opt_id with
  | Ctree.Var "" -> ""
  | Ctree.Var x -> "." ^ x
   | _ -> ""
  in
  let rest_params = List.tl params in
  let id = Ctree.VarAddress((String.concat "." ids)  ^ opt_id_str) in
  let handled_params = List.map (fun p -> handle_object_expr ids p) rest_params in
  id :: handled_params

let rec stmt_to_struct_assignment ids stmt =
  match ids with 
  | [] -> stmt
  | id :: tl ->  
  Ctree.AssignToStructStmt (id, stmt_to_struct_assignment tl stmt)


let rec handle_object_stmt ids inher_opt stmt =
  match stmt with
  (*These will be changed*)
  | Ctree.Assign (id, e) ->  stmt_to_struct_assignment ids (Ctree.Assign(id, handle_object_expr ids e))
  | ForStmt (s, c, i, blk) -> ForStmt (s,c,i, List.map (handle_object_stmt ids inher_opt) blk)
  | IfStmt (e, blk) -> IfStmt (handle_object_expr ids e, List.map (handle_object_stmt ids inher_opt) blk)
  | ElseIfStmt (e, blk) -> ElseIfStmt (e, List.map (handle_object_stmt ids inher_opt) blk)
  | ElseStmt blk -> ElseStmt (List.map (handle_object_stmt ids inher_opt) blk)
  | WhileStmt (e, blk) -> WhileStmt (e, List.map (handle_object_stmt ids inher_opt) blk)
  | FuncCallStmt (id, params) -> FuncCallStmt (id, (handle_params ids params))
  | Increment id -> stmt_to_struct_assignment ids (Increment id)
  | Decrement id -> stmt_to_struct_assignment ids (Decrement id)
  | IncrementPre id -> stmt_to_struct_assignment ids (IncrementPre id)
  | DecrementPre id -> stmt_to_struct_assignment ids (DecrementPre id)
  | IncrementVal (id, e) -> stmt_to_struct_assignment ids (IncrementVal (id, e))
  | DecrementVal (id, e) -> stmt_to_struct_assignment ids (DecrementVal (id, e))
  | ReturnStmt e -> stmt_to_struct_assignment ids (ReturnStmt e)
  | ObjectPropAssign(name, props, expr) ->
    begin
      match (name, inher_opt) with
      | ("super", Some inher) -> stmt_to_struct_assignment ids (ObjectPropAssign(decapitalize inher, props, handle_object_expr ids expr))
      | _ -> ObjectPropAssign(name, props, expr)
    end
  (*These are the same*)
  | VarDefI (ts, id, e) -> VarDefI (ts, id, e)
  | VarDefU (ts, id) -> VarDefU (ts, id)
  | StructInit (ts, id) -> StructInit (ts, id)
  | AssignStructInit ts -> AssignStructInit ts
  | AssignToStructStmt (id, st) -> AssignToStructStmt (id, st)
  | ArrayDef (ts, id, size) -> ArrayDef (ts, id, size)
  | ArrayAssign (id, index, e) -> ArrayAssign (id, index, e)
  | BreakStmt -> BreakStmt
  | ContinueStmt -> ContinueStmt
  | Render id -> Render id
  
let string_of_typespec = function
  | Ast.Int -> "int"
  | Ast.Float -> "float"
  | Ast.Bool -> "int"
  | Ast.Void -> "void"
  | Ast.Generic g -> g

let generate_object ids current_ts inher_opt start_block update_block =
  let struct_init =
    (stmt_to_struct_assignment ids (Ctree.AssignStructInit((convert_ast_typespec_to_ctree_typespec current_ts))))
  in
  struct_init_list := struct_init :: !struct_init_list;
  
  let inher_render = 
    match inher_opt with
    | None -> None
    | Some inher -> 
        let render_stmt = Ctree.Render (String.concat "." ids ^ "." ^ decapitalize inher) in
        Some render_stmt
  in
  
  let handled_start = List.map (handle_object_stmt ids inher_opt) (List.map convert_ast_stmt_to_ctree_stmt start_block) in
  let start_list = handled_start in

  let handled_update = List.map (handle_object_stmt ids inher_opt) (List.map convert_ast_stmt_to_ctree_stmt update_block) in
  let update_list = match inher_render with
    | None -> handled_update
    | Some render_stmt -> handled_update @ [render_stmt]
  in
  (start_list, update_list)




let rec instantiate_class (ts, ids) classes =
    
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
    let fields, inher_opt, start, update = match class_def with
      | Ast.ClassStmt (_, (fields, StartDef start, UpdateDef update, _)) -> (fields, None, start, update)
      | Ast.ClassInherStmt (_, inher, (fields, StartDef start, UpdateDef update, _)) -> (fields, Some(inher), start, update)
    in

    let child_objects = List.fold_right (fun field acc ->
      match field with
      | Ast.FieldClsInit (field_ts, id) ->
        let child_result = instantiate_class (field_ts, ids @ [id]) classes in
        child_result :: acc
      | _ -> acc
    ) fields [] in

    let current_object = generate_object ids ts inher_opt start update in
    current_object :: List.flatten child_objects
  
let collect_object_components (start, update) =
  start_list := !start_list @ start;
  update_list := !update_list @ update

let rec generate_all_objects inst_classes_list classes =
  match inst_classes_list with
  | [] -> []
  | (ts, id) :: tl ->
    let objects = instantiate_class (ts, [id]) classes in
    objects @ generate_all_objects tl classes
  
  let instantiate_classes inst_classes_list classes =
    let all_objects = generate_all_objects inst_classes_list classes in
    List.iter collect_object_components all_objects


let rec generate_func_pt method_list =
  match method_list with
  | [] -> []
  | (ts, id, formals, _) :: tl ->
    let current_func_proto = Ctree.FuncProto (ts, id, formals) in
    current_func_proto :: generate_func_pt tl

let construct_ptypes method_list =
  let func_pt = generate_func_pt method_list in
  (func_pt)

let rec construct_funcs method_list =
  match method_list with
  | [] -> []
  | (ts, id, formals, body) :: tl ->
    let current_func_def = Ctree.FuncDef (ts, id, formals, body) in
    current_func_def :: construct_funcs tl

let construct_main start_list update_list =
  let start = Ctree.Start (!game_field_list @ !struct_init_list @ start_list) in
  let update = Ctree.Update (update_list) in
  (start, update)

let construct_program struct_list construct_list start_list update_list method_list =
  let structs = struct_list in
  let ptypes = construct_ptypes method_list in
  let constructors = construct_list in
  let funcs = construct_funcs method_list in
  let main = construct_main start_list update_list in
  (structs, ptypes, constructors, funcs, main)

let create_tree (gameClass, classes) : Ctree.program =
  class_list := classes;
  begin
    match gameClass with
    | Ast.ClassStmt (_, (fl, StartDef sb, UpdateDef ub, ml))
    | Ast.ClassInherStmt (_, _, (fl, StartDef sb, UpdateDef ub, ml)) ->
      collect_main_components (fl, sb, ub, ml)

  end;
  List.iter (fun clas ->
    let (id, fields, methods, inher) =
      match clas with
      | Ast.ClassStmt (id, (fl, _, _, ml)) -> (id, fl, ml, None)
      | Ast.ClassInherStmt (id, inher, (fl, _, _, ml)) -> (id, fl, ml, Some inher)
    in
    collect_class_components id inher (fields, methods)
  ) classes;
  instantiate_classes !inst_classes_list classes;
  construct_program !struct_list !construct_list !start_list !update_list !method_list

let format_to_c ast =
  let formatted_tree = create_tree ast in
  formatted_tree
