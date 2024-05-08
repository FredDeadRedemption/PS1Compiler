



  let rec convert_ast_stmt_to_ctree_stmt (stmt : Ast.stmt) : Ctree.stmt =
    match stmt with
    | Ast.VarDef (ts, name, expr) ->
      let converted_ts = convert_ast_typespec_to_ctree_typespec ts in
      let converted_expr = convert_ast_expr_to_ctree_expr expr in
      Ctree.VarDef (converted_ts, name, converted_expr)
  
    | Ast.ArrayDef (ts, name, size) ->
      let converted_ts = convert_ast_typespec_to_ctree_typespec ts in
      Ctree.ArrayDef (converted_ts, name, size)
  
    | Ast.ArrayAssign (name, index, expr) ->
      let converted_expr = convert_ast_expr_to_ctree_expr expr in
      Ctree.ArrayAssign (name, index, converted_expr)
  
    | Ast.Assign (name, expr) ->
      let converted_expr = convert_ast_expr_to_ctree_expr expr in
      Ctree.Assign (name, converted_expr)
  
    | Ast.PrintStmt expr ->
      let converted_expr = convert_ast_expr_to_ctree_expr expr in
      Ctree.PrintStmt converted_expr
  
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
  
  
    
  
  (* Helper function to convert Ast typespec to Ctree typespec *)
  and convert_ast_typespec_to_ctree_typespec (ts : Ast.typespec) : Ctree.typespec =
    match ts with
    | Ast.Int -> Ctree.Int
    | Ast.Float -> Ctree.Float
    | Ast.Bool -> Ctree.Bool
    | Ast.Generic s -> Ctree.Generic s
  
  (* Helper function to convert Ast expressions to Ctree expressions *)
  and convert_ast_expr_to_ctree_expr (expr : Ast.expr) : Ctree.expr =
    match expr with
    | Ast.ParenExpr inner -> Ctree.ParenExpr (convert_ast_expr_to_ctree_expr inner)
    | Ast.FuncCall (name, params) -> Ctree.FuncCall (name, List.map convert_ast_expr_to_ctree_expr params)
    | Ast.ArrayAccess (name, index) -> Ctree.ArrayAccess (name, index)
    | Ast.ConstInt value -> Ctree.ConstInt value
    | Ast.ConstFloat value -> Ctree.ConstFloat value
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
  
  (* Conversion functions for unary and binary operations *)
  and convert_ast_unop_to_ctree_unop (op : Ast.unop) : Ctree.unop =
    match op with
    | Ast.UnopNot -> Ctree.UnopNot
    | Ast.UnopNeg -> Ctree.UnopNeg
  
  and convert_ast_binop_to_ctree_binop (op : Ast.binop) : Ctree.binop =
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


let convert_ast_field_to_ctree (Ast.FieldDef (ts, name, expr_opt)) =
  let converted_ts = convert_ast_typespec_to_ctree_typespec ts in
  let converted_expr = match expr_opt with
    | Some expr -> convert_ast_expr_to_ctree_expr expr
    | None -> Ctree.ConstInt 0
  in
  Ctree.VarDef (converted_ts, name, converted_expr)

let assemble_struct id fields =
  let converted_fields = List.map convert_ast_field_to_ctree fields in
  Ctree.StructDef(id, converted_fields)


(* Conversion function for methods to Ctree's expected format *)
let convert_ast_method_to_ctree (Ast.MethodDef (ts, name, formals, body)) : (Ctree.typespec * string * Ctree.formals * Ctree.block) =
  let converted_ts = match ts with
    | Ast.Int -> Ctree.Int
    | Ast.Float -> Ctree.Float
    | Ast.Bool -> Ctree.Bool
    | Ast.Generic s -> Ctree.Generic s
  in

  let converted_formals = List.map (fun (t, s) ->
    match t with
    | Ast.Int -> (Ctree.Int, s)
    | Ast.Float -> (Ctree.Float, s)
    | Ast.Bool -> (Ctree.Bool, s)
    | Ast.Generic st -> (Ctree.Generic st, s)
  ) formals in

  let converted_body = (* Convert Ast block to Ctree block *)
    List.map convert_ast_stmt_to_ctree_stmt body
  in

  (converted_ts, name, converted_formals, converted_body)

(* Update collect_components to convert methods *)
let collect_components id (fields, start_block, update_block, methods) id_list struct_list start_list update_list method_list =

  id_list := id :: !id_list;

  struct_list := assemble_struct id fields :: !struct_list;
  
  let converted_start = List.map convert_ast_stmt_to_ctree_stmt start_block in
  start_list := List.rev_append converted_start !start_list;
  
  let converted_update = List.map convert_ast_stmt_to_ctree_stmt update_block in
  update_list := List.rev_append converted_update !update_list;
  
  (* Add converted methods to the global method list *)
  let converted_methods = List.map convert_ast_method_to_ctree methods in
  method_list := List.rev_append converted_methods !method_list

let rec generate_struct_pt id_list =
  match id_list with
  | [] -> []  
  | id :: tl ->
    let current_struct = Ctree.StructProto(id) in
    current_struct :: generate_struct_pt tl
      
let rec generate_func_pt method_list =
  match method_list with
  | [] -> [] 
  | (ts, id, formals, _) :: tl ->
    let current_func_proto = Ctree.FuncProto(ts, id, formals) in
    current_func_proto :: generate_func_pt tl
      
let construct_ptypes id_list method_list =
  let func_pt = generate_func_pt method_list in
  let struct_pt = generate_struct_pt id_list in
  (func_pt, struct_pt)

let rec construct_funcs method_list =
  match method_list with
  | [] -> []
  | (ts, id, formals, body) :: tl ->
    let current_func_def = Ctree.FuncDef(ts, id, formals, body) in
    current_func_def :: construct_funcs tl


let construct_update update_list =
  Ctree.Update(update_list)

let construct_main start_list update_list =
  let start = start_list in 
  let update = update_list in
  (start, update)

let construct_program id_list struct_list start_list update_list method_list = 
  let ptypes = construct_ptypes id_list method_list  in
  let funcs = construct_funcs method_list in
  let structs = struct_list in
  let main = construct_main start_list update_list in
  (ptypes, funcs, structs, main)


let create_tree (gameClass, classes) = 
  match gameClass with
  | Ast.ClassStmt _ ->
    Printf.printf "Game\n";

  (* Initialize lists to accumulate results*) (* TODO: Skal måske også bruges i gameClass *)
  let id_list     = ref [] in
  let struct_list = ref [] in
  let start_list  = ref [] in
  let update_list = ref [] in
  let method_list = ref [] in

  List.iter (fun clas ->
    match clas with
    | Ast.ClassStmt (id, (fl, StartDef sb, UpdateDef ub, ml)) -> 
      collect_components id (fl, sb, ub, ml) id_list struct_list start_list update_list method_list
  ) classes;

  let ctree = construct_program !id_list !struct_list !start_list !update_list !method_list in
  ctree


let format_to_c ast = 
  let formatted_tree = create_tree ast in
  formatted_tree