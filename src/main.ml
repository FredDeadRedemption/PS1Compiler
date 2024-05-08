(*let () =*)

(*xXxFrontendxXx*)
  let filename = Sys.argv.(1) in
  let filehandle = open_in filename in
  let lexbuf = Lexing.from_channel filehandle in
  let prog = Parser.program Lexer.tokenize lexbuf in

(*xXxBackendxXx*)
 (* let ctree = Cformatter.format_to_c prog in*)
  Type_check.check_type prog;

(*xXxOutputxXx*)
  Ctree_printer.print_program ctree;
  Codegen.print_to_file filename ctree
  
  
  (*Ast_printer.print_program ctree;*)
  
  (*let usage = "usage: ./main [options] file.psx"

let parse_only = ref false

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".psx") then
      raise (Arg.Bad "no .psx extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1



let () =
  let c = open_in file in
  let lexbuf = Lexing.from_channel c in
  let prog = Parser.program Lexer.tokenize lexbuf in
  Compile.print_to_file file prog
 

*)