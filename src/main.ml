
(*xXxFrontendxXx*)
  let filename = Sys.argv.(1) in
  let filehandle = open_in filename in
  let lexbuf = Lexing.from_channel filehandle in
  let prog = Parser.program Lexer.tokenize lexbuf in

(*xXxBackendxXx*)
  (*Type_check.check_type prog;*)
  let ctree = Cformatter.format_to_c prog in

(*xXxOutputxXx*)
  Ast_printer.print_program prog;
  Ctree_printer.print_program ctree;
  

  Codegen.print_to_file filename ctree
  