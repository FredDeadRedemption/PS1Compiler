open OUnit2
open Parser
open Lexer

(* Helper function to tokenize a string *)
let lex_string str =
  let lexbuf = Lexing.from_string str in
  let rec helper acc =
    let token = tokenize lexbuf in
    if token = EOF then List.rev (token :: acc)
    else helper (token :: acc)
  in
  helper []

(*test cases for each token *)
let test_lexer_add _ =
  let input = "+" in
  let expected = [ADD; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_sub _ =
  let input = "-" in
  let expected = [SUB; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_mul _ =
  let input = "*" in
  let expected = [MUL; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_div _ =
  let input = "/" in
  let expected = [DIV; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_mod _ =
  let input = "%" in
  let expected = [MOD; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_eq _ =
  let input = "=" in
  let expected = [EQ; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_excl _ =
  let input = "!" in
  let expected = [EXCL; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_and _ =
  let input = "&&" in
  let expected = [AND; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_or _ =
  let input = "||" in
  let expected = [OR; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_rangle _ =
  let input = ">" in
  let expected = [RANGLE; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_langle _ =
  let input = "<" in
  let expected = [LANGLE; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_lparen _ =
  let input = "(" in
  let expected = [LPAREN; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_rparen _ =
  let input = ")" in
  let expected = [RPAREN; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_lsqbrack _ =
  let input = "[" in
  let expected = [LSQBRACK; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_rsqbrack _ =
  let input = "]" in
  let expected = [RSQBRACK; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_lcurbrack _ =
  let input = "{" in
  let expected = [LCURBRACK; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_rcurbrack _ =
  let input = "}" in
  let expected = [RCURBRACK; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_comma _ =
  let input = "," in
  let expected = [COMMA; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_semicolon _ =
  let input = ";" in
  let expected = [SEMICOLON; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_colon _ =
  let input = ":" in
  let expected = [COLON; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_return _ =
  let input = "return" in
  let expected = [RETURN; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_break _ =
  let input = "break" in
  let expected = [BREAK; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_if _ =
  let input = "if" in
  let expected = [IF; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_else _ =
  let input = "else" in
  let expected = [ELSE; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_print _ =
  let input = "print" in
  let expected = [PRINT; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_start _ =
  let input = "start" in
  let expected = [START; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_update _ =
  let input = "update" in
  let expected = [UPDATE; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_true _ =
  let input = "true" in
  let expected = [TRUE; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_false _ =
  let input = "false" in
  let expected = [FALSE; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_type_int _ =
  let input = "int" in
  let expected = [TYPE_INT; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_type_float _ =
  let input = "float" in
  let expected = [TYPE_FLOAT; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_type_bool _ =
  let input = "bool" in
  let expected = [TYPE_BOOL; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_type_void _ =
  let input = "void" in
  let expected = [TYPE_VOID; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_class _ =
  let input = "class" in
  let expected = [CLASS; EOF] in
  let result = lex_string input in
  assert_equal expected result

let test_lexer_comment _ = 
  let input = "//hallo 1 \n 2" in
  let expected = [INT 2; EOF] in
  let result = lex_string input in 
  assert_equal expected result

let test_lexer_multicomment _ = 
  let input = "/*hallo 1 \n 2*/ 3" in
  let expected = [INT 3; EOF] in
  let result = lex_string input in 
  assert_equal expected result

let test_lexer_integer _ = 
  let input = "123 69 420 -1337 1000000000000000" in
  let expected = [INT 123; INT 69; INT 420; INT (-1337); INT 1000000000000000; EOF] in
  let result = lex_string input in 
  assert_equal expected result

let test_lexer_float _ = 
  let input = "1.23 6.9 4.20 -13.37 1.000000000000000" in
  let expected = [FLOAT 1.23; FLOAT 6.9; FLOAT 4.20; FLOAT (-13.37); FLOAT 1.000000000000000; EOF] in
  let result = lex_string input in 
  assert_equal expected result

let test_lexer_identifier _ = 
  let input = "a b c abekat abe kat hallo hALLO" in
  let expected = [ID "a"; ID "b"; ID "c"; ID "abekat"; ID "abe"; ID "kat"; ID "hallo"; ID "hALLO"; EOF] in
  let result = lex_string input in 
  assert_equal expected result

(* Test case for generic identifiers *)
let test_lexer_generic _ =
  let input = "Animal Building Car" in
  let expected = [TYPE_GENERIC "Animal"; TYPE_GENERIC "Building"; TYPE_GENERIC "Car"; EOF] in
  let result = lex_string input in
  assert_equal expected result

(*suite*)
let suite =
  "Lexer Test Suite" >::: [
    "test_lexer_add" >:: test_lexer_add;
    "test_lexer_sub" >:: test_lexer_sub;
    "test_lexer_mul" >:: test_lexer_mul;
    "test_lexer_div" >:: test_lexer_div;
    "test_lexer_mod" >:: test_lexer_mod;
    "test_lexer_eq" >:: test_lexer_eq;
    "test_lexer_excl" >:: test_lexer_excl;
    "test_lexer_and" >:: test_lexer_and;
    "test_lexer_or" >:: test_lexer_or;
    "test_lexer_rangle" >:: test_lexer_rangle;
    "test_lexer_langle" >:: test_lexer_langle;
    "test_lexer_lparen" >:: test_lexer_lparen;
    "test_lexer_rparen" >:: test_lexer_rparen;
    "test_lexer_lsqbrack" >:: test_lexer_lsqbrack;
    "test_lexer_rsqbrack" >:: test_lexer_rsqbrack;
    "test_lexer_lcurbrack" >:: test_lexer_lcurbrack;
    "test_lexer_rcurbrack" >:: test_lexer_rcurbrack;
    "test_lexer_comma" >:: test_lexer_comma;
    "test_lexer_semicolon" >:: test_lexer_semicolon;
    "test_lexer_colon" >:: test_lexer_colon;
    "test_lexer_return" >:: test_lexer_return;
    "test_lexer_break" >:: test_lexer_break;
    "test_lexer_if" >:: test_lexer_if;
    "test_lexer_else" >:: test_lexer_else;
    "test_lexer_print" >:: test_lexer_print;
    "test_lexer_start" >:: test_lexer_start;
    "test_lexer_update" >:: test_lexer_update;
    "test_lexer_true" >:: test_lexer_true;
    "test_lexer_false" >:: test_lexer_false;
    "test_lexer_type_int" >:: test_lexer_type_int;
    "test_lexer_type_float" >:: test_lexer_type_float;
    "test_lexer_type_bool" >:: test_lexer_type_bool;
    "test_lexer_type_void" >:: test_lexer_type_void;
    "test_lexer_class" >:: test_lexer_class;
    "test_lexer_comment" >:: test_lexer_comment;
    "test_lexer_multicomment" >:: test_lexer_multicomment;
    "test_lexer_integer" >:: test_lexer_integer;
    "test_lexer_float" >:: test_lexer_float;
    "test_lexer_identifier" >:: test_lexer_identifier;
    "test_lexer_generic" >:: test_lexer_generic;
  ]

let () =
  run_test_tt_main suite
