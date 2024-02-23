# 1 "src/frontend/parser/lexer.mll"
 
open Token
(*open Lexing*)

exception Eof

# 9 "src/frontend/parser/lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\211\255\212\255\213\255\001\000\017\000\218\255\002\000\
    \001\000\000\000\003\000\000\000\003\000\003\000\002\000\010\000\
    \020\000\029\000\016\000\005\000\236\255\004\000\001\000\239\255\
    \240\255\241\255\033\000\243\255\033\000\245\255\246\255\247\255\
    \248\255\099\000\250\255\251\255\252\255\253\255\254\255\255\255\
    \109\000\119\000\216\255\217\255\238\255\237\255\013\000\235\255\
    \021\000\017\000\017\000\234\255\020\000\027\000\025\000\028\000\
    \001\000\023\000\002\000\030\000\032\000\233\255\046\000\028\000\
    \231\255\030\000\076\000\226\255\222\255\224\255\062\000\232\255\
    \068\000\072\000\230\255\076\000\082\000\229\255\075\000\076\000\
    \228\255\068\000\085\000\227\255\082\000\080\000\088\000\225\255\
    \075\000\090\000\223\255\087\000\083\000\221\255\089\000\085\000\
    \080\000\220\255\081\000\081\000\085\000\090\000\219\255\153\000\
    \163\000\002\000\253\255\254\255\255\255\004\000\211\000\252\255\
    \253\255\254\255\006\000\017\000\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\042\000\040\000\255\255\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\255\255\044\000\044\000\255\255\
    \255\255\255\255\013\000\255\255\011\000\255\255\255\255\255\255\
    \255\255\006\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \041\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \041\000\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\001\000\003\000\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\000\000\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\000\000\000\000\000\000\000\000\255\255\000\000\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \000\000\255\255\255\255\000\000\000\000\000\000\255\255\000\000\
    \255\255\255\255\000\000\255\255\255\255\000\000\255\255\255\255\
    \000\000\255\255\255\255\000\000\255\255\255\255\255\255\000\000\
    \255\255\255\255\000\000\255\255\255\255\000\000\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\106\000\000\000\000\000\000\000\255\255\111\000\000\000\
    \000\000\000\000\255\255\255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\006\000\003\000\003\000\108\000\004\000\108\000\109\000\
    \113\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \006\000\020\000\000\000\000\000\000\000\025\000\022\000\044\000\
    \039\000\038\000\027\000\029\000\000\000\028\000\033\000\026\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\032\000\031\000\024\000\030\000\023\000\103\000\
    \116\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\042\000\000\000\000\000\000\000\041\000\
    \043\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\035\000\000\000\034\000\000\000\000\000\
    \000\000\091\000\015\000\018\000\057\000\010\000\017\000\098\000\
    \084\000\016\000\046\000\059\000\019\000\009\000\013\000\088\000\
    \008\000\075\000\007\000\094\000\012\000\081\000\014\000\011\000\
    \078\000\072\000\069\000\037\000\021\000\036\000\053\000\048\000\
    \045\000\047\000\070\000\049\000\050\000\051\000\068\000\065\000\
    \062\000\054\000\056\000\058\000\052\000\060\000\061\000\063\000\
    \064\000\066\000\055\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\067\000\071\000\073\000\074\000\076\000\077\000\079\000\
    \080\000\082\000\083\000\085\000\086\000\087\000\089\000\090\000\
    \092\000\093\000\095\000\096\000\097\000\099\000\100\000\101\000\
    \102\000\104\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\113\000\000\000\000\000\
    \114\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\115\000\000\000\000\000\
    \002\000\000\000\107\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\112\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\004\000\105\000\000\000\109\000\105\000\
    \114\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\255\255\000\000\000\000\022\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
    \115\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\026\000\255\255\255\255\255\255\028\000\
    \026\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\000\000\255\255\000\000\255\255\255\255\
    \255\255\009\000\000\000\000\000\056\000\000\000\000\000\007\000\
    \011\000\000\000\019\000\058\000\000\000\000\000\000\000\010\000\
    \000\000\014\000\000\000\008\000\000\000\012\000\000\000\000\000\
    \013\000\015\000\016\000\000\000\000\000\000\000\017\000\018\000\
    \021\000\046\000\016\000\048\000\049\000\050\000\052\000\053\000\
    \054\000\017\000\055\000\057\000\017\000\059\000\060\000\062\000\
    \063\000\065\000\017\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\066\000\070\000\072\000\073\000\075\000\076\000\078\000\
    \079\000\081\000\082\000\084\000\085\000\086\000\088\000\089\000\
    \091\000\092\000\094\000\095\000\096\000\098\000\099\000\100\000\
    \101\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\110\000\255\255\255\255\
    \110\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\110\000\255\255\255\255\
    \000\000\255\255\105\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\110\000";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec tokenize lexbuf =
   __ocaml_lex_tokenize_rec lexbuf 0
and __ocaml_lex_tokenize_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 21 "src/frontend/parser/lexer.mll"
        ( LPAREN )
# 200 "src/frontend/parser/lexer.ml"

  | 1 ->
# 22 "src/frontend/parser/lexer.mll"
        ( RPAREN )
# 205 "src/frontend/parser/lexer.ml"

  | 2 ->
# 23 "src/frontend/parser/lexer.mll"
        ( LBRACE )
# 210 "src/frontend/parser/lexer.ml"

  | 3 ->
# 24 "src/frontend/parser/lexer.mll"
        ( RBRACE )
# 215 "src/frontend/parser/lexer.ml"

  | 4 ->
# 25 "src/frontend/parser/lexer.mll"
        ( LBRACK )
# 220 "src/frontend/parser/lexer.ml"

  | 5 ->
# 26 "src/frontend/parser/lexer.mll"
        ( RBRACK )
# 225 "src/frontend/parser/lexer.ml"

  | 6 ->
# 27 "src/frontend/parser/lexer.mll"
        ( DOT )
# 230 "src/frontend/parser/lexer.ml"

  | 7 ->
# 28 "src/frontend/parser/lexer.mll"
        ( COLON )
# 235 "src/frontend/parser/lexer.ml"

  | 8 ->
# 29 "src/frontend/parser/lexer.mll"
        ( SEMICOLON )
# 240 "src/frontend/parser/lexer.ml"

  | 9 ->
# 30 "src/frontend/parser/lexer.mll"
        ( EQ )
# 245 "src/frontend/parser/lexer.ml"

  | 10 ->
# 31 "src/frontend/parser/lexer.mll"
        ( ADD )
# 250 "src/frontend/parser/lexer.ml"

  | 11 ->
# 32 "src/frontend/parser/lexer.mll"
        ( SUB )
# 255 "src/frontend/parser/lexer.ml"

  | 12 ->
# 33 "src/frontend/parser/lexer.mll"
        ( MUL )
# 260 "src/frontend/parser/lexer.ml"

  | 13 ->
# 34 "src/frontend/parser/lexer.mll"
        ( DIV )
# 265 "src/frontend/parser/lexer.ml"

  | 14 ->
# 35 "src/frontend/parser/lexer.mll"
        ( MOD )
# 270 "src/frontend/parser/lexer.ml"

  | 15 ->
# 36 "src/frontend/parser/lexer.mll"
        ( LANGLE )
# 275 "src/frontend/parser/lexer.ml"

  | 16 ->
# 37 "src/frontend/parser/lexer.mll"
        ( RANGLE )
# 280 "src/frontend/parser/lexer.ml"

  | 17 ->
# 38 "src/frontend/parser/lexer.mll"
         ( AND )
# 285 "src/frontend/parser/lexer.ml"

  | 18 ->
# 39 "src/frontend/parser/lexer.mll"
         ( OR )
# 290 "src/frontend/parser/lexer.ml"

  | 19 ->
# 40 "src/frontend/parser/lexer.mll"
        ( NEG )
# 295 "src/frontend/parser/lexer.ml"

  | 20 ->
# 41 "src/frontend/parser/lexer.mll"
          ( LET )
# 300 "src/frontend/parser/lexer.ml"

  | 21 ->
# 42 "src/frontend/parser/lexer.mll"
            (CONST )
# 305 "src/frontend/parser/lexer.ml"

  | 22 ->
# 43 "src/frontend/parser/lexer.mll"
               ( FUN )
# 310 "src/frontend/parser/lexer.ml"

  | 23 ->
# 44 "src/frontend/parser/lexer.mll"
          ( TYPE_INT )
# 315 "src/frontend/parser/lexer.ml"

  | 24 ->
# 45 "src/frontend/parser/lexer.mll"
            ( TYPE_FLOAT )
# 320 "src/frontend/parser/lexer.ml"

  | 25 ->
# 46 "src/frontend/parser/lexer.mll"
           ( TYPE_BOOL )
# 325 "src/frontend/parser/lexer.ml"

  | 26 ->
# 47 "src/frontend/parser/lexer.mll"
           ( TYPE_VOID )
# 330 "src/frontend/parser/lexer.ml"

  | 27 ->
# 48 "src/frontend/parser/lexer.mll"
           ( TYPE_NULL )
# 335 "src/frontend/parser/lexer.ml"

  | 28 ->
# 49 "src/frontend/parser/lexer.mll"
           ( TRUE )
# 340 "src/frontend/parser/lexer.ml"

  | 29 ->
# 50 "src/frontend/parser/lexer.mll"
            ( FALSE )
# 345 "src/frontend/parser/lexer.ml"

  | 30 ->
# 51 "src/frontend/parser/lexer.mll"
            ( WHILE )
# 350 "src/frontend/parser/lexer.ml"

  | 31 ->
# 52 "src/frontend/parser/lexer.mll"
         ( IF )
# 355 "src/frontend/parser/lexer.ml"

  | 32 ->
# 53 "src/frontend/parser/lexer.mll"
           ( ELSE )
# 360 "src/frontend/parser/lexer.ml"

  | 33 ->
# 54 "src/frontend/parser/lexer.mll"
          ( FOR )
# 365 "src/frontend/parser/lexer.ml"

  | 34 ->
# 55 "src/frontend/parser/lexer.mll"
           ( MAIN )
# 370 "src/frontend/parser/lexer.ml"

  | 35 ->
# 56 "src/frontend/parser/lexer.mll"
            ( PRINT )
# 375 "src/frontend/parser/lexer.ml"

  | 36 ->
# 57 "src/frontend/parser/lexer.mll"
             ( RETURN )
# 380 "src/frontend/parser/lexer.ml"

  | 37 ->
# 58 "src/frontend/parser/lexer.mll"
               ( tokenize lexbuf )
# 385 "src/frontend/parser/lexer.ml"

  | 38 ->
# 59 "src/frontend/parser/lexer.mll"
         ( read_comment lexbuf )
# 390 "src/frontend/parser/lexer.ml"

  | 39 ->
# 60 "src/frontend/parser/lexer.mll"
         ( read_multi_line_comment lexbuf )
# 395 "src/frontend/parser/lexer.ml"

  | 40 ->
let
# 61 "src/frontend/parser/lexer.mll"
               i
# 401 "src/frontend/parser/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 61 "src/frontend/parser/lexer.mll"
                 ( INT(int_of_string i) )
# 405 "src/frontend/parser/lexer.ml"

  | 41 ->
let
# 62 "src/frontend/parser/lexer.mll"
             i
# 411 "src/frontend/parser/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 62 "src/frontend/parser/lexer.mll"
               ( FLOAT(float_of_string i) )
# 415 "src/frontend/parser/lexer.ml"

  | 42 ->
# 64 "src/frontend/parser/lexer.mll"
            ( tokenize lexbuf )
# 420 "src/frontend/parser/lexer.ml"

  | 43 ->
# 65 "src/frontend/parser/lexer.mll"
        ( raise Eof )
# 425 "src/frontend/parser/lexer.ml"

  | 44 ->
let
# 66 "src/frontend/parser/lexer.mll"
         c
# 431 "src/frontend/parser/lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 66 "src/frontend/parser/lexer.mll"
           ( failwith (Printf.sprintf "unexpected character: %C" c) )
# 435 "src/frontend/parser/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_tokenize_rec lexbuf __ocaml_lex_state

and read_comment lexbuf =
   __ocaml_lex_read_comment_rec lexbuf 105
and __ocaml_lex_read_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 69 "src/frontend/parser/lexer.mll"
            ( tokenize lexbuf )
# 447 "src/frontend/parser/lexer.ml"

  | 1 ->
# 70 "src/frontend/parser/lexer.mll"
        ( raise Eof )
# 452 "src/frontend/parser/lexer.ml"

  | 2 ->
# 71 "src/frontend/parser/lexer.mll"
      ( read_comment lexbuf )
# 457 "src/frontend/parser/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_comment_rec lexbuf __ocaml_lex_state

and read_multi_line_comment lexbuf =
   __ocaml_lex_read_multi_line_comment_rec lexbuf 110
and __ocaml_lex_read_multi_line_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 74 "src/frontend/parser/lexer.mll"
         ( tokenize lexbuf )
# 469 "src/frontend/parser/lexer.ml"

  | 1 ->
# 75 "src/frontend/parser/lexer.mll"
            ( read_multi_line_comment lexbuf )
# 474 "src/frontend/parser/lexer.ml"

  | 2 ->
# 76 "src/frontend/parser/lexer.mll"
        ( raise Eof )
# 479 "src/frontend/parser/lexer.ml"

  | 3 ->
# 77 "src/frontend/parser/lexer.mll"
      ( read_multi_line_comment lexbuf )
# 484 "src/frontend/parser/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_multi_line_comment_rec lexbuf __ocaml_lex_state

;;

# 78 "src/frontend/parser/lexer.mll"
 
let main () = begin
  try
    let filename = "../../../code.psx" in (* Sys.argv.(1) pass file as cli arg*)
    let filehandle = open_in filename in
    let lexbuf = Lexing.from_channel filehandle in
    while true do 
      let result = tokenize lexbuf in
      match result with 
      | INT(i) -> Printf.printf "%d (INT)\n" i
      | FLOAT(i) -> Printf.printf "%f (FLOAT)\n" i
      | LPAREN -> Printf.printf "( (LPAREN)\n"
      | RPAREN -> Printf.printf ") (RPAREN)\n"
      | LBRACE -> Printf.printf "{ (LBRACE)\n"
      | RBRACE -> Printf.printf "} (RBRACE)\n"
      | LBRACK -> Printf.printf "[ (LBRACK)\n"
      | RBRACK -> Printf.printf "] (RBRACK)\n"
      | DOT -> Printf.printf ". (DOT)\n"
      | COLON -> Printf.printf ": (COLON)\n"
      | SEMICOLON -> Printf.printf "; (SEMICOLON)\n"
      | EQ -> Printf.printf "= (EQ)\n"
      | ADD -> Printf.printf "+ (ADD)\n"
      | SUB -> Printf.printf "- (SUB)\n"
      | MUL -> Printf.printf "* (MUL)\n"
      | DIV -> Printf.printf "/ (DIV)\n"
      | MOD -> Printf.printf "%% (MOD)\n"
      | LANGLE -> Printf.printf "< (LANGLE)\n"
      | RANGLE -> Printf.printf "> (RANGLE)\n"
      | AND -> Printf.printf "&& (AND)\n"
      | OR -> Printf.printf "|| (OR)\n"
      | NEG -> Printf.printf "! (NEG)\n"
      | LET -> Printf.printf "let (LET)\n"
      | CONST -> Printf.printf "const (CONST)\n"
      | FUN -> Printf.printf "fun (FUN)\n"
      | TYPE_INT -> Printf.printf "int (TYPE_INT)\n"
      | TYPE_FLOAT -> Printf.printf "float (TYPE_FLOAT)\n"
      | TYPE_BOOL -> Printf.printf "bool (TYPE_BOOL)\n"
      | TYPE_VOID -> Printf.printf "void (TYPE_VOID)\n"
      | TYPE_NULL -> Printf.printf "null (TYPE_NULL)\n"
      | TRUE -> Printf.printf "true (TRUE)\n"
      | FALSE -> Printf.printf "false (FALSE)\n"
      | WHILE -> Printf.printf "while (WHILE)\n"
      | IF -> Printf.printf "if (IF)\n"
      | ELSE -> Printf.printf "else (ELSE)\n"
      | FOR -> Printf.printf "for (FOR)\n"
      | MAIN -> Printf.printf "main (MAIN)\n"
      | PRINT -> Printf.printf "print (PRINT)\n"
      | RETURN -> Printf.printf "return (RETURN\n)"
    done
  with Eof -> exit 0
end ;;
main () ;;

# 545 "src/frontend/parser/lexer.ml"
