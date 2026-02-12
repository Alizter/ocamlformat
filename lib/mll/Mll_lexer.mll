(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

(** Lexer for OCamllex (.mll) files *)

{
open Mll_parser

let buffer = Buffer.create 256

let reset_buffer () = Buffer.reset buffer

let get_buffer () = Buffer.contents buffer

let store_char c = Buffer.add_char buffer c

let store_string s = Buffer.add_string buffer s

let store_lexeme lexbuf = Buffer.add_string buffer (Lexing.lexeme lexbuf)

exception Lexer_error of string * Location.t

let error lexbuf msg =
  let loc = Location.{
    loc_start = lexbuf.Lexing.lex_start_p;
    loc_end = lexbuf.Lexing.lex_curr_p;
    loc_ghost = false;
  } in
  raise (Lexer_error (msg, loc))

let make_loc lexbuf =
  Location.{
    loc_start = lexbuf.Lexing.lex_start_p;
    loc_end = lexbuf.Lexing.lex_curr_p;
    loc_ghost = false;
  }

let update_loc lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- {
    pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code (Lexing.lexeme_char lexbuf i) - 48) +
          10 * (Char.code (Lexing.lexeme_char lexbuf (i+1)) - 48) +
          (Char.code (Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if c < 0 || c > 255 then
    error lexbuf "illegal character escape"
  else Char.chr c

let char_for_octal_code lexbuf i =
  let c = 64 * (Char.code (Lexing.lexeme_char lexbuf i) - 48) +
          8 * (Char.code (Lexing.lexeme_char lexbuf (i+1)) - 48) +
          (Char.code (Lexing.lexeme_char lexbuf (i+2)) - 48) in
  Char.chr c

let char_for_hex_code lexbuf i =
  let hex_val c =
    match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | _ -> assert false
  in
  let c = 16 * (hex_val (Lexing.lexeme_char lexbuf i)) +
          (hex_val (Lexing.lexeme_char lexbuf (i+1))) in
  Char.chr c
}

let newline = '\r'? '\n'
let blank = [' ' '\t']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let ident = (lowercase | uppercase) identchar*
let decimal_digit = ['0'-'9']
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let octal_digit = ['0'-'7']

rule token = parse
  | newline
      { update_loc lexbuf; token lexbuf }
  | blank+
      { token lexbuf }
  | "(*"
      { let start_loc = make_loc lexbuf in
        reset_buffer ();
        store_string "(*";
        comment_content 1 lexbuf;
        COMMENT (get_buffer (), start_loc) }
  | "rule"
      { RULE }
  | "parse"
      { PARSE }
  | "shortest"
      { SHORTEST }
  | "and"
      { AND }
  | "let"
      { LET }
  | "as"
      { AS }
  | "eof"
      { EOF_KW }
  | '{'
      { let start_loc = make_loc lexbuf in
        reset_buffer ();
        ocaml_code 1 lexbuf;
        OCAML_CODE (get_buffer (), start_loc) }
  | '|'
      { PIPE }
  | '='
      { EQUAL }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | '*'
      { STAR }
  | '+'
      { PLUS }
  | '?'
      { QUESTION }
  | '#'
      { HASH }
  | '_'
      { UNDERSCORE }
  | '['
      { LBRACKET }
  | '^'
      { CARET }
  | '-'
      { DASH }
  | ']'
      { RBRACKET }
  | '\'' ([^ '\\' '\''] as c) '\''
      { CHAR c }
  | '\'' '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c) '\''
      { CHAR (char_for_backslash c) }
  | '\'' '\\' (decimal_digit decimal_digit decimal_digit) '\''
      { CHAR (char_for_decimal_code lexbuf 2) }
  | '\'' '\\' 'o' (octal_digit octal_digit octal_digit) '\''
      { CHAR (char_for_octal_code lexbuf 3) }
  | '\'' '\\' 'x' (hex_digit hex_digit) '\''
      { CHAR (char_for_hex_code lexbuf 3) }
  | '"'
      { reset_buffer ();
        string lexbuf;
        STRING (get_buffer ()) }
  | ident as s
      { IDENT s }
  | eof
      { EOF }
  | _ as c
      { error lexbuf (Printf.sprintf "unexpected character: %C" c) }

and comment_content depth = parse
  | "(*"
      { store_string "(*"; comment_content (depth + 1) lexbuf }
  | "*)"
      { store_string "*)";
        if depth > 1 then comment_content (depth - 1) lexbuf }
  | newline
      { update_loc lexbuf; store_lexeme lexbuf; comment_content depth lexbuf }
  | eof
      { error lexbuf "unterminated comment" }
  | _ as c
      { store_char c; comment_content depth lexbuf }

and ocaml_code depth = parse
  | '{'
      { store_char '{'; ocaml_code (depth + 1) lexbuf }
  | '}'
      { if depth > 1 then begin
          store_char '}';
          ocaml_code (depth - 1) lexbuf
        end }
  | '"'
      { store_char '"';
        ocaml_string lexbuf;
        ocaml_code depth lexbuf }
  | '\''
      { store_char '\'';
        ocaml_char lexbuf;
        ocaml_code depth lexbuf }
  | "(*"
      { store_string "(*";
        ocaml_comment 1 lexbuf;
        ocaml_code depth lexbuf }
  | "{" (['a'-'z' '_']* as delim) '|'
      { store_string ("{" ^ delim ^ "|");
        ocaml_quoted_string delim lexbuf;
        ocaml_code depth lexbuf }
  | newline
      { update_loc lexbuf;
        store_lexeme lexbuf;
        ocaml_code depth lexbuf }
  | eof
      { error lexbuf "unterminated OCaml code block" }
  | _
      { store_lexeme lexbuf; ocaml_code depth lexbuf }

and ocaml_string = parse
  | '"'
      { store_char '"' }
  | '\\' newline
      { update_loc lexbuf;
        store_lexeme lexbuf;
        ocaml_string lexbuf }
  | '\\' _
      { store_lexeme lexbuf; ocaml_string lexbuf }
  | newline
      { update_loc lexbuf;
        store_lexeme lexbuf;
        ocaml_string lexbuf }
  | eof
      { error lexbuf "unterminated string in OCaml code" }
  | _
      { store_lexeme lexbuf; ocaml_string lexbuf }

and ocaml_char = parse
  | [^ '\\' '\''] '\''
      { store_lexeme lexbuf }
  | '\\' _ '\''
      { store_lexeme lexbuf }
  | '\\' decimal_digit decimal_digit decimal_digit '\''
      { store_lexeme lexbuf }
  | '\\' 'o' octal_digit octal_digit octal_digit '\''
      { store_lexeme lexbuf }
  | '\\' 'x' hex_digit hex_digit '\''
      { store_lexeme lexbuf }
  | ""
      { (* Not a char literal, just a quote - backtrack *)
        () }

and ocaml_comment depth = parse
  | "(*"
      { store_string "(*"; ocaml_comment (depth + 1) lexbuf }
  | "*)"
      { store_string "*)";
        if depth > 1 then ocaml_comment (depth - 1) lexbuf }
  | '"'
      { store_char '"';
        ocaml_string lexbuf;
        ocaml_comment depth lexbuf }
  | newline
      { update_loc lexbuf;
        store_lexeme lexbuf;
        ocaml_comment depth lexbuf }
  | eof
      { error lexbuf "unterminated comment in OCaml code" }
  | _
      { store_lexeme lexbuf; ocaml_comment depth lexbuf }

and ocaml_quoted_string delim = parse
  | '|' (['a'-'z' '_']* as edelim) '}'
      { store_string ("|" ^ edelim ^ "}");
        if edelim <> delim then ocaml_quoted_string delim lexbuf }
  | newline
      { update_loc lexbuf;
        store_lexeme lexbuf;
        ocaml_quoted_string delim lexbuf }
  | eof
      { error lexbuf "unterminated quoted string in OCaml code" }
  | _
      { store_lexeme lexbuf; ocaml_quoted_string delim lexbuf }

and string = parse
  | '"'
      { () }
  | '\\' newline blank*
      { update_loc lexbuf; string lexbuf }
  | '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c)
      { store_char (char_for_backslash c); string lexbuf }
  | '\\' (decimal_digit decimal_digit decimal_digit)
      { store_char (char_for_decimal_code lexbuf 1); string lexbuf }
  | '\\' 'o' (octal_digit octal_digit octal_digit)
      { store_char (char_for_octal_code lexbuf 2); string lexbuf }
  | '\\' 'x' (hex_digit hex_digit)
      { store_char (char_for_hex_code lexbuf 2); string lexbuf }
  | newline
      { update_loc lexbuf; store_lexeme lexbuf; string lexbuf }
  | eof
      { error lexbuf "unterminated string" }
  | _ as c
      { store_char c; string lexbuf }
