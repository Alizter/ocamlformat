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

(** Menhir grammar for OCamllex (.mll) files *)

%{
open Mll_ast

let make_loc (startpos, endpos) =
  Location.{
    loc_start = startpos;
    loc_end = endpos;
    loc_ghost = false;
  }

let mkloc txt (startpos, endpos) =
  {txt; loc = make_loc (startpos, endpos)}
%}

%token <string> IDENT
%token <string> STRING
%token <char> CHAR
%token <string * Location.t> OCAML_CODE
%token <string * Location.t> COMMENT
%token RULE PARSE SHORTEST AND LET AS EOF_KW
%token PIPE EQUAL LPAREN RPAREN STAR PLUS QUESTION HASH UNDERSCORE
%token LBRACKET CARET DASH RBRACKET
%token EOF

%start <Mll_ast.mll_file> mll_file

%%

mll_file:
  | cmts_start = comments
    header = option(ocaml_code)
    body = body_with_comments
    trailer = option(ocaml_code)
    cmts_end = comments
    EOF
    { let (defs, entries) = body in
      { mll_header = header;
        mll_definitions = defs;
        mll_entrypoints = entries;
        mll_trailer = trailer;
        mll_comments = cmts_start @ cmts_end;
        mll_loc = make_loc ($startpos, $endpos) } }

comments:
  | cmts = list(comment)
    { cmts }

comment:
  | c = COMMENT
    { let (s, loc) = c in
      { cmt_content = s; cmt_loc = loc } }

ocaml_code:
  | code = OCAML_CODE
    { let (s, loc) = code in
      { oc_code = s; oc_loc = loc } }

(* Parse body: definitions followed by rule entries, handling comments *)
body_with_comments:
  | cmts = comments LET name = IDENT EQUAL pat = regexp rest = body_with_comments
    { let def = { def_name = mkloc name ($startpos(name), $endpos(name));
                  def_pattern = pat;
                  def_comments = cmts;
                  def_loc = make_loc ($startpos(name), $endpos(pat)) } in
      let (defs, entries) = rest in
      (def :: defs, entries) }
  | cmts = comments RULE first_entry = rule_entry rest = and_rule_entries
    { let first = { first_entry with re_comments = cmts } in
      ([], first :: rest) }

and_rule_entries:
  | (* empty *)
    { [] }
  | cmts = comments AND entry = rule_entry rest = and_rule_entries
    { { entry with re_comments = cmts } :: rest }

rule_entry:
  | name = IDENT args = list(IDENT) EQUAL
    mode = parse_mode
    PIPE? cases = separated_nonempty_list(PIPE, rule_case)
    { { re_name = mkloc name ($startpos(name), $endpos(name));
        re_args = List.map (fun a -> mkloc a ($startpos(args), $endpos(args))) args;
        re_mode = mode;
        re_cases = cases;
        re_comments = [];
        re_loc = make_loc ($startpos, $endpos) } }

parse_mode:
  | PARSE { Parse }
  | SHORTEST { Shortest }

rule_case:
  | pat = regexp code = OCAML_CODE
    { let (s, loc) = code in
      { rc_pattern = pat;
        rc_action = { oc_code = s; oc_loc = loc };
        rc_loc = make_loc ($startpos, $endpos) } }

(* Regexp grammar with proper precedence *)
(* Precedence from lowest to highest: *)
(* 1. as binding *)
(* 2. alternation | *)
(* 3. concatenation *)
(* 4. suffix operators *, +, ? *)
(* 5. atoms *)

regexp:
  | r = regexp_as
    { r }

regexp_as:
  | r = regexp_alt AS name = IDENT
    { Re_as (r, mkloc name ($startpos(name), $endpos(name))) }
  | r = regexp_alt
    { r }

regexp_alt:
  | rs = separated_nonempty_list(PIPE, regexp_concat)
    { match rs with
      | [r] -> r
      | _ -> Re_alt rs }

regexp_concat:
  | rs = nonempty_list(regexp_suffix)
    { match rs with
      | [r] -> r
      | _ -> Re_concat rs }

regexp_suffix:
  | r = regexp_atom STAR
    { Re_star r }
  | r = regexp_atom PLUS
    { Re_plus r }
  | r = regexp_atom QUESTION
    { Re_option r }
  | r = regexp_atom
    { r }

regexp_atom:
  | c = CHAR
    { Re_char (mkloc c ($startpos, $endpos)) }
  | s = STRING
    { Re_string (mkloc s ($startpos, $endpos)) }
  | name = IDENT
    { Re_ident (mkloc name ($startpos, $endpos)) }
  | UNDERSCORE
    { Re_any }
  | EOF_KW
    { Re_eof }
  | LPAREN RPAREN
    { Re_epsilon }
  | LPAREN r = regexp RPAREN
    { Re_group r }
  | LBRACKET elems = charset_content RBRACKET
    { Re_charset (mkloc elems ($startpos, $endpos)) }
  | LBRACKET CARET elems = charset_content RBRACKET
    { Re_negcharset (mkloc elems ($startpos, $endpos)) }
  | r1 = regexp_atom HASH r2 = regexp_atom
    { Re_diff (r1, r2) }

charset_content:
  | elems = list(charset_element)
    { elems }

charset_element:
  | c1 = charset_char DASH c2 = charset_char
    { Cse_range (c1, c2) }
  | c = charset_char
    { Cse_char c }

charset_char:
  | c = CHAR
    { c }
  | s = STRING
    { if String.length s = 1 then s.[0]
      else raise (Mll_ast.Syntax_error ("invalid character in charset", make_loc ($startpos, $endpos))) }
