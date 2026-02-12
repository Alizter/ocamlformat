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

(** Formatter for OCamllex (.mll) files *)

open Fmt
open Ocamlformat_mll.Mll_ast

(** Format a character as an OCamllex character literal *)
let fmt_char_literal c =
  let s =
    match c with
    | '\n' -> "'\\n'"
    | '\r' -> "'\\r'"
    | '\t' -> "'\\t'"
    | '\b' -> "'\\b'"
    | '\\' -> "'\\\\'"
    | '\'' -> "'\\''"
    | ' ' -> "' '"
    | c when Char.to_int c >= 32 && Char.to_int c < 127 ->
        Printf.sprintf "'%c'" c
    | c -> Printf.sprintf "'\\%03d'" (Char.to_int c)
  in
  str s

(** Format a character inside a character class *)
let fmt_charset_char c =
  match c with
  | '\n' -> str "'\\n'"
  | '\r' -> str "'\\r'"
  | '\t' -> str "'\\t'"
  | '\b' -> str "'\\b'"
  | '\\' -> str "'\\\\'"
  | '\'' -> str "'\\''"
  | '-' -> str "'-'"
  | ']' -> str "']'"
  | '^' -> str "'^'"
  | ' ' -> str "' '"
  | c when Char.to_int c >= 32 && Char.to_int c < 127 ->
      str (Printf.sprintf "'%c'" c)
  | c -> str (Printf.sprintf "'\\%03d'" (Char.to_int c))

(** Format a string as an OCamllex string literal *)
let fmt_string_literal s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"' ;
  String.iter
    ~f:(fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.to_int c >= 32 && Char.to_int c < 127 ->
          Buffer.add_char buf c
      | c -> Buffer.add_string buf (Printf.sprintf "\\%03d" (Char.to_int c)) )
    s ;
  Buffer.add_char buf '"' ;
  str (Buffer.contents buf)

(** Format a character set element *)
let fmt_charset_element = function
  | Cse_char c -> fmt_charset_char c
  | Cse_range (c1, c2) -> fmt_charset_char c1 $ str "-" $ fmt_charset_char c2

(** Format a character class (the contents inside [...]) *)
let fmt_charset elems = list elems (str " ") fmt_charset_element

(** Check if a regexp needs parentheses in a given context *)
let needs_parens_in_concat = function
  | Re_alt _ -> true
  | Re_as _ -> true
  | _ -> false

let needs_parens_in_suffix = function
  | Re_alt _ -> true
  | Re_concat _ -> true
  | Re_as _ -> true
  | Re_diff _ -> true
  | _ -> false

(** Check if a regexp ends with a suffix operator (star, plus, or question mark) *)
let rec ends_with_suffix_op = function
  | Re_star _ | Re_plus _ | Re_option _ -> true
  | Re_concat rs -> (
    match List.last rs with Some r -> ends_with_suffix_op r | None -> false )
  | Re_as _ -> false (* "as name" always ends with an identifier *)
  | Re_diff (_, r2) -> ends_with_suffix_op r2
  | _ -> false

(** Format a regular expression *)
let rec fmt_regexp = function
  | Re_epsilon -> str "()"
  | Re_eof -> str "eof"
  | Re_char {txt= c; _} -> fmt_char_literal c
  | Re_string {txt= s; _} -> fmt_string_literal s
  | Re_any -> str "_"
  | Re_charset {txt= elems; _} -> str "[" $ fmt_charset elems $ str "]"
  | Re_negcharset {txt= elems; _} -> str "[^" $ fmt_charset elems $ str "]"
  | Re_concat rs ->
      hovbox 2
        (list rs space_break (fun r ->
             if needs_parens_in_concat r then
               let sp = if ends_with_suffix_op r then str " " else noop in
               str "(" $ fmt_regexp r $ sp $ str ")"
             else fmt_regexp r ) )
  | Re_alt rs ->
      hvbox 0
        (list_fl rs (fun ~first ~last:_ r ->
             (if first then noop else space_break $ str "| ") $ fmt_regexp r )
        )
  | Re_star r -> fmt_regexp_suffix r $ str "*"
  | Re_plus r -> fmt_regexp_suffix r $ str "+"
  | Re_option r -> fmt_regexp_suffix r $ str "?"
  | Re_ident {txt= name; _} -> str name
  | Re_as (r, {txt= name; _}) -> fmt_regexp r $ str " as " $ str name
  | Re_group r ->
      (* Add space before close paren if regexp ends with suffix op *)
      let sp = if ends_with_suffix_op r then str " " else noop in
      str "(" $ fmt_regexp r $ sp $ str ")"
  | Re_diff (r1, r2) ->
      fmt_regexp_suffix r1 $ str " # " $ fmt_regexp_suffix r2

and fmt_regexp_suffix r =
  if needs_parens_in_suffix r then
    let sp = if ends_with_suffix_op r then str " " else noop in
    str "(" $ fmt_regexp r $ sp $ str ")"
  else fmt_regexp r

(** Format OCaml code block - preserve as-is with braces *)
let fmt_ocaml_code {oc_code; _} = str "{" $ str oc_code $ str "}"

(** Format a comment *)
let fmt_comment {cmt_content; _} = str cmt_content

(** Format a definition: let name = pattern *)
let fmt_definition {def_name= {txt= name; _}; def_pattern; def_comments; _} =
  let cmts =
    if List.is_empty def_comments then noop
    else list def_comments force_newline fmt_comment $ force_newline
  in
  cmts
  $ hvbox 2
      ( str "let " $ str name $ str " =" $ space_break
      $ fmt_regexp def_pattern )

(** Format a rule case: | pattern { action } *)
let fmt_rule_case {rc_pattern; rc_action; _} =
  str "  "
  $ hvbox 2
      ( str "| " $ fmt_regexp rc_pattern $ space_break
      $ fmt_ocaml_code rc_action )

(** Format a rule entry: name args = parse | case1 | case2 *)
let fmt_rule_entry ~first
    {re_name= {txt= name; _}; re_args; re_mode; re_cases; re_comments; _} =
  let cmts =
    if List.is_empty re_comments then noop
    else list re_comments force_newline fmt_comment $ force_newline
  in
  let keyword = if first then str "rule " else str "and " in
  let args =
    if List.is_empty re_args then noop
    else str " " $ list re_args (str " ") (fun {txt; _} -> str txt)
  in
  let mode_str =
    match re_mode with Parse -> "parse" | Shortest -> "shortest"
  in
  cmts
  $ vbox 0
      ( hvbox 2 (keyword $ str name $ args $ str (" = " ^ mode_str))
      $ force_newline
      $ vbox 0
          (list_fl re_cases (fun ~first:_ ~last:_ case ->
               fmt_rule_case case $ force_newline ) ) )

(** Format all rule entries *)
let fmt_entrypoints entries =
  list_fl entries (fun ~first ~last:_ entry ->
      (if first then noop else force_newline)
      $ fmt_rule_entry ~first entry )

(** Format a comment *)
let fmt_comment {cmt_content; cmt_loc= _} = str cmt_content

(** Format a complete .mll file *)
let fmt_mll_file
    { mll_header
    ; mll_definitions
    ; mll_entrypoints
    ; mll_trailer
    ; mll_comments
    ; _ } =
  let fmt_comments cmts =
    if List.is_empty cmts then noop
    else list cmts force_newline fmt_comment $ force_newline
  in
  vbox 0
    ( (* Comments at start of file (before header or first definition) *)
      fmt_comments mll_comments
    $
    (* Header *)
    opt mll_header (fun code ->
        fmt_ocaml_code code $ force_newline $ force_newline )
    $
    (* Definitions (each with their attached comments) *)
    ( if List.is_empty mll_definitions then noop
      else
        list mll_definitions force_newline fmt_definition
        $ force_newline $ force_newline )
    $
    (* Rule entries (each with their attached comments) *)
    fmt_entrypoints mll_entrypoints
    $
    (* Trailer *)
    opt mll_trailer (fun code ->
        force_newline $ fmt_ocaml_code code $ force_newline ) )
