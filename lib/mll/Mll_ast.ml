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

(** AST types for OCamllex (.mll) files *)

exception Syntax_error of string * Location.t

(** Location-annotated type *)
type 'a loc = {txt: 'a; loc: Location.t}

(** A comment with its content and location *)
type comment = {cmt_content: string; cmt_loc: Location.t}

(** Character set element in character classes *)
type char_set_element =
  | Cse_char of char  (** Single character, e.g., 'a' *)
  | Cse_range of char * char  (** Character range, e.g., 'a'-'z' *)

(** Regular expression patterns *)
type regexp =
  | Re_epsilon  (** Empty pattern (matches empty string) *)
  | Re_eof  (** End of file *)
  | Re_char of char loc  (** Single character literal *)
  | Re_string of string loc  (** String literal *)
  | Re_any  (** Wildcard _ (matches any character) *)
  | Re_charset of char_set_element list loc  (** Character class [abc] *)
  | Re_negcharset of char_set_element list loc  (** Negated class [^abc] *)
  | Re_concat of regexp list  (** Sequential composition r1 r2 *)
  | Re_alt of regexp list  (** Alternation r1 | r2 *)
  | Re_star of regexp  (** Zero or more r* *)
  | Re_plus of regexp  (** One or more r+ *)
  | Re_option of regexp  (** Optional r? *)
  | Re_ident of string loc  (** Named pattern reference *)
  | Re_as of regexp * string loc  (** Pattern capture: r as name *)
  | Re_group of regexp  (** Parenthesized group (r) *)
  | Re_diff of regexp * regexp  (** Set difference r1 # r2 *)

(** Embedded OCaml code block with location *)
type ocaml_code =
  { oc_code: string  (** Raw OCaml source code *)
  ; oc_loc: Location.t  (** Location in .mll file *) }

(** Named pattern definition: let name = regexp *)
type definition =
  { def_name: string loc
  ; def_pattern: regexp
  ; def_comments: comment list  (** Comments before this definition *)
  ; def_loc: Location.t }

(** A single rule case: | pattern { action } *)
type rule_case =
  {rc_pattern: regexp; rc_action: ocaml_code; rc_loc: Location.t}

(** Parse mode for rule entries *)
type parse_mode = Parse | Shortest

(** Rule entry point: rule name arg1 arg2 = parse | ... *)
type rule_entry =
  { re_name: string loc
  ; re_args: string loc list
  ; re_mode: parse_mode  (** parse or shortest *)
  ; re_cases: rule_case list
  ; re_comments: comment list  (** Comments before this rule entry *)
  ; re_loc: Location.t }

(** Complete .mll file structure *)
type mll_file =
  { mll_header: ocaml_code option  (** { ... } at start *)
  ; mll_definitions: definition list  (** let name = pattern *)
  ; mll_entrypoints: rule_entry list  (** rule ... and ... *)
  ; mll_trailer: ocaml_code option  (** { ... } at end *)
  ; mll_comments: comment list  (** Comments outside OCaml blocks *)
  ; mll_loc: Location.t }
