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

(** Parser entry point for OCamllex (.mll) files *)

exception Parse_error of string * Location.t

let parse ~input_name source =
  let lexbuf = Lexing.from_string source in
  Location.init_info lexbuf input_name ;
  try Mll_parser.mll_file Mll_lexer.token lexbuf with
  | Mll_lexer.Lexer_error (msg, loc) -> raise (Parse_error (msg, loc))
  | Mll_parser.Error ->
      let loc =
        Location.
          { loc_start= lexbuf.Lexing.lex_start_p
          ; loc_end= lexbuf.Lexing.lex_curr_p
          ; loc_ghost= false }
      in
      raise (Parse_error ("syntax error", loc))
