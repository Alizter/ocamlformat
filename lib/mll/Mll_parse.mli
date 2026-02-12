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

val parse : input_name:string -> string -> Mll_ast.mll_file
(** [parse ~input_name source] parses an OCamllex file from [source].
    @param input_name The filename to use for error messages.
    @raise Parse_error if parsing fails. *)
