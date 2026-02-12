(* File-level comment *)
{
(* Header comment inside OCaml block *)
open Lexing
type token = INT of int | IDENT of string | EOF
}

(* Comment before first definition *)
let digit = ['0'-'9']

(* Comment before second definition *)
let letter = ['a'-'z' 'A'-'Z']

(* Comment before rule *)
rule token = parse
| digit+ as n { INT (int_of_string n) }
| letter+ as s { IDENT s }
| eof { EOF }
