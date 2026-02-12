{
(* Test various character class patterns *)
}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let whitespace = [' ' '\t' '\n' '\r']
let printable = [' '-'~']
let special = ['!' '@' '#' '$' '%' '^' '&' '*']

rule scan = parse
  | [^ '\n']+ as line { `Line line }
  | '\n' { `Newline }
  | eof { `Eof }
