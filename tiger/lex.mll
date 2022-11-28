{ 
module T = Tokens
module L = Lexing 
module B = Buffer

let get      = L.lexeme
let sprintf  = Printf.sprintf

let get_line lexbuf = lexbuf.L.lex_curr_p.pos_lnum
let get_col lexbuf =
  let p = lexbuf.L.lex_curr_p in
    p.pos_cnum - p.pos_bol
}
let ws = [' ' '\t']
let nl = '\n'
let wsnl = [' ' '\t' '\n' '\x0C' '\r']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = alpha (alpha|digit|'_')*

rule token = parse
| ws+         { token lexbuf }
| nl          { L.new_line lexbuf; token lexbuf }
| digit+      { T.INT (int_of_string (get lexbuf), get_line lexbuf, get_col lexbuf) }
| "if"        { T.IF () }
