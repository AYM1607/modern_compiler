{ 
module T = Tokens
module L = Lexing 
module B = Buffer
module E = ErrMsg

let get      = L.lexeme
let sprintf  = Printf.sprintf

let get_line lexbuf = lexbuf.L.lex_curr_p.pos_lnum
let get_col lexbuf =
  let p = lexbuf.L.lex_curr_p in
    p.pos_cnum - p.pos_bol

let error msg lexbuf =
  E.error msg (get_line lexbuf) (get_col lexbuf)
}

let ws = [' ' '\t']
let nl = '\n'
let slnl = ['\n' '\x0C'] (* New lines in string literals *)
let slvc = [^'"' '\\' '\t' '\n' '\x0C'] (* String literal valid characters *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = alpha (alpha|digit|'_')*

rule token = parse
| ws+         { token lexbuf }
| nl          { L.new_line lexbuf; token lexbuf }
| digit+      { T.INT (int_of_string (get lexbuf), get_line lexbuf, get_col lexbuf) }
| "while"     { T.WHILE (get_line lexbuf, get_col lexbuf) }
| "for"       { T.FOR (get_line lexbuf, get_col lexbuf) }
| "to"        { T.TO (get_line lexbuf, get_col lexbuf) }
| "break"     { T.BRK (get_line lexbuf, get_col lexbuf) }
| "let"       { T.LET (get_line lexbuf, get_col lexbuf) }
| "in"        { T.IN (get_line lexbuf, get_col lexbuf) }
| "end"       { T.END (get_line lexbuf, get_col lexbuf) }
| "function"  { T.FUN (get_line lexbuf, get_col lexbuf) }
| "var"       { T.VAR (get_line lexbuf, get_col lexbuf) }
| "type"      { T.TYPE (get_line lexbuf, get_col lexbuf) }
| "array"     { T.ARR (get_line lexbuf, get_col lexbuf) }
| "if"        { T.IF (get_line lexbuf, get_col lexbuf) }
| "then"      { T.THEN (get_line lexbuf, get_col lexbuf) }
| "else"      { T.ELSE (get_line lexbuf, get_col lexbuf) }
| "do"        { T.DO (get_line lexbuf, get_col lexbuf) }
| "of"        { T.OF (get_line lexbuf, get_col lexbuf) }
| "nil"       { T.NIL (get_line lexbuf, get_col lexbuf) }
| ','         { T.COMMA (get_line lexbuf, get_col lexbuf) }
| ':'         { T.COLON (get_line lexbuf, get_col lexbuf) }
| ';'         { T.SEMI (get_line lexbuf, get_col lexbuf) }
| '('         { T.LPAREN (get_line lexbuf, get_col lexbuf) }
| ')'         { T.RPAREN (get_line lexbuf, get_col lexbuf) }
| '['         { T.LBRACK (get_line lexbuf, get_col lexbuf) }
| ']'         { T.RBRACK (get_line lexbuf, get_col lexbuf) }
| '{'         { T.LBRACE (get_line lexbuf, get_col lexbuf) }
| '}'         { T.RBRACE (get_line lexbuf, get_col lexbuf) }
| '.'         { T.DOT (get_line lexbuf, get_col lexbuf) }
| '+'         { T.PLUS (get_line lexbuf, get_col lexbuf) }
| '-'         { T.MINUS (get_line lexbuf, get_col lexbuf) }
| '*'         { T.TIMES (get_line lexbuf, get_col lexbuf) }
| '/'         { T.DIV (get_line lexbuf, get_col lexbuf) }
| '='         { T.EQ (get_line lexbuf, get_col lexbuf) }
| "<>"        { T.NEQ (get_line lexbuf, get_col lexbuf) }
| '<'         { T.LT (get_line lexbuf, get_col lexbuf) }
| "<="        { T.LE (get_line lexbuf, get_col lexbuf) }
| '>'         { T.GT (get_line lexbuf, get_col lexbuf) }
| ">="        { T.GE (get_line lexbuf, get_col lexbuf) }
| '&'         { T.AND (get_line lexbuf, get_col lexbuf) }
| '|'         { T.OR (get_line lexbuf, get_col lexbuf) }
| ":="        { T.ASSIGN (get_line lexbuf, get_col lexbuf) }
| id          { T.ID (get lexbuf, get_line lexbuf, get_col lexbuf) }
| '"'         { T.STR (string (B.create 80) lexbuf, get_line lexbuf, get_col lexbuf) }
| "/*"        { comment 0 lexbuf }
| eof         { T.EOF }
| _ as c      { error (sprintf "invalid character %c" c) lexbuf }
and string buf = parse
| slvc        {B.add_string buf (get lexbuf)
              ; string buf lexbuf
              }
| "\\t"       { B.add_char buf '\t'
              ; string buf lexbuf
              }
| "\\n"       { B.add_char buf '\n'
              ; string buf lexbuf
              }
| "\\\""      { B.add_char buf '"'
              ; string buf lexbuf
              }
| "\\\\"        { B.add_char buf '\\'
              ; string buf lexbuf
              }
| '\\'        { string_escape buf lexbuf }
| '"'         { B.contents buf }
| eof         { error "found end of file within string literal" lexbuf }
| _ as c      { error (sprintf "invalid character \"%c\" found inside string literal" c) lexbuf }
and string_escape buf = parse
| slnl        { L.new_line lexbuf
              ; string_escape buf lexbuf
              }
| ws          { string_escape buf lexbuf }
| '\\'        { string buf lexbuf }
| eof         { error "found end of file within string literal" lexbuf }
| _ as c      { error (sprintf "invalid character \"%c\" found inside string escape" c) lexbuf }
and comment depth = parse
| nl          { L.new_line lexbuf
              ; comment depth lexbuf
              }
| "/*"        { comment (depth + 1) lexbuf }
| "*/"        { if depth = 0 then token lexbuf else comment (depth - 1) lexbuf }
| _           { comment depth lexbuf }
| eof         { error "found end of file within comment" lexbuf }
