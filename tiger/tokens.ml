type token =
  | ID of string * int * int
  | INT of int * int * int
  | STR of string * int * int
  | WHILE of int * int
  | FOR of int * int
  | TO of int * int
  | BRK of int * int
  | LET of int * int
  | IN of int * int
  | END of int * int
  | FUN of int * int
  | VAR of int * int
  | TYPE of int * int
  | ARR of int * int
  | IF of int * int
  | THEN of int * int
  | ELSE of int * int
  | DO of int * int
  | OF of int * int
  | NIL of int * int
  | COMMA of int * int
  | COLON of int * int
  | SEMI of int * int
  | LPAREN of int * int
  | RPAREN of int * int
  | LBRACE of int * int
  | RBRACE of int * int
  | LBRACK of int * int
  | RBRACK of int * int
  | DOT of int * int
  | PLUS of int * int
  | MINUS of int * int
  | TIMES of int * int
  | DIV of int * int
  | EQ of int * int
  | NEQ of int * int
  | LT of int * int
  | LE of int * int
  | GT of int * int
  | GE of int * int
  | AND of int * int
  | OR of int * int
  | ASSIGN of int * int
  | EOF

let string_of_token = function
  | ID (i, l, c) -> "ID(" ^ i ^ ")  " ^ string_of_int l ^ ":" ^ string_of_int c
  | INT (n, l, c) ->
      "INT(" ^ string_of_int n ^ ")  " ^ string_of_int l ^ ":" ^ string_of_int c
  | STR (s, l, c) ->
      "STR(" ^ s ^ ")  " ^ string_of_int l ^ ":" ^ string_of_int c
  | WHILE (l, c) -> "WHILE  " ^ string_of_int l ^ ":" ^ string_of_int c
  | FOR (l, c) -> "FOR  " ^ string_of_int l ^ ":" ^ string_of_int c
  | TO (l, c) -> "TO  " ^ string_of_int l ^ ":" ^ string_of_int c
  | BRK (l, c) -> "BRK  " ^ string_of_int l ^ ":" ^ string_of_int c
  | LET (l, c) -> "LET  " ^ string_of_int l ^ ":" ^ string_of_int c
  | IN (l, c) -> "IN  " ^ string_of_int l ^ ":" ^ string_of_int c
  | END (l, c) -> "END  " ^ string_of_int l ^ ":" ^ string_of_int c
  | FUN (l, c) -> "FUN  " ^ string_of_int l ^ ":" ^ string_of_int c
  | VAR (l, c) -> "VAR  " ^ string_of_int l ^ ":" ^ string_of_int c
  | TYPE (l, c) -> "TYPE  " ^ string_of_int l ^ ":" ^ string_of_int c
  | ARR (l, c) -> "ARR  " ^ string_of_int l ^ ":" ^ string_of_int c
  | IF (l, c) -> "IF  " ^ string_of_int l ^ ":" ^ string_of_int c
  | THEN (l, c) -> "THEN  " ^ string_of_int l ^ ":" ^ string_of_int c
  | ELSE (l, c) -> "ELSE  " ^ string_of_int l ^ ":" ^ string_of_int c
  | DO (l, c) -> "DO  " ^ string_of_int l ^ ":" ^ string_of_int c
  | OF (l, c) -> "OF  " ^ string_of_int l ^ ":" ^ string_of_int c
  | NIL (l, c) -> "NIL " ^ string_of_int l ^ ":" ^ string_of_int c
  | COMMA (l, c) -> "COMMA " ^ string_of_int l ^ ":" ^ string_of_int c
  | COLON (l, c) -> "COLON " ^ string_of_int l ^ ":" ^ string_of_int c
  | SEMI (l, c) -> "SEMI  " ^ string_of_int l ^ ":" ^ string_of_int c
  | LPAREN (l, c) -> "LPAREN  " ^ string_of_int l ^ ":" ^ string_of_int c
  | RPAREN (l, c) -> "RPAREN  " ^ string_of_int l ^ ":" ^ string_of_int c
  | LBRACE (l, c) -> "LBRACE  " ^ string_of_int l ^ ":" ^ string_of_int c
  | RBRACE (l, c) -> "RBRACE  " ^ string_of_int l ^ ":" ^ string_of_int c
  | LBRACK (l, c) -> "LBRACK  " ^ string_of_int l ^ ":" ^ string_of_int c
  | RBRACK (l, c) -> "RBRACK  " ^ string_of_int l ^ ":" ^ string_of_int c
  | DOT (l, c) -> "DOT " ^ string_of_int l ^ ":" ^ string_of_int c
  | PLUS (l, c) -> "PLUS  " ^ string_of_int l ^ ":" ^ string_of_int c
  | MINUS (l, c) -> "MINUS " ^ string_of_int l ^ ":" ^ string_of_int c
  | TIMES (l, c) -> "TIMES " ^ string_of_int l ^ ":" ^ string_of_int c
  | DIV (l, c) -> "DIV " ^ string_of_int l ^ ":" ^ string_of_int c
  | EQ (l, c) -> "EQ  " ^ string_of_int l ^ ":" ^ string_of_int c
  | NEQ (l, c) -> "NEQ " ^ string_of_int l ^ ":" ^ string_of_int c
  | LT (l, c) -> "LT  " ^ string_of_int l ^ ":" ^ string_of_int c
  | LE (l, c) -> "LE  " ^ string_of_int l ^ ":" ^ string_of_int c
  | GT (l, c) -> "GT  " ^ string_of_int l ^ ":" ^ string_of_int c
  | GE (l, c) -> "GE  " ^ string_of_int l ^ ":" ^ string_of_int c
  | AND (l, c) -> "AND " ^ string_of_int l ^ ":" ^ string_of_int c
  | OR (l, c) -> "OR  " ^ string_of_int l ^ ":" ^ string_of_int c
  | ASSIGN (l, c) -> "ASSIGN  " ^ string_of_int l ^ ":" ^ string_of_int c
  | EOF -> "EOF"
