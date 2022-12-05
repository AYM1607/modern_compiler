open Printf

exception LexingError

let error msg line col =
  printf "Error while lexing program at %d:%d : %s\n" line col msg;
  raise LexingError
