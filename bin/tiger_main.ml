module L = Lexing
module TL = Tiger.Lex
module TO = Tiger.Tokens

let main () =
  let lexbuf = L.from_channel stdin in
  let rec loop () =
    let tok = TL.token lexbuf in
    match tok with
    | TO.EOF -> print_endline "EOF"
    | x ->
        print_endline @@ TO.string_of_token x;
        loop ()
  in
  loop ()

let () = main ()
