type id = string

type binop = Plus
    | Minus
    | Times
    | Div

type stm = CompoundStm of stm * stm
    | AssignStm of id * exp
    | PrintStm of exp list
and exp = IdExp of id
    | NumExp of int
    | OpExp of exp * binop * exp
    | EseqExp of stm * exp

(*
    The following is a pree representatino of:
    a := 5+3; b := (print (a, a - 1), 10 * a); print (b)
*)
let prog =
    CompoundStm(
        AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
        CompoundStm(
            AssignStm(
                "b",
                EseqExp(
                    PrintStm [IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)],
                    OpExp(NumExp 10, Times, IdExp "a")
                )
            ),
            PrintStm [IdExp "b"]
        )
    )

let rec maxArgs = function
    | CompoundStm (l, r) -> Int.max (maxArgs l) (maxArgs r)
    | PrintStm es -> Int.max (List.length es) (expsMaxArgs es)
    | AssignStm (_, e) -> expMaxArgs e
and expMaxArgs = function
    | IdExp _ -> 0
    | NumExp _ -> 0
    | EseqExp (s, e) -> Int.max (maxArgs s) (expMaxArgs e)
    | OpExp (l, _, r) -> Int.max (expMaxArgs l) (expMaxArgs r)
and expsMaxArgs = function
    | [] -> 0
    | e :: es -> Int.max (expMaxArgs e) (expsMaxArgs es)

(*
Helper functions for the interpreter.
*)
exception NoBinding
let rec lookup i l =
    match i, l with
    | (_, []) -> raise NoBinding
    | (il, (i, v)::s) -> if il = i then v else (lookup il s)
let operate l o r =
    match l, o, r with
    | l, Plus, r -> l + r
    | l, Minus, r -> l - r
    | l, Times, r -> l * r
    | l, Div, r -> l / r

let rec interpStm s t =
    match s, t with
    | AssignStm (i, e), tb ->
        let res, tbe = interpExp e tb
        in (i, res)::tbe
    | CompoundStm(l, r), tb ->
        let tbl = interpStm l tb in
        interpStm r tbl
    | PrintStm [], tb -> (print_newline(); tb)
    | PrintStm(e::es), tb ->
        let res, tbe = interpExp e tb in
        (print_string (string_of_int res ^ " "); interpStm (PrintStm es) tbe)
and interpExp e t =
    match e, t with
    | NumExp n, tb -> (n, tb)
    | IdExp i, tb -> (lookup i tb, tb)
    | EseqExp (s, e), tb ->
        let tbs = interpStm s tb in
        interpExp e tbs
    | OpExp (l, o, r), tb ->
        let resl, tbl = interpExp l tb in
        let resr, tbr = interpExp r tbl in
        (operate resl o resr, tbr)
