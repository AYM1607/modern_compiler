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

(* 
 * maxArgs is a function that counts the maximum number of arguments
 * passed to print statement in any sub-statement of an slp program.
 *)
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

(*
 * interpStm interprets an slp program, keeping a list of assigned variables.
 *)
let rec interpStm s t =
    match s, t with
    (* Interpret the expression, take the resulting value and updated symbols list.
     * Add a id value (value resulted from the expression evaluation) pair at the beginning of the updated list.
     *)
    | AssignStm (i, e), tb ->
        let res, tbe = interpExp e tb
        in (i, res)::tbe
    (*
     * Interperd the left stm first, pass the updated table
     * to the call to interpret the right stm.
     *)
    | CompoundStm(l, r), tb ->
        let tbl = interpStm l tb in
        interpStm r tbl
    (*
     * Print stms have side effects.
     * The base case of an empty exp list prints the terminating new line.
     * The exps are evaluated in order (l to r) and each value is printed
     * followed by a space. Tables produced by exp evaluations are passed
     * to the next evaluation.
     *)
    | PrintStm [], tb -> (print_newline(); tb)
    | PrintStm(e::es), tb ->
        let res, tbe = interpExp e tb in
        (print_string (string_of_int res ^ " "); interpStm (PrintStm es) tbe)
and interpExp e t =
    match e, t with
    (*
     * Returns the integer and table directly.
     *)
    | NumExp n, tb -> (n, tb)
    (*
     * Looks up id in the symbols list, returns the same table.
     *)
    | IdExp i, tb -> (lookup i tb, tb)
    (*
     * Evaluates the statement potentially updating the symbols table.
     * The resulting table is used to evaluate the expression.
     *)
    | EseqExp (s, e), tb ->
        let tbs = interpStm s tb in
        interpExp e tbs
    (*
     * The left expression is evaluated first, the resulting table is used
     * to evaluate the right expression.
     * The results from the evaluations are used to perform the operation.
     *)
    | OpExp (l, o, r), tb ->
        let resl, tbl = interpExp l tb in
        let resr, tbr = interpExp r tbl in
        (operate resl o resr, tbr)
