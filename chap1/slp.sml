type Id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of Id * exp
	     | PrintStm of exp list

     and exp = IdExp of Id
	     | NumExp of int
         | OpExp of exp * binop * exp
         | EseqExp of stm * exp


(* The following program is a tree representation of:
 * a := 5+3; b := (print (a,a - 1), 10*a); print(b)
 *)
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

(* 
 * maxArgs is a function that counts the maximum number of arguments
 * passed to print statement in any sub-statement of an slp program.
 *)
fun maxArgs (CompoundStm(l, r)) = Int.max(maxArgs l, maxArgs r)
  | maxArgs (PrintStm(es)) = Int.max(length es, expsMaxArgs es)
  | maxArgs (AssignStm(_, e)) = expMaxArgs e

and expMaxArgs (EseqExp(s, e)) = Int.max(expMaxArgs e, maxArgs s)
  | expMaxArgs (OpExp(l, _, r)) = Int.max(expMaxArgs l, expMaxArgs r)
  | expMaxArgs (IdExp(_)) = 0
  | expMaxArgs (NumExp(_)) = 0

and expsMaxArgs ([]) = 0
  | expsMaxArgs (e::es) = Int.max(expMaxArgs e, expsMaxArgs es)

(*
 * Helper functions for the interpreter.
 *)
fun lookup (_, []) = raise Empty
  | lookup (idl, (id, v)::s) = if idl = id then v else lookup(idl, s)

fun operate (l, Plus, r) = l + r
  | operate (l, Minus, r) = l - r
  | operate (l, Times, r) = l * r
  | operate (l, Div, r) = l div r

(*
 * interpStm interprets an slp program, keeping a list of assigned variables.
 *)
(* Interpret the expression, take the resulting value and updated symbols list.
 * Add a id value (value resulted from the expression evaluation) pair at the beginning of the updated list.
 *)
fun interpStm (AssignStm(id, e), tb) =
    let val (res, tbe) = interpExp (e, tb)
    in (id, res)::tbe
    end
    (*
     * Interperd the left stm first, pass the updated table
     * to the call to interpret the right stm.
     *)
  | interpStm (CompoundStm(l, r), tb) =
    let val tbl = interpStm (l, tb)
    in interpStm (r, tbl)
    end
    (*
     * Print stms have side effects.
     * The base case of an empty exp list prints the terminating new line.
     * The exps are evaluated in order (l to r) and each value is printed
     * followed by a space. Tables produced by exp evaluations are passed
     * to the next evaluation.
     *)
  | interpStm (PrintStm([]), tb) = (print "\n"; tb)
  | interpStm (PrintStm(e::es), tb) =
    let val (res, tbe) = interpExp (e, tb)
    in (print (Int.toString res ^ " "); interpStm (PrintStm es, tbe))
    end
    (*
     * Looks up id in the symbols list, returns the same table.
     *)
and interpExp (IdExp(id), tb) = (lookup(id, tb), tb)
    (*
     * Returns the integer and table directly.
     *)
  | interpExp (NumExp(n), tb) = (n, tb)
    (*
     * Evaluates the statement potentially updating the symbols table.
     * The resulting table is used to evaluate the expression.
     *)
  | interpExp (EseqExp(s, e), tb) =
    let val tbs = interpStm (s, tb)
    in interpExp(e, tbs)
    end
    (*
     * The left expression is evaluated first, the resulting table is used
     * to evaluate the right expression.
     * The results from the evaluations are used to perform the operation.
     *)
  | interpExp (OpExp(l, opr, r), tb) =
    let val (resl, tbl) = interpExp(l, tb)
        val (resr, tbr) = interpExp(r, tbl)
    in (operate (resl, opr, resr), tbr)
    end
