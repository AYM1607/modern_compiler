type Id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
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

