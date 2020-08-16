use "mlisp - hw1.sml";
use "mlisp - hw2.sml";
use "mlisp - hw4.sml";
use "mlisp - hw5.sml";

fun SexpToString (ATOM(NIL)) = "nil"
| SexpToString (ATOM(NUMBER(x))) = (Int.toString x)
| SexpToString (ATOM(SYMBOL(x))) = x
| SexpToString (CONS(car, cdr)) = "(" ^ (SexpToString car) ^ " , " ^ (SexpToString cdr) ^ ")";

fun removeLastChar str = String.substring(str, 0, String.size(str) - 1);

fun repl env =
  let
    val readRes = parse (tokenize (removeLastChar (valOf (TextIO.inputLine TextIO.stdIn))))
    val evalRes = (eval readRes env)
    val printRes = (print("Result = "^SexpToString(#1 evalRes)^"\n"))
  in
    if true then repl (#2 evalRes) else 1
  end;


repl (emptyNestedEnv());
