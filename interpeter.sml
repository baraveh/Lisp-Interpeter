use "mlisp - hw1.sml";
use "mlisp - hw2.sml";
use "mlisp - hw4.sml";
use "mlisp - hw5.sml";

fun printSexp (ATOM(NIL)) = "\n"
| printSexp (ATOM(NUMBER(x))) = ((Int.toString x) ^ "\n")
| printSexp (ATOM(SYMBOL(x))) = (x ^ "\n")
| printSexp (CONS(car, cdr)) = "(" ^ (printSexp car) ^ "\t" ^ (printSexp cdr) ^ ")";

fun repl() =
  let
    val readRes = parse (tokenize (valOf (TextIO.inputLine TextIO.stdIn)) )
    val evalRes = eval readRes (emptyNestedEnv())
  in
    if true then repl (print(printSexp(#1 evalRes))) else 1
  end;


repl();
