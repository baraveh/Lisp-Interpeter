exception Undefined;
exception Empty;


fun initEnv () = fn (str:string) => raise Undefined;

fun define (newstr: string) (env) (value) = fn (str: string) =>
  if str = newstr then value
  else env str;

fun emptyNestedEnv () = [initEnv ()]

fun pushEnv env envList   = env :: envList;
fun popEnv envList =
  if List.length envList = 0 then raise Empty
  else List.drop(envList, 1);
fun topEnv envList =
  if List.length envList = 0 then raise Empty
  else List.hd envList;

fun defineNested (str: string) (stack) (value) =
    let
      val top = topEnv stack
      val newStack = popEnv stack
    in
      pushEnv (define str top value) newStack
    end;

fun find (str:string) envStack =
    (topEnv envStack) str
    handle Undefined => find str (popEnv envStack)
    handle Empty => raise Undefined;
