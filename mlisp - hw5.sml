exception MlispError;
local
  fun plusFunction ((ATOM(NUMBER(intA)), nestEnvA), (ATOM(NUMBER(intB)), nestEnvB)) = (ATOM(NUMBER(intA + intB))) |
  plusFunction _ = raise MlispError
  fun minusFunction ((ATOM(NUMBER(intA)), nestEnvA), (ATOM(NUMBER(intB)), nestEnvB)) = (ATOM(NUMBER(intA - intB))) |
  minusFunction _ = raise MlispError
  fun divFunction ((ATOM(NUMBER(intA)), nestEnvA), (ATOM(NUMBER(intB)), nestEnvB)) = (ATOM(NUMBER(intA div intB))) |
  divFunction _ = raise MlispError
  fun mulFunction ((ATOM(NUMBER(intA)), nestEnvA), (ATOM(NUMBER(intB)), nestEnvB)) = (ATOM(NUMBER(intA*intB))) |
  mulFunction _ = raise MlispError
  fun consFunction ((sexpA, envA), (sexpB, envB)) = CONS(sexpA, sexpB);
  fun carFunction (CONS(car, cdr), nestEnv) = (car, nestEnv) |
  carFunction _ = raise MlispError
  fun cdrFunction (CONS(car, cdr), nestEnv) = (cdr, nestEnv) |
  cdrFunction _ = raise MlispError

  fun defineFunction ( ATOM(SYMBOL(bindingName)), CONS(sexp, ATOM(NIL)), nestEnv) = defineFunction( ATOM(SYMBOL(bindingName)), sexp, nestEnv)
  | defineFunction ( ATOM(SYMBOL(bindingName)), sexp, nestEnv) =
    let
      val emptyEnv = (initEnv())
      val newEnv = (pushEnv emptyEnv nestEnv)
    in
      ( (ATOM(NIL)) , (defineNested (bindingName) (newEnv) (sexp)))
    end
  | defineFunction _ = raise MlispError

  fun userFunction(CONS(ATOM(NIL), functionBody), (ATOM(NIL), _), nestEnv) = (functionBody, nestEnv)
  | userFunction ( CONS(CONS( (ATOM(SYMBOL(parameter))) , formalParameters ), functionBody), (CONS(actualParameter, actualParametersList),trashEnv), nestEnv ) =
    let
      val  newEnv = (defineNested (parameter) (nestEnv) (actualParameter))
    in
      userFunction ( CONS(formalParameters, functionBody), (actualParametersList,trashEnv), newEnv)
    end
  | userFunction _ = raise MlispError


in
  fun eval (ATOM(NIL)) nestEnv = (ATOM(NIL), nestEnv)
  | eval (ATOM(NUMBER(int))) nestEnv = (ATOM(NUMBER(int)), nestEnv)
  | eval (ATOM(SYMBOL(x))) nestEnv =  if x = "nil" then (ATOM(NIL), nestEnv) else (( (#1(eval ((find x nestEnv)) nestEnv)) , nestEnv) handle Undefined => raise MlispError)
  | eval ( CONS(a, ATOM(NIL))) nestEnv = (eval a nestEnv)
  | eval (CONS((ATOM(SYMBOL(functionName))), (CONS(car, cdr)))) nestEnv =
  if functionName = "+" then ( (plusFunction( (eval car nestEnv) , (eval cdr nestEnv) ) ) ,   nestEnv)
  else if functionName = "-" then  ( (minusFunction( (eval car nestEnv) , (eval cdr nestEnv) ) ) ,   nestEnv)
  else if functionName = "div" then ( (divFunction( (eval car nestEnv) , (eval cdr nestEnv) ) ) ,   nestEnv)
  else if functionName = "*" then ( (mulFunction( (eval car nestEnv) , (eval cdr nestEnv) ) ) ,   nestEnv)
  else if functionName = "cons" then ((consFunction( (eval car nestEnv) , (eval cdr nestEnv))), nestEnv)
  else if functionName = "car" then ( (carFunction((eval (CONS(car, cdr)) nestEnv ))))
  else if functionName = "cdr" then ( (cdrFunction((eval (CONS(car, cdr)) nestEnv ))))
  else if functionName = "define" then (defineFunction(car, cdr, nestEnv))
  else
    let
      val CONS(formalParameters, functionBody) = (((find functionName nestEnv)) handle Undefined => raise MlispError)

      fun buildEnviroment(ATOM(NIL), _ , origEnv) = origEnv
      | buildEnviroment( CONS(ATOM(SYMBOL(parameter)), formalParameters), CONS(CONS(actualParameter, ATOM(NIL)), actualParametersList), origEnv) =
          buildEnviroment(formalParameters, actualParametersList, (defineNested parameter origEnv (actualParameter) ))
      | buildEnviroment( CONS(ATOM(SYMBOL(parameter)), formalParameters), CONS(actualParameter, actualParametersList), origEnv) =
          buildEnviroment(formalParameters, actualParametersList, (defineNested parameter origEnv (actualParameter)))
      |buildEnviroment(ATOM(SYMBOL(name)), sexp, origEnv) = (defineNested name origEnv (#1(eval sexp origEnv)))
      |buildEnviroment _ = raise MlispError

      val modifiedEnv = buildEnviroment(formalParameters, CONS(car, cdr), (pushEnv (initEnv()) (nestEnv) ) )
      val (res, envStack) = (eval functionBody modifiedEnv)
    in
      (res, nestEnv)
    end
  | eval sexp env  = raise MlispError

end
