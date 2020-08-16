datatype Atom =
   SYMBOL of string | NUMBER of int | NIL;
datatype SExp =
    ATOM of Atom | CONS of (SExp * SExp);

local
    fun helper_balance(str:string, num_of_opens:int, num_of_closes:int, index:int) =

    if index = size(str) then
    	if num_of_closes = num_of_opens then true
    	else false

    else if num_of_opens >= num_of_closes then
    	if String.sub(str, index) = #"("
    		then helper_balance(str, num_of_opens + 1, num_of_closes, index + 1)
    	else if String.sub(str, index) = #")"
    		then helper_balance(str, num_of_opens, num_of_closes + 1, index + 1)
    	else
    		helper_balance(str, num_of_opens, num_of_closes, index + 1)
    else
    	false
    fun balance(x:string) = helper_balance(x,0,0,0)

  fun concat stringList = foldr op ^ "" stringList;

  fun findCarCdrHelper tokenList i =
      if balance( (concat( List.take(tokenList, i) )) ) then (List.take(tokenList, i), List.drop(tokenList, i)) else findCarCdrHelper tokenList (i + 1)
  fun findCarCdr tokenList= (findCarCdrHelper tokenList 1)

  fun atomize tokenList =
    let
      val token = List.hd tokenList
    in
      if isNumber(token) then ATOM(NUMBER(atoi(token))) else ATOM(SYMBOL((token)))
    end


  fun removeParenthesis tokenList = List.drop ((List.take (tokenList, (List.length(tokenList) - 1)) ), 1)

  fun parse_helper [] = ATOM (NIL) |
  parse_helper tokenList =
    let
      val tuple = findCarCdr tokenList
      val car = #1 tuple
      val cdr = #2 tuple
    in
      if List.length(car) = 1 then CONS(atomize(car), parse_helper(cdr)) else
      CONS(parse_helper( (removeParenthesis(car)) ), parse_helper(cdr))
    end

in

  fun parse tokenList = parse_helper(removeParenthesis(tokenList))

end;
