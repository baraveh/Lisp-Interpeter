fun isNumber(x:string) =
  if size(x) = 0 then true
  else
    if Char.isDigit(String.sub(x, 0)) then isNumber((String.extract(x,1, NONE)))
    else false;


fun atoi(x:string) =
    if size(x) = 0 then 0
    else
      ord(String.sub(x, size(x) - 1)) - ord(#"0") + 10*atoi(String.substring(x, 0, size(x) - 1));


fun or(x:bool, y:bool) = if x then true else if y then true else false;


infix or;

fun pad_brackets(x:string) =
  if size(x) = 0 then ""
  else
    if String.sub(x, 0) = #"(" or String.sub(x, 0) = #")" then
      " " ^ str(String.sub(x, 0)) ^ " " ^ pad_brackets(String.extract(x,1, NONE))
    else
      str(String.sub(x, 0)) ^ pad_brackets(String.extract(x,1, NONE));

fun is_space(x:char) = if x = #" " then true else false;

fun tokenize(x:string) = String.tokens is_space (pad_brackets(x));
