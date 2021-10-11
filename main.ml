type _ eff += E : tring eff

let comp () =
  print_string "0 ";
  print_string (perform E);
  print_string "3 "

let raise f = f

let main () =
  try comp ()
  with [%effect? E, _] ->
    print_string "1 ";
    continue k "2 ";
    print_string "4 "
