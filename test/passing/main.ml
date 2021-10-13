open Stdlib.EffectHandlers
open Stdlib.EffectHandlers.Deep

exception%effect E: string

let comp () =
  print_string "0 ";
  print_string (perform E);
  print_string "3 "

let () =
  try comp ()
  with [%effect? E, k] ->
    print_string "1 ";
    continue k "2 ";
    print_string "4 "

let () =
  match comp () with
  | e -> e
  | [%effect? E, k] ->
      print_string "1 ";
      continue k "2 ";
      print_string "4 "
