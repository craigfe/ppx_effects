exception%effect E : string

let f () = Stdlib.Effect.perform E
let g () = f

(* If the scrutinee is already of the form [f x], where [f] and [x] are both
   values, use [f] directly as the effect-raising function. *)
let _ = try f () with [%effect? E, _] -> ""

(* Negative test cases: *)

(* - [f] or [x] is not a value *)
let _ = try (Fun.id f) () with [%effect? E, _] -> ""
let _ = try f (Fun.id ()) with [%effect? E, _] -> ""

(* - the scrutinee isn't of the form [f x]: *)
let _ = try g () () with [%effect? E, _] -> ""

let _ =
  try
    let () = () in
    f ()
  with [%effect? E, _] -> ""
