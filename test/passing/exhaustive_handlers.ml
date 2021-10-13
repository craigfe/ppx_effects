(* Tests in which the exception and effect handlers provided by the user handle
   all possible types of [exn] and [eff] respectively. *)

let run_and_discard_effect f a =
  match f a with () -> () | [%effect? _, _] -> ()

let run_and_discard_both f a =
  match f a with () -> () | [%effect? _, _] -> () | exception _ -> ()
