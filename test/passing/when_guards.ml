exception%effect Foo : bool -> unit

let _ =
  match false with
  | _ when true -> 'a'
  | _ -> 'b'
  | exception Not_found -> 'c'
  | exception Failure s when String.equal s "Oops" -> 'd'
  | [%effect? Foo b, _] when b -> 'e'
  | [%effect? Foo b, _ when not b] -> 'e'
  | [%effect? Foo _, k] -> EffectHandlers.Deep.continue k ()
