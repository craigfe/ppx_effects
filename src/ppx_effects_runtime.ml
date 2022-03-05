(** These functions are exported for use by the [ppx_effects] PPX. They are not
    intended to be called directly by users. *)

let raise e =
  let bt = Printexc.get_raw_backtrace () in
  Printexc.raise_with_backtrace e bt

open Stdlib.Effect.Deep

type nonrec ('a, 'b) handler = ('a, 'b) handler = {
  retc : 'a -> 'b;
  exnc : exn -> 'b;
  effc : 'c. 'c Effect.t -> (('c, 'b) continuation -> 'b) option;
}

type nonrec 'a effect_handler = 'a effect_handler = {
  effc : 'b. 'b Effect.t -> (('b, 'a) continuation -> 'a) option;
}

let match_with : type a b c. (a -> b) -> a -> (b, c) handler -> c = match_with
let try_with : type a b. (a -> b) -> a -> b effect_handler -> b = try_with
