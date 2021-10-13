(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open Ppxlib
open Ast_builder.Default

let namespace = "ppx_effects"
let pp_quoted ppf s = Format.fprintf ppf "‘%s’" s
let raise_errorf ~loc fmt = Location.raise_errorf ~loc ("%s: " ^^ fmt) namespace

(** Cases of [match] / [try] can be partitioned into three categories:

    - exception patterns (uing the [exception] keyword);
    - effect patterns (written using [\[%effect? ...\]]);
    - return patterns (available only to [match]).

    The [Obj.Effect_handlers] API requires passing different continuations for
    each of these categories. *)
module Cases = struct
  type partitioned = { ret : cases; exn : cases; eff : cases }

  let partition : map_subnodes:Ast_traverse.map -> cases -> partitioned =
   fun ~map_subnodes cases ->
    ListLabels.fold_right cases ~init:{ ret = []; exn = []; eff = [] }
      ~f:(fun case acc ->
        match case.pc_lhs with
        | [%pat? [%effect? [%p? eff_pattern], [%p? k_pattern]]] ->
            let pc_rhs =
              let loc = case.pc_rhs.pexp_loc in
              [%expr
                Some
                  (fun ([%p k_pattern] :
                         (a, _) Obj.Effect_handlers.Deep.continuation) ->
                    [%e map_subnodes#expression case.pc_rhs])]
            in
            let case =
              { pc_lhs = eff_pattern; pc_rhs; pc_guard = case.pc_guard }
            in
            { acc with eff = case :: acc.eff }
        | [%pat? exception [%p? exn_pattern]] ->
            let pc_rhs = map_subnodes#expression case.pc_rhs in
            let case =
              { pc_lhs = exn_pattern; pc_rhs; pc_guard = case.pc_guard }
            in
            { acc with exn = case :: acc.exn }
        | _ ->
            (* TODO: handle guards on effects and exceptions properly *)
            { acc with ret = case :: acc.ret })

  let contain_effect_handler : cases -> bool =
    List.exists (fun case ->
        match case.pc_lhs with
        | [%pat? [%effect? [%p? _]]] -> true
        | [%pat? [%effect [%e? _]]] ->
            (* The user made a mistake and forgot to add [?] after [effect] (this
               node captures expressions rather than patterns). *)
            raise_errorf ~loc:case.pc_lhs.ppat_loc
              "invalid node %a used as a pattern.@,\
               Hint: did you mean to use %a instead?" pp_quoted
              "[%effect <expr>]" pp_quoted "[%effect? <pattern>]"
        | _ -> false)
end

(** The [Obj.Effect_handlers] API requires effects to happen under a function
    application *)
module Scrutinee = struct
  type delayed = { function_ : expression; argument : expression }

  (* An expression is a syntactic value if its AST structure precludes it from
     raising an effect or an exception. Here we use a very simple
     under-approximation (avoiding multiple recursion): *)
  let rec expr_is_syntactic_value (expr : expression) : bool =
    match expr.pexp_desc with
    | Pexp_ident _ | Pexp_constant _ | Pexp_function _ | Pexp_fun _
    | Pexp_construct (_, None)
    | Pexp_variant (_, None)
    | Pexp_field _ | Pexp_lazy _ ->
        true
    | Pexp_let _ | Pexp_apply _ | Pexp_match _ | Pexp_try _ | Pexp_tuple _
    | Pexp_record _ | Pexp_setfield _ | Pexp_array _ | Pexp_ifthenelse _
    | Pexp_sequence _ | Pexp_while _ | Pexp_for _ | Pexp_new _ | Pexp_override _
    | Pexp_letmodule _ | Pexp_object _ | Pexp_pack _ | Pexp_letop _
    | Pexp_extension _ | Pexp_unreachable ->
        false
    (* Congruence cases: *)
    | Pexp_constraint (e, _)
    | Pexp_coerce (e, _, _)
    | Pexp_construct (_, Some e)
    | Pexp_variant (_, Some e)
    | Pexp_send (e, _)
    | Pexp_setinstvar (_, e)
    | Pexp_letexception (_, e)
    | Pexp_assert e
    | Pexp_newtype (_, e)
    | Pexp_open (_, e) ->
        expr_is_syntactic_value e
    | Pexp_poly _ -> assert false

  let of_expression = function
    | [%expr [%e? function_] [%e? argument]]
      when expr_is_syntactic_value function_ && expr_is_syntactic_value argument
      ->
        { function_; argument }
    | e ->
        (* If the expression is not already of the form [f x] then we must
           allocate a thunk to delay the effect. *)
        let loc = e.pexp_loc in
        (* NOTE: here we use [`unit] over [()] in case the user has
           shadowed the unit constructor. *)
        let function_ = [%expr fun `unit -> [%e e]] in
        let argument = [%expr `unit] in
        { function_; argument }
end

(* Both [exnc] and [effc] require a noop case to represent an unhandled
   exception or effect respectively. [exnc] reraises the unhandled exception,
   and [effc] returns None.

   Caveat: it's possible that a noop case is not needed becuase the user's
   handler is exhaustive, resulting in an unwanted "redundant case" warning.
   We get around this by checking whether the users' cases are syntactically
   exhaustive and not adding the noop case if so.

   It'd be nice to solve this by just locally disabling the redundant case
   warning with [[@warning "-11"]], but this would have to go on the entire
   match (in which case it leaks the users' subexpressions). Unfortunately,
   OCaml doesn't support [[@warning "-11"]] on individual patterns. *)
let extensible_cases_are_exhaustive : cases -> bool =
  let pattern_matches_anything p =
    match p.ppat_desc with Ppat_any | Ppat_var _ -> true | _ -> false
  in
  List.exists (fun case ->
      Option.is_none case.pc_guard && pattern_matches_anything case.pc_lhs)

(* Given a list of effect handlers, build a corresponding [effc] continuation to
   pass to [Deep.{try,match}_with]. *)
let effc ~loc (cases : cases) : expression =
  assert (cases <> []);
  let noop_case =
    match extensible_cases_are_exhaustive cases with
    | true -> []
    | false -> [ case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr None] ]
  in
  [%expr
    fun (type a) (effect : a Obj.Effect_handlers.eff) ->
      [%e pexp_match ~loc [%expr effect] (cases @ noop_case)]]

(* Given a list of exception handlers, build a corresponding [exnc] continuation
   to pass to [Deep.{try,match}_with]. *)
let exnc ~loc (cases : cases) : expression =
  match cases with
  | [] -> [%expr raise] (* TODO: defend against shadowing raise *)
  | _ :: _ ->
      let noop_case =
        match extensible_cases_are_exhaustive cases with
        | true -> []
        | false -> [ case ~lhs:[%pat? e] ~guard:None ~rhs:[%expr raise e] ]
      in
      pexp_function ~loc (cases @ noop_case)

(* Captures top-level [%effect? _] in [try] / [match] expressions and converts
   them to [Deep.{try,match}_with].

   Also handles [exception%effect ...] in structures – see below. *)
let impl : structure -> structure =
  (object (this)
     inherit Ast_traverse.map as super

     method! expression expr =
       let loc = expr.pexp_loc in
       match expr with
       (* Handles: [ match _ with [%effect? E _, k] -> ... ] *)
       | { pexp_desc = Pexp_match (scrutinee, cases); _ }
         when Cases.contain_effect_handler cases ->
           let scrutinee =
             Scrutinee.of_expression (this#expression scrutinee)
           in
           let cases = Cases.partition ~map_subnodes:this cases in
           let retc = pexp_function ~loc cases.ret
           and exnc = exnc ~loc cases.exn
           and effc = effc ~loc cases.eff in
           [%expr
             Obj.Effect_handlers.Deep.match_with [%e scrutinee.function_]
               [%e scrutinee.argument]
               { retc = [%e retc]; exnc = [%e exnc]; effc = [%e effc] }]
       (* Handles: [ try _ with [%effect? E _, k] -> ... ] *)
       | { pexp_desc = Pexp_try (scrutinee, cases); _ }
         when Cases.contain_effect_handler cases ->
           let scrutinee =
             Scrutinee.of_expression (this#expression scrutinee)
           in
           let cases = Cases.partition ~map_subnodes:this cases in
           let effc = effc ~loc cases.eff in
           [%expr
             Obj.Effect_handlers.Deep.try_with [%e scrutinee.function_]
               [%e scrutinee.argument]
               { effc = [%e effc] }]
       | e -> super#expression e

     method! extension =
       function
       | { txt = "effect"; loc }, _ ->
           raise_errorf ~loc
             "dangling [%%effect ...] extension node. This node may be used as:\n\
             \ - the top level of %a or %a patterns as %a\n\
             \ - on an exception definition as %a." pp_quoted "match" pp_quoted
             "try" pp_quoted "[%effect? ...]" pp_quoted "exception%effect ..."
       | e -> super#extension e
  end)
    #structure

let effect_decl_of_exn_decl ~loc (exn : type_exception) : type_extension =
  let name = exn.ptyexn_constructor.pext_name in
  let eff_type = Located.lident ~loc "Obj.Effect_handlers.eff" in
  let constrs, args =
    match exn.ptyexn_constructor.pext_kind with
    | Pext_decl (constrs, body) ->
        let body =
          Option.map (fun typ -> ptyp_constr ~loc eff_type [ typ ]) body
        in
        (constrs, body)
    | Pext_rebind _ ->
        raise_errorf ~loc "cannot process effect defined as an alias of %a."
          pp_quoted name.txt
  in
  let params = [ (ptyp_any ~loc, (NoVariance, NoInjectivity)) ] in
  type_extension ~loc ~path:eff_type ~params
    ~constructors:
      [ extension_constructor ~loc ~name ~kind:(Pext_decl (constrs, args)) ]
    ~private_:Public

let str_effect_decl =
  Extension.declare "effect" Structure_item
    Ast_pattern.(pstr (pstr_exception __ ^:: nil))
    (fun ~loc ~path:_ exn ->
      pstr_typext ~loc (effect_decl_of_exn_decl ~loc exn))

let sig_effect_decl =
  Extension.declare "effect" Signature_item
    Ast_pattern.(psig (psig_exception __ ^:: nil))
    (fun ~loc ~path:_ exn ->
      psig_typext ~loc (effect_decl_of_exn_decl ~loc exn))

let () =
  Reserved_namespaces.reserve namespace;
  Driver.register_transformation
    ~extensions:[ str_effect_decl; sig_effect_decl ]
    ~impl namespace

(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2021 Craig Ferguson <me@craigfe.io>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
  ————————————————————————————————————————————————————————————————————————————*)
