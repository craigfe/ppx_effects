(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open Ppxlib
open Ast_builder.Default

let namespace = "ppx_effects"
let pp_quoted ppf s = Format.fprintf ppf "‘%s’" s

module Cases = struct
  type partitioned = { ret : cases; exn : cases; eff : cases }

  let partition : cases -> partitioned =
    ListLabels.fold_right ~init:{ ret = []; exn = []; eff = [] }
      ~f:(fun case acc ->
        match case.pc_lhs with
        | [%pat? [%effect? [%p? eff_pattern], [%p? k_pattern]]] ->
            let case =
              {
                case with
                pc_lhs = eff_pattern;
                pc_rhs =
                  (let loc = case.pc_rhs.pexp_loc in
                   [%expr
                     Some
                       (fun ([%p k_pattern] :
                              (a, _) Obj.Effect_handlers.Deep.continuation) ->
                         [%e case.pc_rhs])]);
              }
            in
            { acc with eff = case :: acc.eff }
        | [%pat? exception [%p? exn_pattern]] ->
            let case = { case with pc_lhs = exn_pattern } in
            { acc with exn = case :: acc.exn }
        | _ ->
            (* TODO: handle guards on effects and exceptions properly *)
            { acc with ret = case :: acc.ret })

  let contain_effect_handler : cases -> bool =
    List.exists (fun case ->
        match case.pc_lhs with [%pat? [%effect? [%p? _]]] -> true | _ -> false)
end

let effc ~loc cases =
  [%expr
    fun (type a) (effect : a Obj.Effect_handlers.eff) ->
      [%e
        pexp_match ~loc [%expr effect]
          (cases @ [ case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr None] ])]]

let impl : structure -> structure =
  (* Capture [try] / [match] blocks containing top-level [effects]. *)
  (object (this)
     inherit Ast_traverse.map as super

     method! expression expr =
       let loc = expr.pexp_loc in
       match expr with
       (* match _ with [%effect? E _, k] -> ... *)
       | { pexp_desc = Pexp_match (scrutinee, cases); _ }
         when Cases.contain_effect_handler cases ->
           let scrutinee = super#expression scrutinee in
           let cases = Cases.partition cases in
           let expand_cases_rhs =
             List.map (fun case ->
                 { case with pc_rhs = this#expression case.pc_rhs })
           in
           let retc = pexp_function ~loc (cases.ret |> expand_cases_rhs)
           and exnc =
             pexp_function ~loc
               ((cases.exn |> expand_cases_rhs)
               @ [ case ~lhs:[%pat? e] ~guard:None ~rhs:[%expr raise e] ])
           and effc = effc ~loc (cases.eff |> expand_cases_rhs) in
           [%expr
             Obj.Effect_handlers.Deep.match_with
               (fun () -> [%e scrutinee])
               ()
               { retc = [%e retc]; exnc = [%e exnc]; effc = [%e effc] }]
       (* try _ with [%effect? E _, k] -> ... *)
       | { pexp_desc = Pexp_try (scrutinee, cases); _ }
         when Cases.contain_effect_handler cases ->
           let cases = Cases.partition cases in
           let expand_cases_rhs =
             List.map (fun case ->
                 { case with pc_rhs = this#expression case.pc_rhs })
           in
           let effc = effc ~loc (cases.eff |> expand_cases_rhs) in
           [%expr
             Obj.Effect_handlers.Deep.try_with
               (fun () -> [%e scrutinee])
               ()
               { effc = [%e effc] }]
       | e -> super#expression e

     method! structure_item stri =
       let loc = stri.pstr_loc in
       match stri with
       | [%stri [%%effect [%%i? { pstr_desc = Pstr_exception exn; _ }]]] ->
           (* TODO: handle attributes on the extension? *)
           let name = exn.ptyexn_constructor.pext_name in
           let eff_type = Located.lident ~loc "Obj.Effect_handlers.eff" in
           let constrs, args =
             match exn.ptyexn_constructor.pext_kind with
             | Pext_decl (constrs, body) ->
                 let body =
                   Option.map
                     (fun typ -> ptyp_constr ~loc eff_type [ typ ])
                     body
                 in
                 (constrs, body)
             | Pext_rebind _ ->
                 Location.raise_errorf ~loc
                   "%s: cannot process effect defined as an alias of %a."
                   namespace pp_quoted name.txt
           in
           let params = [ (ptyp_any ~loc, (NoVariance, NoInjectivity)) ] in
           pstr_typext ~loc
             (type_extension ~loc ~path:eff_type ~params
                ~constructors:
                  [
                    extension_constructor ~loc ~name
                      ~kind:(Pext_decl (constrs, args));
                  ]
                ~private_:Public)
       | s -> super#structure_item s

     method! extension =
       function
       | { txt = "effect"; loc }, _ ->
           Location.raise_errorf ~loc
             "%s: dangling [%%effect ...] extension node. This node may be \
              used in the top level of %a or %a patterns as %a, or on an \
              exception definition as %a."
             namespace pp_quoted "match" pp_quoted "try" pp_quoted
             "[%effect? ...]" pp_quoted "exception%effect ..."
       | e -> super#extension e
  end)
    #structure

let () =
  Reserved_namespaces.reserve namespace;
  Driver.register_transformation ~impl namespace

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
