(lang dune 2.9)
(generate_opam_files true)

(name ppx_effects)
(source (github CraigFe/ppx_effects))
(license MIT)
(authors "Craig Ferguson")
(maintainers "me@craigfe.io")

(package
 (name ppx_effects)
 (synopsis "PPX syntax for untyped effects in OCaml 5.0")
 (description "PPX syntax for untyped effects in OCaml 5.0")
 (documentation "https://craigfe.github.io/ppx-effects")
 (depends
  (ocaml-base-compiler (>= 5.0))
  (ppxlib (and (>= 0.12.0)))))
