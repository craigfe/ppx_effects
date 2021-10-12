## `ppx_effects` — WIP syntax extensions for untyped effects in OCaml 5.

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2FCraigFe%2Fppx_effects%2Fmain&logo=ocaml)](https://ci.ocamllabs.io/github/CraigFe/ppx_effects)

- `effect E : string -> int` is written as `exception%effect E : string -> int`
- `| effect (E _) k ->` is written as `| [%effect? (E _), k] ->`
