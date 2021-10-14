## `ppx_effects` – syntax extensions for untyped effects in OCaml 5.0

OCaml 5.0 will ship with support for [_effects_][effects-tutorial]! :tada:

However, since the effect implementation is currently untyped, the compiler
doesn't yet provide any dedicated syntax to support defining or handling
effects. This PPX provides a close approximation to the _proposed_ syntax,
hopefully making it simpler to use effects in your OCaml 5.0 code (and easing
future migrations to a dedicated syntax).

[effects-tutorial]: https://github.com/ocamllabs/ocaml-effects-tutorial

**STATUS: EXPERIMENTAL**

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2FCraigFe%2Fppx_effects%2Fmain&logo=ocaml)](https://ci.ocamllabs.io/github/CraigFe/ppx_effects)

## Usage

In short:

- **Declaring effects**: `effect E : string -> int` is written as `exception%effect E : string -> int`
- **Handling effects**: `| effect (E _) k ->` is written as `| [%effect? (E _), k] ->`

See the result of porting this PPX to various effectful repositories here:

- [ocaml-multicore/effects-examples](https://github.com/CraigFe/effects-examples/pull/1)
- [ocamllabs/ocaml-effects-tutorial](https://github.com/CraigFe/ocaml-effects-tutorial/pull/1)

## Install

This library has not yet been released to `opam`. To install it, first 

```
opam pin add --yes https://github.com/CraigFe/ppx_effects.git
opam install ppx_effects
```

Users of [`dune`](https://github.com/ocaml/dune/) can then use this PPX on their
libraries and executables by adding the appropriate stanza field:

```lisp
(library
 ...
 (preprocess (pps ppx_effects)))
```

## Details

Using the PPX should ideally be exactly like using the dedicated syntax.
However, there are a few implementation details that can leak to PPX users:

- the expansion of `match` / `try` expressions containing top-level `[%effect?
  ...]` patterns introduces a locally-abstract type named `continue_input`
  representing the type of values passed to `continue` (and returned from a
  suspended `perform`). This type name can appear in error messages, but
  shouldn't be referred to from user code. (If you find you _do_ need this type
  name for some reason, raise an issue on this repository!)
  
- in order to use the low-level effects API provided by the compiler, an
  effectful computation being `match`-ed or `try`-ed must be wrapped in an
  allocated thunk (e.g. `fun () -> ...`). This thunk has a small performance
  cost, so very performance-critical code should arrange to make this expression
  a simple unary function application (as in, `match f x with` or `try f x
  with`) instead – this avoids needing to allocate the thunk.
