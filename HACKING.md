# Hacking on Flambda backend project for OCaml

This page is intended to keep track of useful information for people who
want to modify the Flambda backend.


## Rebuilding during dev work

To rebuild after making changes, you can just type `make` (or `make -j16`, etc).

There is a special target `make hacking` which starts Dune in polling mode.  The rebuild
performed here is equivalent to `make ocamlopt` in the upstream distribution: it rebuilds the
compiler itself, but doesn't rebuild the stdlib or anything else with the new compiler.
This target is likely what you want for development of large features in the middle end or
backend.  Rebuild times for this target should be very fast.  (`make hacking` can be
run directly after `configure`, there is no need to do a full `make` first.)

The aim is to minimise patches against the upstream compiler (the
contents of the ocaml/ subdirectory), but you can configure and build
in that directory as you would for upstream.  If a bootstrap is
required, the normal bootstrapping commands should also work: from
within the ocaml/ subdirectory, follow the instructions in
[ocaml/BOOTSTRAP.adoc](ocaml/BOOSTRAP.adoc); the newly-bootstrapped
compiler will be picked up the next time that the Flambda backend is
built from the toplevel directory of the checkout.

Any changes in `ocaml/asmcomp` and `ocaml/middle_end` directories
should also be applied to the corresponding directories `backend` and
`middle_end`.


## Running the compiler produced by "make hacking" on an example without the stdlib

For small examples that don't need the stdlib or any other library provided by the
compiler distribution, it suffices to have run `make hacking`, followed by
something like:
```
./_build1/default/flambda_backend_main_native.exe -nostdlib -nopervasives -c test.ml
```
(The `flambda_backend_main_native.exe` executable is the one that ends up as `ocamlopt.opt` in
the installation directory.)

## Getting the compilation command for a stdlib file

For example because you need to get the `-dflambda` output because of a bug.
```
rm -f _build2/default/ocaml/stdlib/.stdlib.objs/native/std_exit.cmx
PATH=<FLAMBDA_BACKEND>/_build1/install/default/bin:$PATH <DUNE> build --profile=release --build-dir=_build2 --verbose _build2/default/ocaml/stdlib/.stdlib.objs/native/std_exit.cmx
```
where `<FLAMBDA_BACKEND>` is the path to your clone and `<DUNE>` is the path to the dune provided to `configure`.

## How to add a new intrinsic to the compiler

The Flambda backend has a means of replacing calls to external functions
with inline instruction sequences.  This can be used to implement
"intrinsic" operations that typically correspond to very few (often one)
machine instruction.  The external functions, typically written in C,
can still be provided for portability.

Follow the steps below to first update the
[ocaml_intrinsics](https://github.com/janestreet/ocaml_intrinsics)
library, and then the compiler.
  
- Choose existing .ml file or add a new one.
- Add `external` declaration of the function with two C stubs:
  for bytecode and native implementations.
  Only C stubs for bytecode should be annotated with CAMLprim.
  Naming convention: start the stubs with `caml_` because the aim is
  to integrate them into the compiler.
- Make sure that the C stubs work correctly on all support targets
  (architectures, operating systems, and compilers).
- Annotate with `[noalloc]` `[@unboxed]` and `[@untagged]` as
  appropriate. These annotations only apply to the native C stub.
- Annotated with `[@@builtin]` which gives
  the compiler a permission to replace calls to the native C stub
  with instructions.
- Annotate with `[@only_generative_effects]`, `[@no_effects]`, and
 `[@no_coeffects]`, described in
 `semantics_of_primitives.mli` in the compiler.
  These annotations are used by middle-end optimizations and therefore
  apply to only to native compilation.  Their use is currently
  inaccurate in the compiler when it comes to generative effects
  involving arguments and return values only.  In particular, when the
  native C stub is `[@@noalloc]` and its return value is `[@unboxed]`,
  the function should be marked with `[@only_generative_effects]`, but
  is it currently marked with `[@no_effects]`, to be consistent with
  compiler builtins. This will be fixed in Flambda2.
- Add tests and benchmarks for the new functions.
- Now, and only now, update the compiler. The intrinsics can be added in one
  of the two places in `backend` directory:
  -- Cmm:
     Add an instruction to cmm and update cmmgen to emit it for
     the corresponding function application. The intrinsics will be
     applied on all supported architectures, but emitting it might involve
     changes in all the IRs below Cmm in all targets.
     `Proc.operation_supported` make the process easier.
  -- Mach: add an architecture-specifc instruction by extending
     Ispecific, and update selection.ml.
- Compile the library with the modified compiler, making sure that
  all tests pass. Check that functions calls are replaced with the
  corresponding instructions by manually inspecting the generated
  assembly code.
- There are currently no compiler tests for different intrinics. It
  relies on the library tests to avoid duplication. Library tests use
  `Core`, but the library itself does not.
