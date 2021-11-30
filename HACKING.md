# Hacking on the Flambda backend

This page is intended to keep track of useful information for people who
want to modify the Flambda backend.  Jump to:

  - [Branches, pull requests, etc.](#branches-pull-requests-etc)
  - [Upstream subtree](#upstream-subtree)
  - [Code formatting](#code-formatting)
  - [Rebuilding during dev work](#rebuilding-during-dev-work)
  - [Running tests](#running-tests)
  - [Running only part of the upstream testsuite](#running-only-part-of-the-upstream-testsuite)
  - [Running the compiler produced by "make hacking" on an example without the stdlib](#running-the-compiler-produced-by-make-hacking-on-an-example-without-the-stdlib)
  - [Getting the compilation command for a stdlib file](#getting-the-compilation-command-for-a-stdlib-file)
  - [Bootstrapping the ocaml subtree](#bootstrapping-the-ocaml-subtree)
  - [Testing the compiler built locally with OPAM](#testing-the-compiler-built-locally-with-opam)
  - [Pulling changes onto a release branch](#pulling-changes-onto-a-release-branch)
  - [Rebasing to a new major version of the upstream compiler](#rebasing-to-a-new-major-version-of-the-upstream-compiler)
  - [How to add a new intrinsic to the compiler](#how-to-add-a-new-intrinsic-to-the-compiler)
  - [How to add a new command line option](#how-to-add-a-new-command-line-option)
  - [Installation tree comparison script](#installation-tree-comparison-script)

## Branches, pull requests, etc.

Pull requests should be submitted to the `main` branch, which is the default.

PRs should not be merged unless all CI checks have passed unless there is a good
reason.  It is not necessary to wait for CI checks to pass after genuinely trivial
changes to a PR that was previously passing CI.

There are also release branches (e.g. `release-4.12`) which are used for cutting
production releases (which are all marked by git tags).  These branches should not be
committed to without approval from the person responsible for the next release.

## Upstream subtree

The `ocaml/` directory contains a patched version of the upstream OCaml compiler
around which is built the Flambda backend.  This directory is currently handled as
a git subtree (not a submodule).

Patches to the `ocaml/` subdirectory should be minimised and in the majority of cases
be suitable for upstream submission.

We are planning to move to a model where the patched upstream compiler is maintained
in a normal upstream-style repository (i.e. forked from [`ocaml/ocaml`](https://github.com/ocaml/ocaml)).

## Code formatting

The CI checks that all Flambda 2 code (in `middle_end/flambda2/`) and
Cfg code (in `backend/cfg/`) is
formatted correctly as per the provided `.ocamlformat` file.  To prepare
your environment for the correct version of `ocamlformat` you can follow
the OPAM commands [in the CI check](https://github.com/ocaml-flambda/flambda-backend/blob/main/.github/workflows/ocamlformat.yml).  (Note that the OPAM compiler will not
be used for the Flambda backend build itself.)  All of the code can be
formatted using `make fmt` and the check can be run using `make check-fmt`.

Changes to `.ocamlformat` should be made as pull requests that include
reformatting files as needed.

In the event that one needs to rebase a patch over formatting changes, here is a reasonably seamless way to proceed:

Assuming a specific formatting commit:
```shell
# main formatting commit for flambda2/ in the repository
format_commit=331c16734636a218261d4835fb77b38c5788f50a
```

Rebase as usual until its parent:
```shell
git rebase $format_commit~1
```

Then rebase once more on the commit itself:
```shell
git rebase $format_commit -Xtheirs --exec 'make fmt && git commit -a --amend --no-edit'
```
Each commit will be amended with formatting. Any conflict appearing can be resolved automatically by choosing our side (hence, `theirs` on a rebase, surprisingly enough). This is correct assuming the commit contains no semantic changes.

Finally, finish the rebase as usual up to the desired point:
```shell
git rebase upstream/main
```

Depending on the initial changes, it might be necessary to do this multiple times for each relevant formatting commit.

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

## Running tests

Prior to `make install` you can do:
- `make runtest` to run the Flambda backend tests (which use dune);
- `make runtest-upstream` to run the upstream testsuite. The upstream
testsuite runs much faster if you install GNU parallel. This is likely
already present on Linux machines. On macOS, install Homebrew, then `brew
install parallel`.

There is also a `make ci` target (best run as e.g. `make -j16 ci`) which does a full build
and test run.

Some of our tests are expect tests run using a custom tool called `flexpect`.
Corrected outputs can be promoted using `make promote`.

## Running only part of the upstream testsuite

This can be done from the `_runtest` directory after it has been initialised by a previous `make runtest-upstream`.
Any changes you have made to the tests in the real testsuite directory (`ocaml/testsuite/`) will need to be copied
into here first.  Then you can do things like:
```
OCAMLSRCDIR=<FLAMBDA_BACKEND>/_runtest make one DIR=tests/runtime-errors
```
where `<FLAMBDA_BACKEND>` is the path to your clone.
You may also need the `CAML_LD_LIBRARY_PATH` setting depending on what you are testing (see `Makefile.in` at the
root).

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

## Bootstrapping the ocaml subtree

This can be done following the usual upstream procedures,
working entirely within the `ocaml/` subdirectory.  Thoroughly clean the tree (e.g. `git clean -dfx`),
go into `ocaml/`, then run the upstream configure script.  After that perform the bootstrap (e.g.
`make world` followed by `make bootstrap`).  Before recompiling the Flambda backend as normal it would
be advisable to clean the whole tree again.

## Testing the compiler built locally with OPAM

It is possible to create a OPAM switch with the Flambda backend compiler.

First, you'll need to install the `opam-custom-install` plugin. See
[here](https://gitlab.ocamlpro.com/louis/opam-custom-install) for instructions.
(This can be done in any OPAM switch, e.g. a standard 4.12.0 switch.)

Then you'll need to create an empty switch. The recommended way is to use a
local switch in the Flambda backend directory:

```shell
opam switch create . --empty
```

(A global switch can also be used, in which case the `--prefix` argument
to `configure` given below needs to point at the switch directory under the OPAM root.
It is also necessary to `opam switch` to the new switch and then update the current
environment with `opam env` after the above `opam switch create` command.)

The Flambda backend must also be configured with this switch as prefix
(this can be done before actually creating the switch, the directory only
needs to exist during the installation step):

```shell
./configure --prefix=/path/to/cwd/_opam ...
```

Note that if the Flambda backend tree is already configured, it should be cleaned
thoroughly (e.g. `git clean -dfx`) before reconfiguring with a different prefix.

Then build the compiler normally (`make`).
Once that is done, we're ready to install the compiler:

```shell
opam custom-install ocaml-variants.4.12.0+flambda2+trunk -- make install
```

The exact version doesn't matter that much, but the version number should
match the one in the Flambda backend tree.  (The name of the package given
here is independent of the name of the switch.)

To finish the installation, `opam install ocaml.4.12.0` will install the remaining
auxiliary packages necessary for a regular switch. After that, normal opam
packages can be installed the usual way.

It is also possible to update the compiler after hacking:
```shell
# This will reinstall the compiler, and recompile all packages
# that depend on the compiler
opam custom-install ocaml-variants -- make install
# This skips recompilation of other packages,
# particularly useful for debugging
opam custom-install --no-recompilations ocaml-variants -- make install
```

## Pulling changes onto a release branch

This should only be done with the approval of the person responsible for the next release.
One way of doing it is as follows:
```
git checkout -b release-4.12 flambda-backend/release-4.12
git reset --hard flambda-backend/main
git rebase -i flambda-backend/release-4.12
```
assuming that `flambda-backend` is the git remote for the Flambda backend repo.

The resulting local branch `release-4.12` should _not_ require a force push when pushed
to the remote.

## Rebasing to a new major version of the upstream compiler

The procedure for this is still under development; talk to @poechsel or @mshinwell.

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

## How to add a new command line option

1) Add a ref to `flambda_backend_flags.ml{i}`
2) Add the flag's constructor `mk_<flag>` in `flambda_backend_args.ml`
3) Add the callback for the new flag to `Flambda_backend_options` module type
   in `flambda_backend_args.ml{i}`
4) List the flag in the body of `Make_flambda_backend_options` functor
5) Implement the flag in `Flambda_backend_options_impl`
   by setting the corresponding ref in Flambda_backend_flags
6) Add the flag to `Extra_params` if it can be set via `OCAMLPARAM`

## Installation tree comparison script

A target `make compare` exists to run a comparison script that finds differences
between the upstream and Flambda backend install trees.  This script currently
only runs on Linux, although it shouldn't be hard to port to macOS, especially
if using GNU binutils.  It is recommended to install the Jane Street `patdiff` executable
before running `make compare`.  The comparison script has not been maintained since the
early releases of the Flambda backend; it was written as part of the acceptance process
for the initial release.
