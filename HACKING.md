# Hacking on the Flambda backend

This page is intended to keep track of useful information for people who
want to modify the Flambda backend.  Jump to:

- [Branches, pull requests, etc.](#branches-pull-requests-etc)
- [Upstream subtree](#upstream-subtree)
- [Code formatting](#code-formatting)
- [Rebuilding during dev work](#rebuilding-during-dev-work)
- [Updating magic numbers](#updating-magic-numbers)
- [Running tests](#running-tests)
- [Running only part of the upstream testsuite](#running-only-part-of-the-upstream-testsuite)
- [Running tests with coverage analysis](#running-tests-with-coverage-analysis)
- [Running the compiler produced by "make hacking" on an example without the stdlib](#running-the-compiler-produced-by-make-hacking-on-an-example-without-the-stdlib)
- [Using the OCaml debugger to debug the compiler](#using-the-ocaml-debugger-to-debug-the-compiler)
  - [Alternative debugger workflow](#alternative-debugger-workflow)
- [Getting the compilation command for a stdlib file](#getting-the-compilation-command-for-a-stdlib-file)
- [Bootstrapping the ocaml subtree](#bootstrapping-the-ocaml-subtree)
- [Testing the compiler built locally with OPAM (new method)](#testing-the-compiler-built-locally-with-opam-new-method)
- [Testing the compiler built locally with OPAM (old method)](#testing-the-compiler-built-locally-with-opam-old-method)
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

Note in particular that a recent (>= 1.10.0) version of the `re` library is
required due to a bug in the parsing of `.ocamlformat-enable` syntax.

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

To rebuild after making changes, you can just type `make`. You need to
have a working OCaml 4.14 or 4.14.1 compiler on your PATH before doing so,
e.g. installed via OPAM. You also need to have dune and menhir.

`menhir` should be pinned to a specific version: `opam pin add menhir 20231231`.

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

## Updating magic numbers

Start from a completely clean tree.  Then change into the `ocaml` subdirectory
and proceed as follows:
```
./configure
make coldstart
make coreall
```
Then edit `runtime/caml/exec.h` and `utils/config.mlp` to bump the numbers.  Then:
```
make coreall
make bootstrap
```
and commit the result.

## Running tests

Prior to `make install` you can do:
- `make runtest` to run the Flambda backend tests (which use dune);
- `make runtest-upstream` to run the upstream testsuite. The upstream
testsuite runs much faster if you install GNU parallel. This is likely
already present on Linux machines. On macOS, install Homebrew, then `brew
install parallel`.

There is also a `make ci` target which does a full build and test run.

Some of our tests are expect tests run using a custom tool called `flexpect`.
Corrected outputs can be promoted using `make promote`.

See `ocaml/HACKING.jst.adoc` for documentation on additional test-related
targets. When that documentation says to run (say) `make -f Makefile.jst test-one`
from the `ocaml` subdirectory, you should instead run `make test-one` from the
root of the repo. Here are some examples of commands you can run:

```
$ make test-one TEST=typing-local/local.ml
$ make test-one-no-rebuild TEST=typing-local/local.ml
$ make promote-one TEST=typing-local/local.ml
$ make promote-one-no-rebuild TEST=typing-local/local.ml
# Promote failures from the last run
$ make promote-failed
# You can also use the full path from the root of the repo.
# This interacts better with tab completion.
$ make test-one TEST=ocaml/testsuite/tests/typing-local/local.ml
```

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

## Running tests with coverage analysis

Coverage analysis is available for the Flambda backend tests (that is, just the
ones run by `make runtest`), which are intended to provide good coverage on
their own. We use `bisect_ppx` to perform the analysis. Since binaries
instrumented with `bisect_ppx` have coverage enabled unconditionally, coverage
support is disabled by default at compile time.

Coverage requires the `bisect_ppx` package to be installed in your OPAM switch.
Since no OPAM environment is available when building the final compiler, we
instead enable coverage on the boot compiler and run the tests directly on the
boot compiler.

To enable coverage, pass the `--enable-coverage` flag to `./configure`.
(Remember to clean, as by `git clean -dfX`, whenever re-running
`./configure`). When coverage is enabled, `make boot-runtest` will
run the tests on the boot compiler and produce coverage data, and
`make coverage` will produce an HTML report in `_coverage/`. Alternatively,
with coverage enabled, `make ci` will build the boot compiler, run the
tests, and produce the report.

## Running the compiler produced by "make hacking" on an example without the stdlib

For small examples that don't need the stdlib or any other library provided by the
compiler distribution, it suffices to have run `make hacking`, followed by
something like:
```
./_build/_bootinstall/bin/ocamlopt.opt -nostdlib -nopervasives -c test.ml
```

## Using the OCaml debugger to debug the compiler

First, run `make debug`. This completes four steps:

1. `make install`
2. Sets up the `ocaml/tools/debug_printers` script so that you can `source
   ocaml/tools/debug_printers` during a debugging session to see
   otherwise-abstract variable values.
3. Symlinks `./ocamlc` and `./ocamlopt` to point to the bytecode versions of
   those compilers. This is convenient for emacs integration, because emacs
   looks for sources starting in the directory containing the executable.
4. Creates a `.ocamldebug` file to automatically load the right search path
   and the `debug_printers` set up above.

Then it's time to run the debugger itself.  The recommended workflow is to add
the elisp below to your emacs init file, and then use the command
`ocamldebug-ocamlc` to debug `ocamlc` or the command `ocamldebug-ocamlopt` to
debug `ocamlopt`. Running your built `ocamldebug` file on `ocamlc` or `ocamlopt`
should also work, if you wish to work outside emacs.

```
;; directly inspired by the ocamldebug implementation in ocamldebug.el
(require 'ocamldebug)
(defun ocamldebug-ocaml (cmd)
  "Runs ocamldebug on the provided command"
  (interactive)
  (let* ((ocaml-dir (expand-file-name
                     (locate-dominating-file (buffer-file-name) ".git")))
         (pgm-path (file-name-concat ocaml-dir cmd))
         (comint-name (concat "ocamldebug-" cmd))
         (buffer-name (concat "*" comint-name "*"))
         (ocamldebug-command-name
          (file-name-concat ocaml-dir "_build/install/main/bin/ocamldebug")))
    (unless (file-exists-p ocamldebug-command-name)
      (error "No debugger found; run `make debug` first."))
    (pop-to-buffer buffer-name)
    (unless (comint-check-proc buffer-name)
      (setq default-directory ocaml-dir)
      (setq ocamldebug-debuggee-args
            (read-from-minibuffer (format "Args for ocamlc: ")
                                  ocamldebug-debuggee-args))
      ;; In addition to the directories in .ocamldebug, use 'find' to
      ;; see also list directories with -I; this finds any new cmo directories
      ;; since the last 'make debug'
      (let* ((cmo-top-dir (file-name-concat ocaml-dir "_build/main"))
             (find-cmo-cmd (concat "find "
                                   cmo-top-dir
                                   " -name '*.cmo' -type f -printf '%h\n' | sort -u"))
             (cmo-dirs (shell-command-to-string find-cmo-cmd)))
        (setq cmo-dir-list (split-string cmo-dirs "\n" t)))
      (let* ((user-args (split-string-shell-command ocamldebug-debuggee-args))
             (includes (mapcan (lambda (dir) (list "-I" dir)) cmo-dir-list))
             (args (append (list
                             comint-name
                             ocamldebug-command-name
                             nil
                             "-emacs"
                             "-cd" default-directory)
                           includes
                           (list pgm-path)
                           user-args)))
        (apply #'make-comint args)
        (set-process-filter (get-buffer-process (current-buffer))
                            #'ocamldebug-filter)
        (set-process-sentinel (get-buffer-process (current-buffer))
                              #'ocamldebug-sentinel)
        (ocamldebug-mode)))
    (ocamldebug-set-buffer)))
(defun ocamldebug-ocamlc ()
  "Runs ocamldebug on the ocamlc built from the source file in the active buffer"
  (interactive)
  (ocamldebug-ocaml "ocamlc"))
(defun ocamldebug-ocamlopt ()
  "Runs ocamldebug on the ocamlopt built from the source file in the active buffer"
  (interactive)
  (ocamldebug-ocaml "ocamlopt"))
```

These commands will prompt you for the arguments to be passed to the compiler.
Usually this includes the location of a test `.ml` file to be compiled (note
that `~` will not be expanded, so using a full path is often necessary).
Compiler command line flags may also be passed this way (e.g., `-extension`
flags).

Once at the ocamldebugger's `(ocd)` prompt, you are ready to set breakpoints
in relevant compiler source files with `C-x C-a C-b` and `run` the debugger.

See [the manual section](https://v2.ocaml.org/manual/debugger.html) for more
information about the debugger.

### Alternative debugger workflow

Rather than using our elisp above, you can instead manually invoke the
ocamldebug emacs mode as follows:

1. Run `M-x camldebug RET`
2. Choose the `ocamlc` or `ocamlopt` symlink in the root of the repo.
3. Choose the arguments to pass to the compiler, likely a full path to a test
   `.ml` file.
4. Choose the built `ocamldebug`, in your install directory.
5. Set any breakpoints you want. The easiest way is to navigate to the line
   where you want the breakpoint and use `C-x C-a C-b` in emacs.
6. Add relevant directories to `ocamldebug`'s search path.  (If you skip this,
   printing any value may produce `Cannot find module Misc.` or similar
   errors).  If debugging `ocamlc`, run:
   ```
   (ocd) directory _build/main/ocaml/.ocamlcommon.objs/byte
   ```
   If debugging `ocamlopt`, you'll need various additional directories depending
   on your middle end.  You can find the right directories by searching for cmo
   files corresponding to the module named in the error message.
7. `run` to your breakpoint.

The elisp `ocamldebug-ocaml{c,opt}` functions automate steps 1, 2, 4, 6, and 7,
above.

## Getting the compilation command for a stdlib file

For example because you need to get the `-dflambda` output because of a bug.
```
rm -f _build/runtime_stdlib/ocaml/stdlib/.stdlib.objs/native/std_exit.cmx
<DUNE> build --workspace=duneconf/runtime_stdlib.ws --verbose ocaml/stdlib/.stdlib.objs/native/std_exit.cmx
```
where `<DUNE>` is the path to the dune provided to `configure`.

## Bootstrapping the ocaml subtree

This can be done following the usual upstream procedures,
working entirely within the `ocaml/` subdirectory.  Thoroughly clean the tree (e.g. `git clean -dfX`),
go into `ocaml/`, then run the upstream configure script.  After that perform the bootstrap (e.g.
`make core` followed by `make bootstrap`).  Before recompiling the Flambda backend as normal it would
be advisable to clean the whole tree again.

## Testing the compiler built locally with OPAM (new method)

This is still under development, but should work!
```shell
opam repo add flambda-backend git+https://github.com/chambart/opam-repository-js.git#with-extensions
opam switch create 5.1.1+flambda2 --repos flambda-backend,default
eval $(opam env --switch=5.1.1+flambda2)
```

## Testing the compiler built locally with OPAM (old method)

It is possible to create a OPAM switch with the Flambda backend compiler.

The first step is to choose where to put the switch. One possibility is to use a
local switch at the root of the tree, in which case the prefix will be
`${flambda-backend-root-dir}/_opam`, but it's also possible to use a local switch elsewhere or
a global switch. For a global switch named `flambda-backend`, the prefix will be
`$(opam var root)/flambda-backend`.

The Flambda backend must then be configured with this switch as prefix:

```shell
./configure --prefix=${opam_switch_prefix} ...
```

Note that if the Flambda backend tree is already configured, it should be cleaned
thoroughly (e.g. `git clean -dfX`) before reconfiguring with a different prefix.

Then build the compiler with the command `make _install` (this is the default
target plus some setup in preparation for installation). As usual when building,
a 4.14 compiler (and dune and menhir) need to be in the path. See the warning above
about the version of menhir to use.

Now the build part is done, we don't need to stay in the build environment
anymore; the switch creation will likely replace it if your terminal is setup
to automatically follow the active opam switch.

The next step is to create the switch if it wasn't done already (if you already
had created a switch from a previous attempt, you will need to remove it first):

```shell
# For a local switch:
opam switch create . --empty --repositories=flambda2=git+https://github.com/ocaml-flambda/flambda2-opam.git,default
# For a global switch:
opam switch create flambda-backend --empty --repositories=flambda2=git+https://github.com/ocaml-flambda/flambda2-opam.git,default
```

Then we can install the compiler. The recommended way is to use the `opam-custom-install`
plugin. See [here](https://gitlab.ocamlpro.com/louis/opam-custom-install)
for instructions. The plugin can be installed in any existing OPAM switch,
for example a 4.14 switch used for building. Once installed, the plugin will be
available whatever the current active switch is.
Once the plugin is installed, we can use it to install the compiler:

```shell
opam custom-install ocaml-variants.4.14.0+flambda2 -- make -C ${flambda-backend-root-dir} install_for_opam
```
The `-C ${flambda-backend-dir}` part can be omitted if we're still in the build directory.

Note that due to issues with some versions of the custom-install plugin,
it is recommended to run the command `opam reinstall --forget-pending` after
every use of `opam custom-install`, otherwise any subsequent `opam` command
tries to rebuild the compiler from scratch.

To finish the installation, `opam install ocaml.4.14.0` will install the remaining
auxiliary packages necessary for a regular switch. After that, normal opam
packages can be installed the usual way.

It is also possible to update the compiler after hacking, by running the
`opam custom-install` command again. It also accepts a `-n` flag to skip
recompilation of the packages which depend on the compiler, which can be useful
when the output of the compiler is unchanged apart from extra logging.

As `opam-custom-install` is still experimental, it can sometimes be hard to install.
In this case, it is possible to use the more fragile `opam install --fake` command:

```shell
opam install --fake ocaml-variants.4.14.0+flambda2
make -C ${flambda-backend-root-dir} install_for_opam
```

The main drawback of this approach is that there isn't any way to cleanup an
installation properly without deleting the whole switch; if the set of installed
files change between one `make install_for_opam` command and the next, strange
bugs might appear.

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
