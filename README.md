# flambda-backend
The Flambda backend project for OCaml.

## Installation instructions

Only currently tested on Linux/x86-64 and macOS/x86-64.
```
$ opam switch 4.11.1
$ eval $(opam env)
$ git clone https://github.com/ocaml-flambda/dune
$ cd dune
$ git checkout origin/special_dune
$ make release
$ cd ..
$ git clone https://github.com/ocaml-flambda/flambda-backend
$ cd flambda-backend
$ git checkout origin/4.11
$ autoconf
$ ./configure --prefix=/path/to/install/dir --enable-middle-end=closure --with-dune=$(pwd)/../dune/dune.exe
$ make
$ make install
```

Note that `make install` completely overwrites the given `--prefix` directory.

Prior to `make install` you can do:
- `make runtest` to run the Flambda backend tests (which use dune);
- `make runtest-upstream` to run the upstream testsuite. The upstream
testsuite runs much faster if you install GNU parallel. This is likely
already present on Linux machines. On macOS, install Homebrew, then `brew
install parallel`.

You can also specify `--enable-middle-end=flambda`.

To rebuild after making changes, you can just type `make` (then `make runtest-upstream` and/or `make install` as required).  If you start a new shell (e.g. after a machine reboot) you first need to ensure your OPAM environment is set up correctly.  For example:
```
$ opam switch 4.11.1
$ eval $(opam env)
$ cd flambda-backend
$ make
```
