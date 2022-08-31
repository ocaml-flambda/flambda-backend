SHELL = /bin/bash
include Makefile.config
include ocaml/Makefile.config
export ARCH

ws_boot   = --root=. --workspace=duneconf/boot.ws
ws_runstd = --root=. --workspace=duneconf/runtime_stdlib.ws
ws_main   = --root=. --workspace=duneconf/main.ws

ifeq ($(coverage),yes)
  coverage_dune_flags=--instrument-with bisect_ppx
  ocaml_subdirs_to_ignore=otherlibs
else
  coverage_dune_flags=
  ocaml_subdirs_to_ignore=
endif

define dune_boot_context
(lang dune 2.8)
; We need to call the boot context "default" so that dune selects it for merlin
(context (default
  (name default)
  ; CR sdolan: profile dev might be faster, but the compiler currently fails to build in dev.
  (profile boot)))
endef

define dune_runtime_stdlib_context
(lang dune 2.8)
(context (default
  (name runtime_stdlib)
  (profile main)
  (paths
    (PATH ("$(CURDIR)/_build/_bootinstall/bin" :standard))
    (OCAMLLIB ("$(CURDIR)/_build/_bootinstall/lib/ocaml")))
  (env (_ (env-vars
    ("OCAMLPARAM" "$(BUILD_OCAMLPARAM)"))))))
endef

define dune_main_context
(lang dune 2.8)
(context (default
  (name main)
  (profile main)
  (paths
    (PATH ("$(CURDIR)/_build/_bootinstall/bin" :standard))
    (OCAMLLIB ("$(CURDIR)/_build/install/runtime_stdlib/lib/ocaml_runtime_stdlib")))
  (env (_ (env-vars
    ("OCAMLPARAM" "$(BUILD_OCAMLPARAM)"))))))
endef


.DEFAULT_GOAL := compiler
.PHONY: boot-compiler boot-runtest runtime-stdlib compiler runtest

boot-compiler: _build/_bootinstall
	$(dune) build $(ws_boot) $(coverage_dune_flags) \
	  boot_ocamlopt.exe \
	  ocaml/main_native.exe \
	  ocaml/tools/ocamlmklib_native.exe \
	  ocaml/tools/ocamldep_native.exe \
	  tools/ocamlobjinfo_native.exe

boot-runtest: boot-compiler
	$(dune) runtest $(ws_boot) $(coverage_dune_flags) --force

runtime-stdlib: boot-compiler
	$(dune) build $(ws_runstd) --only-package=ocaml_runtime_stdlib @install
#	dune does not believe the compiler can make .cmxs unless the following file exists
	@touch _build/install/runtime_stdlib/lib/ocaml_runtime_stdlib/dynlink.cmxa

compiler: runtime-stdlib
	$(dune) build $(ws_main) --only-package=ocaml @install ocaml/ocamltest/ocamltest.byte

runtest: compiler
	$(dune) runtest $(ws_main)


# This Makefile supports old versions that don't have $(file), so we're using
# environment var trickery to get a multiline string into a file
duneconf/boot.ws: export contents = $(dune_boot_context)
duneconf/runtime_stdlib.ws: export contents = $(dune_runtime_stdlib_context)
duneconf/main.ws: export contents = $(dune_main_context)
duneconf/%.ws:
	echo "$$contents" > $@

# We need to disable ocaml/otherlibs when compiling with coverage, because we
# need to compile against the user's opam instead. Unfortunately, Dune gives us
# no nicer way of declaring data_only_dirs differently in different workspaces,
# so we have to output a file to be included in ocaml/dune.
#
# Also, Dune only allows one (data_only_dirs) declaration per file, so here we
# have to account for the declaration that would already have been in
# ocaml/dune.
ocaml/dirs-to-ignore.inc:
	echo "(data_only_dirs yacc $(ocaml_subdirs_to_ignore))" > $@

_build/_bootinstall: ocaml/Makefile.config duneconf/boot.ws duneconf/runtime_stdlib.ws duneconf/main.ws \
	ocaml/dirs-to-ignore.inc

# natdynlinkops2:
# We need to augment dune's substitutions so this part isn't so
# difficult.  We use /bin/echo to avoid builtin variants of "echo"
# which don't accept "-n".  Unfortunately if there are no
# NATDYNLINKOPS, we need to provide a harmless option, otherwise dune
# will provide '' on the command line to ocamlopt which causes an
# error.
# CR mshinwell: This should be moved into the upstream dune build system.
#
	cat ocaml/Makefile.config \
	  | sed 's/^NATDYNLINKOPTS=$$/NATDYNLINKOPTS=-g/' \
	  | grep '^NATDYNLINKOPTS=' \
	  | sed 's/^[^=]*=\(.*\)/-ccopt\n"\1"/' \
	  > ocaml/otherlibs/dynlink/natdynlinkops
	/bin/echo -n $$(cat ocaml/Makefile.config \
	  | sed 's/^NATDYNLINKOPTS=$$/NATDYNLINKOPTS=-bin-annot/' \
	  | grep '^NATDYNLINKOPTS=' \
	  | sed 's/^[^=]*=\(.*\)/\1/') \
	  > ocaml/otherlibs/dynlink/natdynlinkops2
	if [ "$$(cat ocaml/otherlibs/dynlink/natdynlinkops2)" \
	       != "-bin-annot" ]; \
	then \
	  /bin/echo -n "-ccopt" > ocaml/otherlibs/dynlink/natdynlinkops1; \
	else \
	  /bin/echo -n "-bin-annot" > ocaml/otherlibs/dynlink/natdynlinkops1; \
	fi

# flags.sexp
	echo '(:standard $(if $(filter true,$(FUNCTION_SECTIONS)),-function-sections,))' > ocamlopt_flags.sexp
	echo '( $(OC_CFLAGS) )' > oc_cflags.sexp
	echo '( $(OC_CPPFLAGS) )' > oc_cppflags.sexp
	echo '( $(SHAREDLIB_CFLAGS) )' > sharedlib_cflags.sexp

# _build/_bootinstall: a minimal install directory for the boot compiler,
# which is placed on PATH for subsequent builds
	rm -rf _build/_bootinstall
	mkdir -p _build/_bootinstall/{bin,lib/ocaml}
	cp $^ _build/_bootinstall/lib/ocaml
	ln -sf ../../default/boot_ocamlopt.exe _build/_bootinstall/bin/ocamlopt.opt
	ln -sf ../../default/ocaml/main_native.exe _build/_bootinstall/bin/ocamlc.opt
	ln -sf ../../default/ocaml/tools/ocamlmklib_native.exe _build/_bootinstall/bin/ocamlmklib.opt
	ln -sf ../../default/ocaml/tools/ocamldep_native.exe _build/_bootinstall/bin/ocamldep.opt
	ln -sf ../../default/tools/ocamlobjinfo_native.exe _build/_bootinstall/bin/ocamlobjinfo.opt
	ln -sf "`which ocamllex`" _build/_bootinstall/bin/ocamllex.opt
	for prog in ocamlopt ocamlc ocamllex ocamldep ocamlmklib; do \
	  ln -sf "$$prog.opt" "_build/_bootinstall/bin/$$prog"; \
	done

# save a bit of disk space in build trees by using cp -l on Linux
# (not available on e.g. OS X)
cpl=$(if $(filter linux,$(SYSTEM)),cp -l,cp -L)

# Assemble the contents of the install directory in _install
# This is needed to run the upstream testsuite (see runtest-upstream below)
# We use a local directory rather than the final install path, since
# the final install path may be on a different filesystem (and hence be
# slow and/or unable to make hardlinks)
.PHONY: _install install install_for_opam
_install: compiler
	rm -rf _install
	mkdir -p _install/{bin,lib/ocaml}
	$(cpl) _build/install/{runtime_stdlib,main}/bin/* _install/bin/
	( cd _install/bin; for i in *.opt; do ln -s $$i $${i%.opt}; done )
	$(cpl) -R _build/install/runtime_stdlib/lib/ocaml_runtime_stdlib/* _install/lib/ocaml/
	rm -f _install/lib/ocaml/{META,dune-package,Makefile.config,dynlink.cmxa}
	$(cpl) -R _build/install/main/lib/ocaml/* _install/lib/ocaml/
	rm -f _install/lib/ocaml/{META,dune-package}
	rm -f _install/lib/ocaml/compiler-libs/*.cmo
	$(cpl) {_build/install/main,_install}/lib/ocaml/compiler-libs/topstart.cmo
	for file in topdirs opttopdirs; do \
	  cp -f _install/lib/ocaml/compiler-libs/$$file.{cmi,mli,cmt,cmti} _install/lib/ocaml; \
	done
	mkdir _install/lib/stublibs

	find _build/main/ \( -name "flambda2*.cmi" \
          -or -name "flambda2*.cmti" -or -name "flambda2*.cmt" \) \
          -exec cp -f {} _install/lib/ocaml/compiler-libs \;

# Copy _install to the final install directory (no-op if they are the same)
install: _install
	mkdir -p '$(prefix)'
	rsync --chmod=u+rw,go+r -rl _install/ '$(prefix)'

# Same as above, but relies on a successfull earlier _install
install_for_opam:
	mkdir -p '$(prefix)'
	rsync --chmod=u+rw,go+r -rl _install/ '$(prefix)'

main_prefix = _build/install/main
main_build = _build/main

# The following horror will be removed when work to allow the testsuite to
# run on an installed tree (led by David Allsopp) is completed.
.PHONY: runtest-upstream
runtest-upstream: _install
	rm -rf _runtest
	mkdir _runtest
	ln -s ../_install _runtest/_install
	cp -a ocaml/testsuite _runtest/testsuite
	 # replace backend-specific testsuite/tools with their new versions
	rm _runtest/testsuite/tools/*
	cp -a testsuite/tools/* _runtest/testsuite/tools/
	 # replace backend-specific testsuite/tests/asmgen with their new versions
	rm _runtest/testsuite/tests/asmgen/*
	cp -a testsuite/tests/asmgen/* _runtest/testsuite/tests/asmgen/

	ln -s ../ocaml/Makefile.tools _runtest/Makefile.tools
	ln -s ../ocaml/Makefile.build_config _runtest/Makefile.build_config
	ln -s ../ocaml/Makefile.config_if_required _runtest/Makefile.config_if_required
	ln -s ../ocaml/Makefile.config _runtest/Makefile.config

#	Create an OCaml directory laid out like the testsuite expects,
#	by copying and symlinking in bits from the install/build directory
	(cd _runtest; \
	 for exe in _install/bin/*; do ln -s $$exe; done; \
	 for exe in ocamlc ocamlopt ocamllex; do \
	   rm -f $$exe; ln -s $$exe.byte $$exe; \
	 done; \
	 ln -s _install/lib/ocaml stdlib; \
	 mkdir runtime; \
	 for f in ocamlrun* stdlib/caml stdlib/stublibs/*; do \
	   ln -s ../$$f runtime/`basename $$f`; \
	 done; \
	 ln -s . lex; ln -s . yacc; \
	 ln -s _install/lib/ocaml/compiler-libs compilerlibs; \
	 mkdir -p otherlibs/{unix,dynlink/native,str}; \
	 ln -s ../stdlib/threads otherlibs/systhreads; \
	 $(cpl) stdlib/{lib,}unix* otherlibs/unix; \
	 $(cpl) stdlib/dynlink* otherlibs/dynlink; \
	 $(cpl) stdlib/{lib,}str* otherlibs/str; \
	 ln -s ../_build/main/ocaml/toplevel/.ocamltoplevel.objs/byte toplevel; \
	)

	# Various directories are put on the -I paths by tools/Makefile;
	# utils/ is one such, so we just dump the .cm* files in there for
	# various things.
	mkdir _runtest/utils
	cp $(main_prefix)/lib/ocaml/compiler-libs/*.cmi _runtest/utils
	cp $(main_prefix)/lib/ocaml/compiler-libs/*.cmo _runtest/utils
	cp $(main_prefix)/lib/ocaml/compiler-libs/*.cmx _runtest/utils
	cp $(main_prefix)/lib/ocaml/*.cmi _runtest/utils
	cp $(main_prefix)/lib/ocaml/*.cma _runtest/utils
	cp $(main_prefix)/lib/ocaml/*.a _runtest/utils
	cp $(main_prefix)/lib/ocaml/*.cmxa _runtest/utils
	cp $(main_build)/ocaml/.ocamlcommon.objs/native/config.o _runtest/utils
	# Needed for tests/warnings
	cp ocaml/utils/warnings.ml _runtest/utils
	# Suppress linker errors about -I directories not existing.
	for dir in asmcomp bytecomp driver file_formats lambda middle_end \
	  parsing typing; do ln -s utils _runtest/$$dir; done
	# stublibs
	mkdir -p _runtest/lib/ocaml/stublibs/
	cp $(main_prefix)/lib/ocaml/stublibs/*.so _runtest/lib/ocaml/stublibs
	# ocamldebug
	mkdir _runtest/debugger
	ln -s ../ocamldebug _runtest/debugger
	cp $(main_build)/ocaml/debugger/.main.eobjs/byte/*.cm* \
	  _runtest/debugger
	# The ast_invariants test needs VERSION to be present.  In fact ideally
	# we should have all the source files in _runtest too for this test,
	# but for the moment we accept it being a weaker check.  We're not
	# working on parts of the compiler that deal with the AST anyway in
	# this repo.
	touch _runtest/VERSION
	# tools
	mkdir _runtest/tools
	cp $(main_build)/ocaml/tools/ocamlmklib_byte.bc _runtest/tools/ocamlmklib
	cp $(main_build)/tools/ocamlobjinfo_byte.bc _runtest/tools/ocamlobjinfo
	# ocamltest itself
	mkdir _runtest/ocamltest
	cp $(main_build)/ocaml/ocamltest/ocamltest.byte _runtest/ocamltest/ocamltest
	for dir in `cd ocaml/testsuite; ls -1 -d tests/*`; do \
	  if ! grep -q "^  $$dir " testsuite/flambda2-test-list; then \
	    echo "  $$dir"; \
	  fi; \
	done > _runtest/flambda2-test-list
	(export OCAMLSRCDIR=$$(pwd)/_runtest; \
	 export CAML_LD_LIBRARY_PATH=$$(pwd)/_runtest/lib/ocaml/stublibs; \
	 if $$(which gfortran > /dev/null 2>&1); then \
	   export LIBRARY_PATH=$$(dirname $$(gfortran -print-file-name=libgfortran.a)); \
	 fi; \
	 cd _runtest/testsuite \
	  && if $$(which parallel > /dev/null 2>&1); \
             then \
	       echo "Running testsuite in parallel (nproc=$$(nproc))"; \
               if [ "$(middle_end)" = "flambda2" ]; then \
                 make --no-print-directory list-parallel FILE=$$(pwd)/../flambda2-test-list; \
	       else \
                 make --no-print-directory parallel; \
               fi \
             else \
	       echo "Running testsuite sequentially"; \
               if [ "$(middle_end)" = "flambda2" ]; then \
                 make --no-print-directory list FILE=$$(pwd)/../flambda2-test-list; \
	       else \
                 make --no-print-directory all; \
               fi \
             fi)

.PHONY: ci
ifeq ($(coverage),yes)
ci: ci-coverage
else
ci: ci-no-coverage
endif

.PHONY: ci-no-coverage
ci-no-coverage: runtest runtest-upstream

.PHONY: ci-coverage
ci-coverage: boot-runtest coverage

# This target is like a polling version of upstream "make ocamlopt"
.PHONY: hacking
hacking: _build/_bootinstall
	$(dune) build $(ws_boot) -w boot_ocamlopt.exe

.PHONY: hacking-runtest
hacking-runtest: _build/_bootinstall
	$(dune) runtest $(ws_boot) $(coverage_dune_flags) -w

# Only needed for running the test tools by hand; runtest will take care of
# building them using Dune
.PHONY: test-tools
test-tools: runtime-stdlib
	$(dune) build $(ws_main) @middle_end/flambda2/tests/tools/all

ARCHES=amd64 arm64
.PHONY: check_all_arches
check_all_arches: _build/_bootinstall
	for arch in $(ARCHES); do \
	  ARCH=$$arch $(dune) build $(ws_boot) ocamloptcomp.cma; \
	done

# Compare the Flambda backend installation tree against the upstream one.

.PHONY: compare
compare: _compare/config.status _install
	rm -f .rsync-output-compare
	rsync -i -a --filter=':- $$(pwd)/ocaml/.gitignore' \
	  $$(pwd)/ocaml/ $$(pwd)/_compare \
	  | grep -v '/$$' \
	  | tee .rsync-output-compare
	if [ -s .rsync-output-compare ] || ! [ -d _compare/_install ]; then \
	  (cd _compare && \
	    $(MAKE) world.opt && \
	    $(MAKE) ocamlnat && \
	    $(MAKE) install); \
	fi
	./scripts/compare.sh $$(pwd)/_compare/_install $$(pwd)/_install \
	  _install/bin/ocamlobjinfo.opt

_compare/config.status: ocaml/config.status
	rm -rf _compare
	mkdir _compare
	rsync -a --filter=':- $$(pwd)/ocaml/.gitignore' \
	  $$(pwd)/ocaml/ $$(pwd)/_compare
	(cd _compare && ./configure $(CONFIGURE_ARGS) --prefix=$$(pwd)/_install)

.PHONY: promote
promote:
	$(dune) promote $(ws_main)

.PHONY: fmt
fmt:
	ocamlformat -i \
	  $$(find middle_end/flambda2 \
	    \( -name "*.ml" -or -name "*.mli" \) \
	    -and \! \( -name "flambda_parser.*" -or -name "flambda_lex.*" \))
	ocamlformat -i \
	  $$(find backend/cfg \
	    \( -name "*.ml" -or -name "*.mli" \))
	ocamlformat -i middle_end/mangling.ml
	ocamlformat -i middle_end/mangling.mli
	ocamlformat -i \
	  $$(find backend/asm_targets \
	    \( -name "*.ml" -or -name "*.mli" \))
	ocamlformat -i \
	  $$(find backend/debug \
	    \( -name "*.ml" -or -name "*.mli" \))
	ocamlformat -i backend/cmm_helpers.ml{,i}
	ocamlformat -i tools/merge_archives.ml
	ocamlformat -i \
	  $$(find backend/debug/dwarf \
	    \( -name "*.ml" -or -name "*.mli" \))

.PHONY: check-fmt
check-fmt:
	@if [ "$$(git status --porcelain middle_end/flambda2)" != "" ] || \
           [ "$$(git status --porcelain backend/cfg)" != "" ] || \
           [ "$$(git status --porcelain middle_end/mangling.ml)" != "" ] || \
           [ "$$(git status --porcelain middle_end/mangling.mli)" != "" ] || \
           [ "$$(git status --porcelain backend/asm_targets)" != "" ] || \
           [ "$$(git status --porcelain backend/debug)" != "" ] || \
           [ "$$(git status --porcelain backend/cmm_helpers.ml{,i})" != "" ] || \
           [ "$$(git status --porcelain tools/merge_archives.ml)" != "" ]; then \
	  echo; \
	  echo "Tree must be clean before running 'make check-fmt'"; \
	  exit 1; \
	fi
	$(MAKE) fmt
	@if [ "$$(git diff middle_end/flambda2)" != "" ] || \
           [ "$$(git diff backend/cfg)" != "" ] || \
           [ "$$(git diff middle_end/mangling.ml)" != "" ] || \
           [ "$$(git diff middle_end/mangling.mli)" != "" ] || \
           [ "$$(git diff backend/asm_targets)" != "" ] || \
           [ "$$(git diff backend/debug)" != "" ] || \
           [ "$$(git diff backend/cmm_helpers.ml{,i})" != "" ] || \
           [ "$$(git diff tools/merge_archives.ml)" != "" ]; then \
	  echo; \
	  echo "The following code was not formatted correctly:"; \
	  echo "(the + side of the diff is how it should be formatted)"; \
	  echo "(working copy now contains correctly-formatted code)"; \
	  echo; \
	  git diff --no-ext-diff; \
	  exit 1; \
	fi

.PHONY: regen-flambda2-parser
regen-flambda2-parser:
	$(dune) build $(ws_main) @middle_end/flambda2/parser/regen --auto-promote || true
# Make sure regeneration is idempotent, and also check that the previous step
# worked (can't tell the difference between failure and successful
# auto-promotion)
	$(dune) build $(ws_main) @middle_end/flambda2/parser/regen


## Build upstream compiler.
.PHONY: build_upstream
build_upstream: ocaml/config.status
	rsync -a ocaml/ _build_upstream
	(cd _build_upstream && \
	    $(MAKE) world.opt && \
	    $(MAKE) ocamlnat)

.PHONY: install_upstream
install_upstream: build_upstream
	(cd _build_upstream && $(MAKE) install)
	cp ocaml/VERSION $(prefix)/lib/ocaml/

.PHONY: coverage
coverage: boot-runtest
	rm -rf _coverage
	bisect-ppx-report html --tree -o _coverage \
	  --coverage-path=_build/default \
		--source-path=. \
	  --source-path=_build/default
	@echo Coverage report generated in _coverage/index.html
