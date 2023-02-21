SHELL = /usr/bin/env bash
include Makefile.config
include ocaml/Makefile.config
export ARCH

boot_ocamlc = ocaml/main_native.exe
boot_ocamlopt = boot_ocamlopt.exe
boot_ocamlmklib = ocaml/tools/ocamlmklib.exe
boot_ocamldep = ocaml/tools/ocamldep.exe
boot_ocamlobjinfo = tools/flambda_backend_objinfo.exe
ocamldir = ocaml
toplevels_installed = top opttop

$(ocamldir)/duneconf/jst-extra.inc:
	echo > $@

include ocaml/Makefile.common-jst

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

.PHONY: hacking-runtest
hacking-runtest: _build/_bootinstall
	$(dune) build $(ws_boot) $(coverage_dune_flags) -w $(boot_targets) @runtest

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
	ocamlformat -i backend/cmm_builtins.ml{,i}
	ocamlformat -i backend/checkmach.ml{,i}
	ocamlformat -i tools/merge_archives.ml
	ocamlformat -i \
	  $$(find backend/debug/dwarf \
	    \( -name "*.ml" -or -name "*.mli" \))
	ocamlformat -i \
	  $$(find utils \
	    \( -name "*.ml" -or -name "*.mli" \))
	ocamlformat -i \
	  $$(find ocaml/utils \
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
           [ "$$(git status --porcelain backend/cmm_builtins.ml{,i})" != "" ] || \
           [ "$$(git status --porcelain backend/checkmach.ml{,i})" != "" ] || \
           [ "$$(git status --porcelain tools/merge_archives.ml)" != "" ] || \
           [ "$$(git status --porcelain ocaml/utils)" != "" ] || \
           [ "$$(git status --porcelain utils)" != "" ]; then \
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
           [ "$$(git diff backend/cmm_builtins.ml{,i})" != "" ] || \
           [ "$$(git diff backend/checkmach.ml{,i})" != "" ] || \
           [ "$$(git diff tools/merge_archives.ml)" != "" ] || \
           [ "$$(git diff ocaml/utils)" != "" ] || \
           [ "$$(git diff utils)" != "" ]; then \
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
	ln -s ocamltoplevel.cmxa \
	  $(prefix)/lib/ocaml/compiler-libs/ocamlopttoplevel.cmxa
	ln -s ocamltoplevel.a \
	  $(prefix)/lib/ocaml/compiler-libs/ocamlopttoplevel.a

.PHONY: build_and_test_upstream
build_and_test_upstream: build_upstream
	if $$(which gfortran > /dev/null 2>&1); then \
	  export LIBRARY_PATH=$$(dirname $$(gfortran -print-file-name=libgfortran.a)); \
	fi; \
	cd _build_upstream/testsuite \
	 && if $$(which parallel > /dev/null 2>&1); \
            then \
	      echo "Running testsuite in parallel (nproc=$$(nproc))"; \
	      make --no-print-directory parallel; \
            else \
	      echo "Running testsuite sequentially"; \
              make --no-print-directory all; \
            fi

.PHONY: coverage
coverage: boot-runtest
	rm -rf _coverage
	bisect-ppx-report html --tree -o _coverage \
	  --coverage-path=_build/default \
		--source-path=. \
	  --source-path=_build/default
	@echo Coverage report generated in _coverage/index.html
