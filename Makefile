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
ci-no-coverage: runtest runtest-upstream minimizer

.PHONY: ci-coverage
ci-coverage: boot-runtest coverage

# CR mshinwell: build is broken
# .PHONY: minimizer-upstream
# minimizer-upstream:
# 	cp chamelon/dune.upstream chamelon/dune
# 	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_main) @chamelon/all

.PHONY: minimizer
minimizer: _build/_bootinstall
	cp chamelon/dune.jst chamelon/dune
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_main) @chamelon/all

.PHONY: hacking-runtest
hacking-runtest: _build/_bootinstall
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) $(coverage_dune_flags) -w $(boot_targets) @runtest

# Only needed for running the test tools by hand; runtest will take care of
# building them using Dune
.PHONY: test-tools
test-tools: runtime-stdlib
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_main) @middle_end/flambda2/tests/tools/all

ARCHES=amd64 arm64
.PHONY: check_all_arches
check_all_arches: _build/_bootinstall
	for arch in $(ARCHES); do \
	  ARCH=$$arch RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) ocamloptcomp.cma; \
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
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) promote $(ws_main)

.PHONY: fmt
fmt:
	ocamlformat -i $$(find . \( -name "*.ml" -or -name "*.mli" \))

.PHONY: check-fmt
check-fmt:
	@if [ "$$(git status --porcelain)" != "" ]; then \
	  echo; \
	  echo "Tree must be clean before running 'make check-fmt'"; \
	  exit 1; \
	fi
	$(MAKE) fmt
	@if [ "$$(git diff)" != "" ]; then \
	  echo; \
	  echo "The following code was not formatted correctly:"; \
	  echo "(the + side of the diff is how it should be formatted)"; \
	  echo "(working copy now contains correctly-formatted code)"; \
	  echo; \
	  git diff --no-ext-diff; \
	  exit 1; \
	fi

.PHONY: regen-flambda2-parser
regen-flambda2-parser: $(dune_config_targets)
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) @middle_end/flambda2/parser/regen --auto-promote || true
# Make sure regeneration is idempotent, and also check that the previous step
# worked (can't tell the difference between failure and successful
# auto-promotion)
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) @middle_end/flambda2/parser/regen

.PHONY: regen-flambda2-tests
regen-flambda2-tests: boot-compiler regen-flambda2-test-dune-rules
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_runstd) \
	  @middle_end/flambda2/tests/regen --auto-promote || true
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_runstd) \
	  @middle_end/flambda2/tests/regen

.PHONY: regen-flambda2-test-dune-rules
regen-flambda2-test-dune-rules: $(dune_config_targets)
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) \
	  @middle_end/flambda2/tests/regen-dune-rules --auto-promote || true
	RUNTIME_DIR=$(RUNTIME_DIR) $(dune) build $(ws_boot) \
	  @middle_end/flambda2/tests/regen-dune-rules

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
	export OCAMLSRCDIR=$$(pwd)/_build_upstream \
         && cd _build_upstream/testsuite \
	 && if $$(which parallel > /dev/null 2>&1); \
            then \
	      echo "Running testsuite in parallel (nproc=$$(nproc))"; \
	      make --no-print-directory parallel; \
            else \
	      echo "Running testsuite sequentially"; \
              make --no-print-directory all; \
            fi
	cd _build_upstream && $(MAKE) check_all_arches

.PHONY: coverage
coverage: boot-runtest
	rm -rf _coverage
	bisect-ppx-report html --tree -o _coverage \
	  --coverage-path=_build/default \
		--source-path=. \
	  --source-path=_build/default
	@echo Coverage report generated in _coverage/index.html

.PHONY: debug
.NOTPARALLEL: debug
debug: install debug-printers ocamlc ocamlopt .ocamldebug

ocamlc:
	ln -s $(prefix)/bin/ocamlc.byte ocamlc

ocamlopt:
	ln  -s $(prefix)/bin/ocamlopt.byte ocamlopt

.ocamldebug: install
	find _build/main -name '*.cmo' -type f -exec dirname {} \; | sort -u | sed 's/^/directory /' > .ocamldebug
	echo "source _build/main/$(ocamldir)/tools/debug_printers" >> .ocamldebug
