SHELL = /usr/bin/env bash
include Makefile.config
export ARCH


ifeq ($(shell test -x '$(DUNE)' || echo fail), fail)
  $(error Dune not found. Run ./configure --with-dune=/path/to/dune.exe (See HACKING.ox))
endif

dune = $(DUNE)

ifeq ($(shell which ocamlopt >& /dev/null || echo fail), fail)
  $(error ocamlopt not found. See HACKING.ox)
endif

boot_ocamlc = main_native.exe
boot_ocamlopt = optmain_native.exe
boot_ocamlmklib = tools/ocamlmklib.exe
boot_ocamldep = tools/ocamldep.exe
boot_ocamlobjinfo = tools/objinfo.exe
ocamldir = .
#toplevels_installed = top opttop
toplevels_installed = top


dune-project: dune-project.ox
	cp $^ $@

duneconf/ox-extra.inc:
	echo '(include ../ox.dune)' > $@

include Makefile.common-ox
