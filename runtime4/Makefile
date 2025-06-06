#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

ROOTDIR = ..

include $(ROOTDIR)/Makefile.common

# Lists of source files

BYTECODE_C_SOURCES := $(addsuffix .c, \
  interp misc stacks fix_code startup_aux startup_byt freelist major_gc \
  minor_gc memory alloc roots_byt globroots fail_byt signals \
  signals_byt printexc backtrace_byt backtrace compare ints eventlog prng \
  floats simd str array io extern intern hash sys meta parsing gc_ctrl md5 obj \
  lexing callback debugger weak compact finalise custom dynlink \
  afl $(UNIX_OR_WIN32) bigarray main memprof domain float32 \
  skiplist codefrag blake2 systhreads/st_stubs)

NATIVE_C_SOURCES := $(addsuffix .c, \
  startup_aux startup_nat main fail_nat roots_nat signals \
  signals_nat misc freelist major_gc minor_gc memory alloc compare ints \
  floats simd str array io extern intern hash sys parsing gc_ctrl eventlog prng md5 obj \
  lexing $(UNIX_OR_WIN32) printexc callback weak compact finalise custom \
  globroots backtrace_nat backtrace dynlink_nat debugger meta \
  dynlink clambda_checks afl bigarray float32 \
  memprof domain skiplist codefrag blake2 \
  systhreads/st_stubs)

# Header files generated by configure
CONFIGURED_HEADERS := caml/m.h caml/s.h caml/version.h

# Header files generated by make
BUILT_HEADERS := caml/opnames.h caml/jumptbl.h build_config.h

ifeq "$(TOOLCHAIN)" "msvc"
ASM_EXT := asm
ASM_SOURCES := $(ARCH)nt.$(ASM_EXT)
else
ASM_EXT := S
ASM_SOURCES := $(ARCH).$(ASM_EXT)
endif

# Targets to build and install

PROGRAMS := ocamlrun$(EXE)
BYTECODE_STATIC_LIBRARIES := ld.conf libcamlrun.$(A)
BYTECODE_SHARED_LIBRARIES :=
NATIVE_STATIC_LIBRARIES := libasmrun.$(A)
NATIVE_SHARED_LIBRARIES :=

ifeq "$(RUNTIMED)" "true"
PROGRAMS += ocamlrund$(EXE)
BYTECODE_STATIC_LIBRARIES += libcamlrund.$(A)
NATIVE_STATIC_LIBRARIES += libasmrund.$(A)
endif

ifeq "$(INSTRUMENTED_RUNTIME)" "true"
PROGRAMS += ocamlruni$(EXE)
BYTECODE_STATIC_LIBRARIES += libcamlruni.$(A)
NATIVE_STATIC_LIBRARIES += libasmruni.$(A)
endif

ifeq "$(UNIX_OR_WIN32)" "unix"
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
BYTECODE_STATIC_LIBRARIES += libcamlrun_pic.$(A)
BYTECODE_SHARED_LIBRARIES += libcamlrun_shared.$(SO)
NATIVE_STATIC_LIBRARIES += libasmrun_pic.$(A)
NATIVE_STATIC_LIBRARIES += libasmrunnnp.$(A)
NATIVE_SHARED_LIBRARIES += libasmrun_shared.$(SO)
endif
endif

# List of object files for each target

ASM_OBJECTS := $(ASM_SOURCES:.$(ASM_EXT)=.$(O))

libcamlrun_OBJECTS := $(BYTECODE_C_SOURCES:.c=.b.$(O))

libcamlrun_non_shared_OBJECTS := \
  $(subst $(UNIX_OR_WIN32).b.$(O),$(UNIX_OR_WIN32)_non_shared.b.$(O), \
          $(libcamlrun_OBJECTS))

libcamlrund_OBJECTS := $(BYTECODE_C_SOURCES:.c=.bd.$(O)) \
  instrtrace.bd.$(O)

libcamlruni_OBJECTS := $(BYTECODE_C_SOURCES:.c=.bi.$(O))

libcamlrunpic_OBJECTS := $(BYTECODE_C_SOURCES:.c=.bpic.$(O))

libasmrun_OBJECTS := $(NATIVE_C_SOURCES:.c=.n.$(O)) $(ASM_OBJECTS)

libasmrund_OBJECTS := $(NATIVE_C_SOURCES:.c=.nd.$(O)) $(ASM_OBJECTS)

libasmruni_OBJECTS := $(NATIVE_C_SOURCES:.c=.ni.$(O)) $(ASM_OBJECTS)

libasmrunpic_OBJECTS := $(NATIVE_C_SOURCES:.c=.npic.$(O)) \
  $(ASM_OBJECTS:.$(O)=_libasmrunpic.$(O))

libasmrunnnp_OBJECTS := $(NATIVE_C_SOURCES:.c=.nnp.$(O)) $(ASM_OBJECTS)

# General (non target-specific) assembler and compiler flags

ifneq "$(CCOMPTYPE)" "msvc"
OC_CFLAGS += -g -Wno-unknown-warning-option
endif

OC_CPPFLAGS += -DCAMLDLLIMPORT=

# merge5: runtime4 requires -DCAML_NAME_SPACE and doesn't compile cleanly with two
# new warnings added to the build in ocaml5
OC_CPPFLAGS += -DCAML_NAME_SPACE
OC_CFLAGS := $(filter-out -Wstrict-prototypes -Wold-style-definition, $(OC_CFLAGS))

OC_NATIVE_CPPFLAGS = -DNATIVE_CODE -DTARGET_$(ARCH)

ifeq "$(UNIX_OR_WIN32)" "unix"
OC_NATIVE_CPPFLAGS += -DMODEL_$(MODEL)
endif

OC_NATIVE_CPPFLAGS += -DSYS_$(SYSTEM)

OC_DEBUG_CPPFLAGS=-DDEBUG
OC_INSTR_CPPFLAGS=-DCAML_INSTR

ifeq "$(TOOLCHAIN)" "msvc"
ASMFLAGS=
endif

ASPPFLAGS = -DSYS_$(SYSTEM) -I$(ROOTDIR)/runtime4
ifeq "$(UNIX_OR_WIN32)" "unix"
ASPPFLAGS += -DMODEL_$(MODEL)
endif

# Commands used to build native libraries

LIBS := $(BYTECCLIBS)

ifeq "$(UNIX_OR_WIN32)" "win32"
LIBS += $(EXTRALIBS)
endif

# Build, install and clean targets

.PHONY: all
all: $(BYTECODE_STATIC_LIBRARIES) $(BYTECODE_SHARED_LIBRARIES) $(PROGRAMS) \
     sak$(EXE)

.PHONY: allopt
ifneq "$(NATIVE_COMPILER)" "false"
allopt: $(NATIVE_STATIC_LIBRARIES) $(NATIVE_SHARED_LIBRARIES)
else
allopt:
	$(error The build has been configured with --disable-native-compiler)
endif

INSTALL_INCDIR=$(INSTALL_LIBDIR)/caml
.PHONY: install
install:
	$(INSTALL_PROG) $(PROGRAMS) "$(INSTALL_BINDIR)"
	$(INSTALL_DATA) $(BYTECODE_STATIC_LIBRARIES) "$(INSTALL_LIBDIR)"
ifneq "$(BYTECODE_SHARED_LIBRARIES)" ""
	$(INSTALL_PROG) $(BYTECODE_SHARED_LIBRARIES) "$(INSTALL_LIBDIR)"
endif
	mkdir -p "$(INSTALL_INCDIR)"
	$(INSTALL_DATA) caml/domain_state.tbl caml/*.h "$(INSTALL_INCDIR)"

.PHONY: installopt
installopt:
	$(INSTALL_DATA) $(NATIVE_STATIC_LIBRARIES) "$(INSTALL_LIBDIR)"
ifneq "$(NATIVE_SHARED_LIBRARIES)" ""
	$(INSTALL_PROG) $(NATIVE_SHARED_LIBRARIES) "$(INSTALL_LIBDIR)"
endif

.PHONY: clean
clean:
	rm -f *.o *.obj *.a *.lib *.so *.dll ld.conf
	rm -f ocamlrun ocamlrund ocamlruni ocamlruns sak
	rm -f ocamlrun.exe ocamlrund.exe ocamlruni.exe ocamlruns.exe sak.exe
	rm -f primitives primitives.new prims.c $(BUILT_HEADERS)
	rm -f domain_state*.inc
	rm -rf $(DEPDIR)

.PHONY: distclean
distclean: clean
	rm -f $(CONFIGURED_HEADERS)

# Generated non-object files

ld.conf: $(ROOTDIR)/Makefile.config
	echo "$(STUBLIBDIR)" > $@
	echo "$(LIBDIR)" >> $@

# If primitives contain duplicated lines (e.g. because the code is defined
# like
# #ifdef X
# CAMLprim value caml_foo() ...
# #else
# CAMLprim value caml_foo() ...
# end), horrible things will happen (duplicated entries in Runtimedef ->
# double registration in Symtable -> empty entry in the PRIM table ->
# the bytecode interpreter is confused).
# We sort the primitive file and remove duplicates to avoid this problem.

# Warning: we use "sort | uniq" instead of "sort -u" because in the MSVC
# port, the "sort" program in the path is Microsoft's and not cygwin's

# Warning: POSIX sort is locale dependent, that's why we set LC_ALL explicitly.
# Sort is unstable for "is_directory" and "isatty"
# see http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sort.html:
# "using sort to process pathnames, it is recommended that LC_ALL .. set to C"

# To speed up builds, we avoid changing "primitives" when files
# containing primitives change but the primitives table does not
gen_prim_cmd = \
  if [ -x gen_primitives.sh ]; then \
     ./gen_primitives.sh > primitives.new; \
     cmp -s primitives primitives.new || echo primitives.new; \
  else echo gen-primitives-unavailable; fi
primitives: $(shell $(gen_prim_cmd))
	cp $^ $@

prims.c : primitives
	(echo '#define CAML_INTERNALS'; \
         echo '#include "caml/mlvalues.h"'; \
	 echo '#include "caml/prims.h"'; \
   echo '#pragma GCC diagnostic ignored "-Wdeprecated-non-prototype"'; \
	 sed -e 's/.*/extern value &();/' primitives; \
	 echo 'c_primitive caml_builtin_cprim[] = {'; \
	 sed -e 's/.*/  &,/' primitives; \
	 echo '  0 };'; \
	 echo 'char * caml_names_of_builtin_cprim[] = {'; \
	 sed -e 's/.*/  "&",/' primitives; \
	 echo '  0 };') > prims.c

caml/opnames.h : caml/instruct.h
	tr -d '\r' < $< | \
	sed -e '/\/\*/d' \
	    -e '/^#/d' \
	    -e 's/enum /static char * names_of_/' \
	    -e 's/{$$/[] = {/' \
	    -e 's/\([[:upper:]][[:upper:]_0-9]*\)/"\1"/g' > $@

# caml/jumptbl.h is required only if you have GCC 2.0 or later
caml/jumptbl.h : caml/instruct.h
	tr -d '\r' < $< | \
	sed -n -e '/^  /s/ \([A-Z]\)/ \&\&lbl_\1/gp' \
	       -e '/^}/q' > $@
# These are provided as a temporary shim to allow cross-compilation systems
# to supply a host C compiler and different flags and a linking macro.
SAK_CC ?= $(CC)
SAK_CFLAGS ?= $(OC_CFLAGS) $(CFLAGS) $(OC_CPPFLAGS) $(CPPFLAGS)
SAK_LINK ?= $(MKEXE_USING_COMPILER)

sak$(EXE): sak.$(O)
	$(call SAK_LINK,$@,$^ $(if $(filter true,$(WITH_ADDRESS_SANITIZER)),-fsanitize=address -fsanitize-recover=address))

sak.$(O): sak.c caml/misc.h caml/config.h
	$(SAK_CC) -c $(SAK_CFLAGS) $(OUTPUTOBJ)$@ $<

C_LITERAL = $(shell ./sak$(EXE) encode-C-literal '$(1)')

build_config.h: $(ROOTDIR)/Makefile.config sak$(EXE)
	echo '/* This file is generated from $(ROOTDIR)/Makefile.config */' > $@
	echo '#define OCAML_STDLIB_DIR $(call C_LITERAL,$(LIBDIR))' >> $@
	echo '#define HOST "$(HOST)"' >> $@

# Libraries and programs

ocamlrun$(EXE): prims.$(O) libcamlrun.$(A)
	$(MKEXE) -o $@ $^ $(LIBS)

ocamlruns$(EXE): prims.$(O) libcamlrun_non_shared.$(A)
	$(call MKEXE_USING_COMPILER,$@,$^ $(LIBS))

libcamlrun.$(A): $(libcamlrun_OBJECTS)
	$(call MKLIB,$@, $^)

libcamlrun_non_shared.$(A): $(libcamlrun_non_shared_OBJECTS)
	$(call MKLIB,$@, $^)

ocamlrund$(EXE): prims.$(O) libcamlrund.$(A)
	$(MKEXE) $(MKEXEDEBUGFLAG) -o $@ $^ $(LIBS)

libcamlrund.$(A): $(libcamlrund_OBJECTS)
	$(call MKLIB,$@, $^)

ocamlruni$(EXE): prims.$(O) libcamlruni.$(A)
	$(MKEXE) -o $@ $^ $(INSTRUMENTED_RUNTIME_LIBS) $(LIBS)

libcamlruni.$(A): $(libcamlruni_OBJECTS)
	$(call MKLIB,$@, $^)

libcamlrun_pic.$(A): $(libcamlrunpic_OBJECTS)
	$(call MKLIB,$@, $^)

libcamlrun_shared.$(SO): $(libcamlrunpic_OBJECTS)
	$(MKDLL) -o $@ $^ $(BYTECCLIBS)

libasmrun.$(A): $(libasmrun_OBJECTS)
	$(call MKLIB,$@, $^)

libasmrund.$(A): $(libasmrund_OBJECTS)
	$(call MKLIB,$@, $^)

libasmruni.$(A): $(libasmruni_OBJECTS)
	$(call MKLIB,$@, $^)

libasmrun_pic.$(A): $(libasmrunpic_OBJECTS)
	$(call MKLIB,$@, $^)

libasmrunnnp.$(A): $(libasmrunnnp_OBJECTS)
	$(call MKLIB,$@, $^)

libasmrun_shared.$(SO): $(libasmrunpic_OBJECTS)
	$(MKDLL) -o $@ $^ $(NATIVECCLIBS)

# Target-specific preprocessor and compiler flags

%.bd.$(O): OC_CPPFLAGS += $(OC_DEBUG_CPPFLAGS)
%.bd.$(D): OC_CPPFLAGS += $(OC_DEBUG_CPPFLAGS)

%.bi.$(O): OC_CPPFLAGS += $(OC_INSTR_CPPFLAGS)
%.bi.$(D): OC_CPPFLAGS += $(OC_INSTR_CPPFLAGS)

%.bpic.$(O): OC_CFLAGS += $(SHAREDLIB_CFLAGS)

%.n.$(O): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS)
%.n.$(D): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS)

%.nd.$(O): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(OC_DEBUG_CPPFLAGS)
%.nd.$(D): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(OC_DEBUG_CPPFLAGS)

%.ni.$(O): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(OC_INSTR_CPPFLAGS)
%.ni.$(D): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(OC_INSTR_CPPFLAGS)

%.npic.$(O): OC_CFLAGS += $(SHAREDLIB_CFLAGS)
%.npic.$(O): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS)
%.npic.$(D): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS)

%.nnp.$(O): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) -DNO_NAKED_POINTERS
%.nnp.$(D): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) -DNO_NAKED_POINTERS

# The major GC performs better with this flag on Intel processors
# This is a workaround for an Intel CPU bug:
# https://www.intel.co.uk/content/www/uk/en/support/articles/000055650/processors.html
major_gc.%: OC_CFLAGS += $(INTEL_JCC_BUG_CFLAGS)

# Compilation of C files

# The COMPILE_C_FILE macro below receives as argument the pattern
# that corresponds to the name of the generated object file
# (without the extension, which is added by the macro)
define COMPILE_C_FILE
ifneq "$(COMPUTE_DEPS)" "false"
ifneq "$(1)" "%"
# -MG would ensure that the dependencies are generated even if the files listed
# in $$(BUILT_HEADERS) haven't been assembled yet. However, this goes subtly
# wrong if the user has the headers installed, as gcc will pick up a dependency
# on those instead and the local ones will not be generated. For this reason, we
# don't use -MG and instead include $(BUILT_HEADERS) in the order only
# dependencies to ensure that they exist before dependencies are computed.
$(DEPDIR)/$(1).$(D): %.c | $(DEPDIR) $(BUILT_HEADERS)
	$$(DEP_CC) $$(OC_CPPFLAGS) $$(CPPFLAGS) $$< -MT \
	  '$$*$(subst %,,$(1)).$(O)' -MF $$@
endif # ifneq "$(1)" "%"
$(1).$(O): $(2).c
else
$(1).$(O): $(2).c $(CONFIGURED_HEADERS) $(BUILT_HEADERS) $(RUNTIME_HEADERS)
endif # ifneq "$(COMPUTE_DEPS)" "false"
	$$(CC) -c $$(OC_CFLAGS) $$(CFLAGS) $$(OC_CPPFLAGS) $$(CPPFLAGS) \
	  $$(OUTPUTOBJ)$$@ $$<
endef

object_types := % %.b %.bd %.bi %.bpic
ifneq "$(NATIVE_COMPILER)" "false"
object_types += %.n %.nnp %.nd %.ni %.np %.npic
endif

$(foreach object_type, $(object_types), \
  $(eval $(call COMPILE_C_FILE,$(object_type),%)))

$(UNIX_OR_WIN32)_non_shared.%.$(O): OC_CPPFLAGS += -DBUILDING_LIBCAMLRUNS

$(eval $(call COMPILE_C_FILE,$(UNIX_OR_WIN32)_non_shared.%,$(UNIX_OR_WIN32)))

$(foreach object_type,$(subst %,,$(object_types)), \
  $(eval dynlink$(object_type).$(O): $(ROOTDIR)/Makefile.config))

# Compilation of assembly files

%.o: %.S
	$(ASPP) $(ASPPFLAGS) -o $@ $< || \
	{ echo "If your assembler produced syntax errors, it is probably";\
          echo "unhappy with the preprocessor. Check your assembler, or";\
          echo "try producing $*.o by hand.";\
          exit 2; }

%_libasmrunpic.o: %.S
	$(ASPP) $(ASPPFLAGS) $(SHAREDLIB_CFLAGS) -o $@ $<

domain_state64.inc: caml/domain_state.tbl gen_domain_state64_inc.awk
	$(AWK) -f ./gen_domain_state64_inc.awk $< > $@

domain_state32.inc: caml/domain_state.tbl gen_domain_state32_inc.awk
	$(AWK) -f ./gen_domain_state32_inc.awk $< > $@

amd64nt.obj: amd64nt.asm domain_state64.inc
	$(ASM)$@ $(ASMFLAGS) $<

i386nt.obj: i386nt.asm domain_state32.inc
	$(ASM)$@ $(ASMFLAGS) $<

%_libasmrunpic.obj: %.asm
	$(ASM)$@ $(ASMFLAGS) $<

# Dependencies

DEP_FILES := $(addsuffix .b, $(basename $(BYTECODE_C_SOURCES) instrtrace))
ifneq "$(NATIVE_COMPILER)" "false"
DEP_FILES += $(addsuffix .n, $(basename $(NATIVE_C_SOURCES)))
DEP_FILES += $(addsuffix .nnp, $(basename $(NATIVE_C_SOURCES)))
endif
DEP_FILES += $(addsuffix d, $(DEP_FILES)) \
             $(addsuffix i, $(DEP_FILES)) \
             $(addsuffix pic, $(DEP_FILES))
DEP_FILES := $(addsuffix .$(D), $(DEP_FILES))

ifeq "$(COMPUTE_DEPS)" "true"
-include $(addprefix $(DEPDIR)/, $(DEP_FILES))
endif

# This empty target is here for AppVeyor to allow dependencies to be built
# without doing anything else.
.PHONY: setup-depend
setup-depend:
