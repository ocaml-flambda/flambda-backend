/* CR mshinwell: Reverted to 5.x version, need to check conflicts */

/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Some runtime initialization functions that are common to bytecode
   and native code. */

#include <stdio.h>
#include <string.h>
#include "caml/backtrace.h"
#include "caml/memory.h"
#include "caml/callback.h"
#include "caml/major_gc.h"
#ifndef NATIVE_CODE
#include "caml/dynlink.h"
#endif
#include "caml/osdeps.h"
#include "caml/startup_aux.h"
#include "caml/prims.h"
#include "caml/signals.h"
#include "caml/gc_ctrl.h"
#include "caml/fiber.h"
#include "caml/platform.h"

#include <pthread.h>
#include <sys/resource.h>

#ifdef _WIN32
extern void caml_win32_unregister_overflow_detection (void);
#endif

/* Configuration parameters and flags */

static struct caml_params params;
const struct caml_params* const caml_params = &params;

static size_t get_pthreads_stack_size_in_bytes(void)
{
  pthread_attr_t attr;
  size_t res =
    // default value, retrieved from a recent system (May 2024)
    8 * 1024 * 1024;
  if (pthread_attr_init(&attr) == 0) {
    pthread_attr_getstacksize(&attr, &res);
    pthread_attr_destroy(&attr);
  }
  return res;
}

static void init_startup_params(void)
{
#ifndef NATIVE_CODE
  char_os * cds_file;
#endif

  // Initial stack sizes only apply in native code with stack checks disabled.

  struct rlimit rlimit;
  if (getrlimit(RLIMIT_STACK, &rlimit)) {
    // default value, retrieved from a recent system (May 2024)
    caml_init_main_stack_wsz = Wsize_bsize(8192 * 1024);
  } else {
    if (rlimit.rlim_cur == RLIM_INFINITY) {
      caml_init_main_stack_wsz = Max_stack_def;
    } else {
      caml_init_main_stack_wsz = Wsize_bsize(rlimit.rlim_cur);
    }
  }
  if (caml_init_main_stack_wsz > Max_stack_def) {
    caml_init_main_stack_wsz = Max_stack_def;
  }

  caml_init_thread_stack_wsz = Wsize_bsize(get_pthreads_stack_size_in_bytes());
  caml_init_fiber_stack_wsz = caml_init_thread_stack_wsz;

  params.init_percent_free = Percent_free_def;
  params.init_max_percent_free = Max_percent_free_def;
  params.init_minor_heap_wsz = Minor_heap_def;
  params.init_custom_major_ratio = Custom_major_ratio_def;
  params.init_custom_minor_ratio = Custom_minor_ratio_def;
  params.init_custom_minor_max_bsz = Custom_minor_max_bsz_def;
  params.init_major_heap_increment = Heap_chunk_def;
  params.init_max_stack_wsz = Max_stack_def;
  params.max_domains = Max_domains_def;
  params.runtime_events_log_wsize = Default_runtime_events_log_wsize;
  params.use_hugetlb_pages = 0;

#ifdef DEBUG
  // Silenced in oxcaml to make it easier to run tests that
  // check program output.
  // atomic_store_relaxed(&caml_verb_gc, CAML_GC_MSG_VERBOSE | CAML_GC_MSG_MINOR);
#endif
#ifndef NATIVE_CODE
  cds_file = caml_secure_getenv(T("CAML_DEBUG_FILE"));
  if (cds_file != NULL) {
    params.cds_file = caml_stat_strdup_os(cds_file);
  }
#endif
  params.trace_level = 0;
  params.cleanup_on_exit = 0;
  params.print_magic = 0;
  params.print_config = 0;
  params.event_trace = 0;
}

static void scanmult (char_os *opt, uintnat *var)
{
  char_os mult = ' ';
  unsigned int val = 1;
  sscanf_os (opt, T("=%u%c"), &val, &mult);
  sscanf_os (opt, T("=0x%x%c"), &val, &mult);
  switch (mult) {
  case 'k':   *var = (uintnat) val * 1024; break;
  case 'M':   *var = (uintnat) val * (1024 * 1024); break;
  case 'G':   *var = (uintnat) val * (1024 * 1024 * 1024); break;
  case 'v':   atomic_store_relaxed((atomic_uintnat *)var, val); break;
  default:    *var = (uintnat) val; break;
  }
}

static void parse_gc_tweak(char_os** opt_p)
{
  char_os *opt = *opt_p;
  char_os *name = opt;
  while (*opt != '\0') {
    if (*opt == '=') {
      if (opt - name == sizeof("help") -1 &&
          memcmp(name, "help", opt - name) == 0) { /* TODO: strncmp_os */
        fprintf(stderr, "Known GC tweaks:\n");
        caml_print_gc_tweaks();
      } else {
        uintnat* p = caml_lookup_gc_tweak(name, opt - name);
        if (p == NULL) {
          fprintf(stderr, "Ignored unknown GC tweak '%.*s'. "
                  "Use 'Xhelp=1' to list known tweaks\n",
                  (int)(opt - name), name);
        } else {
          scanmult(opt, p);
        }
      }
      break;
    } else {
      opt++;
    }
  }
  *opt_p = opt;
}


static void parse_ocamlrunparam(char_os* opt)
{
  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      /* keep in sync with runtime4 and with caml_runtime_parameters() */
      case 'a': break; /* Allocation policy in runtime 4 */
      case 'b': scanmult (opt, &params.backtrace_enabled); break;
      case 'c': scanmult (opt, &params.cleanup_on_exit); break;
      case 'd': scanmult (opt, &params.max_domains); break;
      case 'e': scanmult (opt, &params.runtime_events_log_wsize); break;
      case 'h': break; /* init heap size in runtime 4 */
      case 'H': scanmult (opt, &params.use_hugetlb_pages); break;
      case 'i': scanmult (opt, &params.init_major_heap_increment); break;
      case 'l': scanmult (opt, &params.init_max_stack_wsz); break;
      case 'm': scanmult (opt, &params.init_custom_minor_ratio); break;
      case 'M': scanmult (opt, &params.init_custom_major_ratio); break;
      case 'n': scanmult (opt, &params.init_custom_minor_max_bsz); break;
      case 'o': scanmult (opt, &params.init_percent_free); break;
      case 'O': scanmult (opt, &params.init_max_percent_free); break;
      case 'p': scanmult (opt, &params.parser_trace); break;
      case 'R': break; /*  see stdlib/hashtbl.mli */
      case 's': scanmult (opt, &params.init_minor_heap_wsz); break;
      case 't': scanmult (opt, &params.trace_level); break;
      case 'v': scanmult (opt, (uintnat *)&caml_verb_gc); break;
      case 'V': scanmult (opt, &params.verify_heap); break;
      case 'w': break; /* major window in runtime 4 */
      case 'W': scanmult (opt, &caml_runtime_warnings); break;
      case 'X': parse_gc_tweak(&opt); break;
      case ',': continue;
      }
      while (*opt != '\0'){
        if (*opt++ == ',') break;
      }
    }
  }

  /* Validate */
  if (params.max_domains < 1) {
    caml_fatal_error("OCAMLRUNPARAM: max_domains(d) must be at least 1");
  }
  if (params.max_domains > Max_domains_max) {
    caml_fatal_error("OCAMLRUNPARAM: max_domains(d) is too large. "
                     "The maximum value is %d.", Max_domains_max);
  }
}

#ifdef NATIVE_CODE
// Any default parameters added to an ocaml executable by passing -ocamlrunparam
// to the compiler.
// See asmcomp/asmlink.ml
extern char caml_ocamlrunparam[];
#endif

void caml_parse_ocamlrunparam(void)
{
  init_startup_params();
  caml_init_gc_tweaks();

  char_os *opt = caml_secure_getenv (T("OCAMLRUNPARAM"));
  if (opt == NULL) opt = caml_secure_getenv (T("CAMLRUNPARAM"));

#ifdef NATIVE_CODE
  parse_ocamlrunparam(caml_ocamlrunparam);
#endif

  parse_ocamlrunparam(opt);
}

/* The number of outstanding calls to caml_startup */
static int startup_count = 0;

/* Has the runtime been shut down already? */
static int shutdown_happened = 0;


int caml_startup_aux(int pooling)
{
  if (shutdown_happened == 1)
    caml_fatal_error("caml_startup was called after the runtime "
                     "was shut down with caml_shutdown");

  /* Second and subsequent calls are ignored,
     since the runtime has already started */
  startup_count++;
  if (startup_count > 1)
    return 0;

  if (pooling)
    caml_stat_create_pool();

  return 1;
}

static void call_registered_value(char* name)
{
  const value *f = caml_named_value(name);
  if (f != NULL)
    caml_callback_exn(*f, Val_unit);
}

CAMLexport void caml_shutdown(void)
{
  Caml_check_caml_state();
  if (startup_count <= 0)
    caml_fatal_error("a call to caml_shutdown has no "
                     "corresponding call to caml_startup");

  /* Do nothing unless it's the last call remaining */
  startup_count--;
  if (startup_count > 0)
    return;

  call_registered_value("Pervasives.do_at_exit");
  call_registered_value("Thread.at_shutdown");
  caml_finalise_heap();
  caml_free_locale();
#ifndef NATIVE_CODE
  caml_free_shared_libs();
#endif
  caml_stat_destroy_pool();
  caml_terminate_signals();
#if defined(_WIN32) && defined(NATIVE_CODE)
  caml_win32_unregister_overflow_detection();
#endif

  shutdown_happened = 1;
}

void caml_init_exe_name(const char_os* exe_name)
{
  params.exe_name = exe_name;
}

void caml_init_section_table(const char* section_table,
                             asize_t section_table_size)
{
  params.section_table = section_table;
  params.section_table_size = section_table_size;
}

/* Set to 1 if prelinking is in use. */
uintnat caml_prelinking_in_use = 0;
