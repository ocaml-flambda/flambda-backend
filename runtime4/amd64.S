/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2003 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Asm part of the runtime system, AMD64 processor */
/* Must be preprocessed by cpp */

/* PIC mode support based on contribution by Paul Stravers (see PR#4795) */

#include "caml/m.h"

#if defined(SYS_macosx)

#define LBL(x) L##x
#define G(r) _##r
#define GREL(r) _##r@GOTPCREL
#define GCALL(r) _##r
#define TEXT_SECTION(name) .text
#define FUNCTION_ALIGN 2
#define EIGHT_ALIGN 3
#define SIXTEEN_ALIGN 4
#define FUNCTION(name) \
        .globl name; \
        .align FUNCTION_ALIGN; \
        name:

#elif defined(SYS_mingw64) || defined(SYS_cygwin)

#define LBL(x) .L##x
#define G(r) r
#undef  GREL
#define GCALL(r) r
#define TEXT_SECTION(name)
#define FUNCTION_ALIGN 4
#define EIGHT_ALIGN 8
#define SIXTEEN_ALIGN 16
#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .globl name; \
        .align FUNCTION_ALIGN; \
        name:

#else

#define LBL(x) .L##x
#define G(r) r
#define GREL(r) r@GOTPCREL
#define GCALL(r) r@PLT
#if defined(FUNCTION_SECTIONS)
#define TEXT_SECTION(name) .section .text.caml.##name,"ax",%progbits
#else
#define TEXT_SECTION(name)
#endif
#define FUNCTION_ALIGN 4
#define EIGHT_ALIGN 8
#define SIXTEEN_ALIGN 16
#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .globl name; \
        .type name,@function; \
        .align FUNCTION_ALIGN; \
        name:

#endif

#if defined(SYS_linux) || defined(SYS_gnu)
#define ENDFUNCTION(name) \
        .size name, . - name
#else
#define ENDFUNCTION(name)
#endif

#ifdef ASM_CFI_SUPPORTED
#define CFI_STARTPROC .cfi_startproc
#define CFI_ENDPROC .cfi_endproc
#define CFI_ADJUST(n) .cfi_adjust_cfa_offset n
#define CFI_OFFSET(r, n) .cfi_offset r, n
#define CFI_SAME_VALUE(r) .cfi_same_value r
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#define CFI_OFFSET(r, n)
#define CFI_SAME_VALUE(r)
#endif

#ifdef WITH_FRAME_POINTERS

#define ENTER_FUNCTION \
        pushq   %rbp; CFI_ADJUST(8); \
        movq    %rsp, %rbp
#define LEAVE_FUNCTION \
        popq    %rbp; CFI_ADJUST(-8);

#else

#define ENTER_FUNCTION \
        subq    $8, %rsp; CFI_ADJUST (8);
#define LEAVE_FUNCTION \
        addq    $8, %rsp; CFI_ADJUST (-8);

#endif

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../runtime4/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state(var) (8*domain_field_caml_##var)(%r14)

#if defined(__PIC__) && !defined(SYS_mingw64) && !defined(SYS_cygwin)

/* Position-independent operations on global variables. */

/* Store [srcreg] in global [dstlabel].  Clobbers %r11. */
#define STORE_VAR(srcreg,dstlabel) \
        movq    GREL(dstlabel)(%rip), %r11 ; \
        movq    srcreg, (%r11)

#define STORE_VAR32(srcreg,dstlabel) \
        movq    GREL(dstlabel)(%rip), %r11 ; \
        movl    srcreg, (%r11)

/* Load global [srclabel] in register [dstreg].  Clobbers %r11. */
#define LOAD_VAR(srclabel,dstreg) \
        movq    GREL(srclabel)(%rip), %r11 ; \
        movq    (%r11), dstreg

/* Compare global [label] with register [reg].  Clobbers %rax. */
#define CMP_VAR(label,reg) \
        movq    GREL(label)(%rip), %rax ; \
        cmpq    (%rax), reg

/* Test 32-bit global [label] against mask [imm].  Clobbers %r11. */
#define TESTL_VAR(imm,label) \
        movq    GREL(label)(%rip), %r11 ; \
        testl   imm, (%r11)

/* Push global [label] on stack.  Clobbers %r11. */
#define PUSH_VAR(srclabel) \
        movq    GREL(srclabel)(%rip), %r11 ; \
        pushq   (%r11); CFI_ADJUST (8)

/* Pop global [label] off stack.  Clobbers %r11. */
#define POP_VAR(dstlabel) \
        movq    GREL(dstlabel)(%rip), %r11 ; \
        popq    (%r11);  CFI_ADJUST (-8)

/* Load address of global [label] in register [dst]. */
#define LEA_VAR(label,dst) \
        movq    GREL(label)(%rip), dst

#else

/* Non-PIC operations on global variables.  Slightly faster. */

#define STORE_VAR(srcreg,dstlabel) \
        movq    srcreg, G(dstlabel)(%rip)

#define STORE_VAR32(srcreg,dstlabel) \
        movl    srcreg, G(dstlabel)(%rip)

#define LOAD_VAR(srclabel,dstreg) \
        movq    G(srclabel)(%rip), dstreg

#define CMP_VAR(label,reg) \
        cmpq    G(label)(%rip), %r15

#define TESTL_VAR(imm,label) \
        testl   imm, G(label)(%rip)

#define PUSH_VAR(srclabel) \
        pushq   G(srclabel)(%rip) ; CFI_ADJUST(8)

#define POP_VAR(dstlabel) \
        popq    G(dstlabel)(%rip); CFI_ADJUST(-8)

#define LEA_VAR(label,dst) \
        leaq    G(label)(%rip), dst
#endif

/* Save and restore all callee-save registers on stack.
   Keep the stack 16-aligned. */

#if defined(SYS_mingw64) || defined(SYS_cygwin)

/* Win64 API: callee-save regs are rbx, rbp, rsi, rdi, r12-r15, xmm6-xmm15 */

// CR-someday mslater: support wider SIMD on Windows

#define PUSH_CALLEE_SAVE_REGS \
        pushq   %rbx; CFI_ADJUST (8); CFI_OFFSET(rbx, -16); \
        pushq   %rbp; CFI_ADJUST (8); CFI_OFFSET(rbp, -24); \
                      /* Allows debugger to walk the stack */ \
        pushq   %rsi; CFI_ADJUST (8); CFI_OFFSET(rsi, -32); \
        pushq   %rdi; CFI_ADJUST (8); CFI_OFFSET(rdi, -40); \
        pushq   %r12; CFI_ADJUST (8); CFI_OFFSET(r12, -48); \
        pushq   %r13; CFI_ADJUST (8); CFI_OFFSET(r13, -56); \
        pushq   %r14; CFI_ADJUST (8); CFI_OFFSET(r14, -64); \
        pushq   %r15; CFI_ADJUST (8); CFI_OFFSET(r15, -72); \
        subq    $(8+10*16), %rsp; CFI_ADJUST (8+10*16); \
        movupd  %xmm6, 0*16(%rsp); \
        movupd  %xmm7, 1*16(%rsp); \
        movupd  %xmm8, 2*16(%rsp); \
        movupd  %xmm9, 3*16(%rsp); \
        movupd  %xmm10, 4*16(%rsp); \
        movupd  %xmm11, 5*16(%rsp); \
        movupd  %xmm12, 6*16(%rsp); \
        movupd  %xmm13, 7*16(%rsp); \
        movupd  %xmm14, 8*16(%rsp); \
        movupd  %xmm15, 9*16(%rsp)

#define POP_CALLEE_SAVE_REGS \
        movupd  0*16(%rsp), %xmm6; \
        movupd  1*16(%rsp), %xmm7; \
        movupd  2*16(%rsp), %xmm8; \
        movupd  3*16(%rsp), %xmm9; \
        movupd  4*16(%rsp), %xmm10; \
        movupd  5*16(%rsp), %xmm11; \
        movupd  6*16(%rsp), %xmm12; \
        movupd  7*16(%rsp), %xmm13; \
        movupd  8*16(%rsp), %xmm14; \
        movupd  9*16(%rsp), %xmm15; \
        addq    $(8+10*16), %rsp; CFI_ADJUST (-8-10*16); \
        popq    %r15; CFI_ADJUST(-8); CFI_SAME_VALUE(r15); \
        popq    %r14; CFI_ADJUST(-8); CFI_SAME_VALUE(r14); \
        popq    %r13; CFI_ADJUST(-8); CFI_SAME_VALUE(r13); \
        popq    %r12; CFI_ADJUST(-8); CFI_SAME_VALUE(r12); \
        popq    %rdi; CFI_ADJUST(-8); CFI_SAME_VALUE(rdi); \
        popq    %rsi; CFI_ADJUST(-8); CFI_SAME_VALUE(rsi); \
        popq    %rbp; CFI_ADJUST(-8); CFI_SAME_VALUE(rbp); \
        popq    %rbx; CFI_ADJUST(-8); CFI_SAME_VALUE(rbx)

#else

/* Unix API: callee-save regs are rbx, rbp, r12-r15 */

#define PUSH_CALLEE_SAVE_REGS \
        pushq   %rbx; CFI_ADJUST(8); CFI_OFFSET(rbx, -16); \
        pushq   %rbp; CFI_ADJUST(8); CFI_OFFSET(rbp, -24); \
        pushq   %r12; CFI_ADJUST(8); CFI_OFFSET(r12, -32); \
        pushq   %r13; CFI_ADJUST(8); CFI_OFFSET(r13, -40); \
        pushq   %r14; CFI_ADJUST(8); CFI_OFFSET(r14, -48); \
        pushq   %r15; CFI_ADJUST(8); CFI_OFFSET(r15, -56); \
        subq    $8, %rsp; CFI_ADJUST(8)

#define POP_CALLEE_SAVE_REGS \
        addq    $8, %rsp; CFI_ADJUST(-8); \
        popq    %r15; CFI_ADJUST(-8); CFI_SAME_VALUE(r15); \
        popq    %r14; CFI_ADJUST(-8); CFI_SAME_VALUE(r14); \
        popq    %r13; CFI_ADJUST(-8); CFI_SAME_VALUE(r13); \
        popq    %r12; CFI_ADJUST(-8); CFI_SAME_VALUE(r12); \
        popq    %rbp; CFI_ADJUST(-8); CFI_SAME_VALUE(rbp); \
        popq    %rbx; CFI_ADJUST(-8); CFI_SAME_VALUE(rbx)

#endif

#if defined(SYS_mingw64) || defined (SYS_cygwin)
   /* Calls from OCaml to C must reserve 32 bytes of extra stack space */
#  define PREPARE_FOR_C_CALL subq $32, %rsp; CFI_ADJUST(32)
#  define CLEANUP_AFTER_C_CALL addq $32, %rsp; CFI_ADJUST(-32)
   /* Stack probing mustn't be larger than the page size */
#  define STACK_PROBE_SIZE 4096
#else
#  define PREPARE_FOR_C_CALL
#  define CLEANUP_AFTER_C_CALL
#  define STACK_PROBE_SIZE 4096
#endif

/* Registers holding arguments of C functions. */

#if defined(SYS_mingw64) || defined(SYS_cygwin)
#define C_ARG_1 %rcx
#define C_ARG_2 %rdx
#define C_ARG_3 %r8
#define C_ARG_4 %r9
#else
#define C_ARG_1 %rdi
#define C_ARG_2 %rsi
#define C_ARG_3 %rdx
#define C_ARG_4 %rcx
#endif

        .text

#if defined(FUNCTION_SECTIONS)
        TEXT_SECTION(caml_hot__code_begin)
        .globl  G(caml_hot__code_begin)
G(caml_hot__code_begin):

        TEXT_SECTION(caml_hot__code_end)
        .globl  G(caml_hot__code_end)
G(caml_hot__code_end):
#endif

        TEXT_SECTION(caml_system__code_begin)
        .globl  G(caml_system__code_begin)
G(caml_system__code_begin):
        ret  /* just one instruction, so that debuggers don't display
        caml_system__code_begin instead of caml_call_gc */

/* Allocation */

#define SAVE_XMM_REGS                               \
        subq    $(16*16), %rsp; CFI_ADJUST (16*16); \
        movupd  %xmm0, 0*16(%rsp);                  \
        movupd  %xmm1, 1*16(%rsp);                  \
        movupd  %xmm2, 2*16(%rsp);                  \
        movupd  %xmm3, 3*16(%rsp);                  \
        movupd  %xmm4, 4*16(%rsp);                  \
        movupd  %xmm5, 5*16(%rsp);                  \
        movupd  %xmm6, 6*16(%rsp);                  \
        movupd  %xmm7, 7*16(%rsp);                  \
        movupd  %xmm8, 8*16(%rsp);                  \
        movupd  %xmm9, 9*16(%rsp);                  \
        movupd  %xmm10, 10*16(%rsp);                \
        movupd  %xmm11, 11*16(%rsp);                \
        movupd  %xmm12, 12*16(%rsp);                \
        movupd  %xmm13, 13*16(%rsp);                \
        movupd  %xmm14, 14*16(%rsp);                \
        movupd  %xmm15, 15*16(%rsp);

#define RESTORE_XMM_REGS                            \
        movupd  0*16(%rsp), %xmm0;                  \
        movupd  1*16(%rsp), %xmm1;                  \
        movupd  2*16(%rsp), %xmm2;                  \
        movupd  3*16(%rsp), %xmm3;                  \
        movupd  4*16(%rsp), %xmm4;                  \
        movupd  5*16(%rsp), %xmm5;                  \
        movupd  6*16(%rsp), %xmm6;                  \
        movupd  7*16(%rsp), %xmm7;                  \
        movupd  8*16(%rsp), %xmm8;                  \
        movupd  9*16(%rsp), %xmm9;                  \
        movupd  10*16(%rsp), %xmm10;                \
        movupd  11*16(%rsp), %xmm11;                \
        movupd  12*16(%rsp), %xmm12;                \
        movupd  13*16(%rsp), %xmm13;                \
        movupd  14*16(%rsp), %xmm14;                \
        movupd  15*16(%rsp), %xmm15;                \
        addq    $(16*16), %rsp; CFI_ADJUST(-16*16);

#define SAVE_YMM_REGS                               \
        subq    $(16*32), %rsp; CFI_ADJUST (16*32); \
        vmovupd  %ymm0, 0*32(%rsp);                 \
        vmovupd  %ymm1, 1*32(%rsp);                 \
        vmovupd  %ymm2, 2*32(%rsp);                 \
        vmovupd  %ymm3, 3*32(%rsp);                 \
        vmovupd  %ymm4, 4*32(%rsp);                 \
        vmovupd  %ymm5, 5*32(%rsp);                 \
        vmovupd  %ymm6, 6*32(%rsp);                 \
        vmovupd  %ymm7, 7*32(%rsp);                 \
        vmovupd  %ymm8, 8*32(%rsp);                 \
        vmovupd  %ymm9, 9*32(%rsp);                 \
        vmovupd  %ymm10, 10*32(%rsp);               \
        vmovupd  %ymm11, 11*32(%rsp);               \
        vmovupd  %ymm12, 12*32(%rsp);               \
        vmovupd  %ymm13, 13*32(%rsp);               \
        vmovupd  %ymm14, 14*32(%rsp);               \
        vmovupd  %ymm15, 15*32(%rsp);

#define RESTORE_YMM_REGS                            \
        vmovupd  0*32(%rsp), %ymm0;                 \
        vmovupd  1*32(%rsp), %ymm1;                 \
        vmovupd  2*32(%rsp), %ymm2;                 \
        vmovupd  3*32(%rsp), %ymm3;                 \
        vmovupd  4*32(%rsp), %ymm4;                 \
        vmovupd  5*32(%rsp), %ymm5;                 \
        vmovupd  6*32(%rsp), %ymm6;                 \
        vmovupd  7*32(%rsp), %ymm7;                 \
        vmovupd  8*32(%rsp), %ymm8;                 \
        vmovupd  9*32(%rsp), %ymm9;                 \
        vmovupd  10*32(%rsp), %ymm10;               \
        vmovupd  11*32(%rsp), %ymm11;               \
        vmovupd  12*32(%rsp), %ymm12;               \
        vmovupd  13*32(%rsp), %ymm13;               \
        vmovupd  14*32(%rsp), %ymm14;               \
        vmovupd  15*32(%rsp), %ymm15;               \
        addq    $(16*32), %rsp; CFI_ADJUST(-16*32);

#define SAVE_ZMM_REGS                                \
        subq     $(32*64), %rsp; CFI_ADJUST (32*64); \
        vmovupd  %zmm0, 0*64(%rsp);                  \
        vmovupd  %zmm1, 1*64(%rsp);                  \
        vmovupd  %zmm2, 2*64(%rsp);                  \
        vmovupd  %zmm3, 3*64(%rsp);                  \
        vmovupd  %zmm4, 4*64(%rsp);                  \
        vmovupd  %zmm5, 5*64(%rsp);                  \
        vmovupd  %zmm6, 6*64(%rsp);                  \
        vmovupd  %zmm7, 7*64(%rsp);                  \
        vmovupd  %zmm8, 8*64(%rsp);                  \
        vmovupd  %zmm9, 9*64(%rsp);                  \
        vmovupd  %zmm10, 10*64(%rsp);                \
        vmovupd  %zmm11, 11*64(%rsp);                \
        vmovupd  %zmm12, 12*64(%rsp);                \
        vmovupd  %zmm13, 13*64(%rsp);                \
        vmovupd  %zmm14, 14*64(%rsp);                \
        vmovupd  %zmm15, 15*64(%rsp);                \
        vmovupd  %zmm16, 16*64(%rsp);                \
        vmovupd  %zmm17, 17*64(%rsp);                \
        vmovupd  %zmm18, 18*64(%rsp);                \
        vmovupd  %zmm19, 19*64(%rsp);                \
        vmovupd  %zmm20, 20*64(%rsp);                \
        vmovupd  %zmm21, 21*64(%rsp);                \
        vmovupd  %zmm22, 22*64(%rsp);                \
        vmovupd  %zmm23, 23*64(%rsp);                \
        vmovupd  %zmm24, 24*64(%rsp);                \
        vmovupd  %zmm25, 25*64(%rsp);                \
        vmovupd  %zmm26, 26*64(%rsp);                \
        vmovupd  %zmm27, 27*64(%rsp);                \
        vmovupd  %zmm28, 28*64(%rsp);                \
        vmovupd  %zmm29, 29*64(%rsp);                \
        vmovupd  %zmm30, 30*64(%rsp);                \
        vmovupd  %zmm31, 31*64(%rsp);

#define RESTORE_ZMM_REGS                             \
        vmovupd  0*64(%rsp), %zmm0;                  \
        vmovupd  1*64(%rsp), %zmm1;                  \
        vmovupd  2*64(%rsp), %zmm2;                  \
        vmovupd  3*64(%rsp), %zmm3;                  \
        vmovupd  4*64(%rsp), %zmm4;                  \
        vmovupd  5*64(%rsp), %zmm5;                  \
        vmovupd  6*64(%rsp), %zmm6;                  \
        vmovupd  7*64(%rsp), %zmm7;                  \
        vmovupd  8*64(%rsp), %zmm8;                  \
        vmovupd  9*64(%rsp), %zmm9;                  \
        vmovupd  10*64(%rsp), %zmm10;                \
        vmovupd  11*64(%rsp), %zmm11;                \
        vmovupd  12*64(%rsp), %zmm12;                \
        vmovupd  13*64(%rsp), %zmm13;                \
        vmovupd  14*64(%rsp), %zmm14;                \
        vmovupd  15*64(%rsp), %zmm15;                \
        vmovupd  16*64(%rsp), %zmm16;                \
        vmovupd  17*64(%rsp), %zmm17;                \
        vmovupd  18*64(%rsp), %zmm18;                \
        vmovupd  19*64(%rsp), %zmm19;                \
        vmovupd  20*64(%rsp), %zmm20;                \
        vmovupd  21*64(%rsp), %zmm21;                \
        vmovupd  22*64(%rsp), %zmm22;                \
        vmovupd  23*64(%rsp), %zmm23;                \
        vmovupd  24*64(%rsp), %zmm24;                \
        vmovupd  25*64(%rsp), %zmm25;                \
        vmovupd  26*64(%rsp), %zmm26;                \
        vmovupd  27*64(%rsp), %zmm27;                \
        vmovupd  28*64(%rsp), %zmm28;                \
        vmovupd  29*64(%rsp), %zmm29;                \
        vmovupd  30*64(%rsp), %zmm30;                \
        vmovupd  31*64(%rsp), %zmm31;                \
        addq    $(32*64), %rsp; CFI_ADJUST(-32*64);

#ifdef WITH_FRAME_POINTERS
#define SAVE_FP         ENTER_FUNCTION;
#define RESTORE_FP      LEAVE_FUNCTION;
#else
#define SAVE_FP         pushq %rbp; CFI_ADJUST(8);
#define RESTORE_FP      popq  %rbp; CFI_ADJUST(-8);
#endif
#define PASTE(...) __VA_ARGS__

#define Make_call_gc(name, SAVE_SIMD_REGS, RESTORE_SIMD_REGS) \
        LBL(caml_call_gc##name): \
        /* Record lowest stack address and return address. */ \
        movq    (%rsp), %r11; \
        movq    %r11, Caml_state(last_return_address); \
        leaq    8(%rsp), %r11; \
        movq    %r11, Caml_state(bottom_of_stack); \
        /* Touch the stack to trigger a recoverable segfault \
           if insufficient space remains */ \
        subq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(STACK_PROBE_SIZE); \
        movq    %r11, 0(%rsp); \
        addq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(-STACK_PROBE_SIZE); \
        /* Build array of registers, save it into Caml_state->gc_regs */ \
        SAVE_FP; \
        pushq   %r11; CFI_ADJUST (8); \
        pushq   %r10; CFI_ADJUST (8); \
        pushq   %r13; CFI_ADJUST (8); \
        pushq   %r12; CFI_ADJUST (8); \
        pushq   %r9; CFI_ADJUST (8); \
        pushq   %r8; CFI_ADJUST (8); \
        pushq   %rcx; CFI_ADJUST (8); \
        pushq   %rdx; CFI_ADJUST (8); \
        pushq   %rsi; CFI_ADJUST (8); \
        pushq   %rdi; CFI_ADJUST (8); \
        pushq   %rbx; CFI_ADJUST (8); \
        pushq   %rax; CFI_ADJUST (8); \
        movq    %rsp, Caml_state(gc_regs); \
        /* Save young_ptr */ \
        movq    %r15, Caml_state(young_ptr); \
        /* Save floating-point registers */ \
        PASTE SAVE_SIMD_REGS; \
        /* Call the garbage collector */ \
        PREPARE_FOR_C_CALL; \
        call    GCALL(caml_garbage_collection); \
        CLEANUP_AFTER_C_CALL; \
        /* Restore young_ptr */ \
        movq    Caml_state(young_ptr), %r15; \
        /* Restore all regs used by the code generator */ \
        PASTE RESTORE_SIMD_REGS; \
        popq    %rax; CFI_ADJUST(-8); \
        popq    %rbx; CFI_ADJUST(-8); \
        popq    %rdi; CFI_ADJUST(-8); \
        popq    %rsi; CFI_ADJUST(-8); \
        popq    %rdx; CFI_ADJUST(-8); \
        popq    %rcx; CFI_ADJUST(-8); \
        popq    %r8; CFI_ADJUST(-8); \
        popq    %r9; CFI_ADJUST(-8); \
        popq    %r12; CFI_ADJUST(-8); \
        popq    %r13; CFI_ADJUST(-8); \
        popq    %r10; CFI_ADJUST(-8); \
        popq    %r11; CFI_ADJUST(-8); \
        RESTORE_FP; \
        /* Return to caller */ \
        ret;

FUNCTION(G(caml_call_gc))
CFI_STARTPROC
Make_call_gc(, (SAVE_XMM_REGS), (RESTORE_XMM_REGS))
CFI_ENDPROC
ENDFUNCTION(G(caml_call_gc))

FUNCTION(G(caml_call_gc_avx))
CFI_STARTPROC
Make_call_gc(_avx, (SAVE_YMM_REGS), (RESTORE_YMM_REGS))
CFI_ENDPROC
ENDFUNCTION(G(caml_call_gc_avx))

FUNCTION(G(caml_call_gc_avx512))
CFI_STARTPROC
Make_call_gc(_avx512, (SAVE_ZMM_REGS), (RESTORE_ZMM_REGS))
CFI_ENDPROC
ENDFUNCTION(G(caml_call_gc_avx512))

#define Make_alloc(bytes, simd) \
        subq    $bytes, %r15; \
        cmpq    Caml_state(young_limit), %r15; \
        jb      LBL(caml_call_gc##simd); \
        ret

FUNCTION(G(caml_alloc1))
CFI_STARTPROC
Make_alloc(16,)
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc1))

FUNCTION(G(caml_alloc1_avx))
CFI_STARTPROC
Make_alloc(16,_avx)
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc1_avx))

FUNCTION(G(caml_alloc1_avx512))
CFI_STARTPROC
Make_alloc(16,_avx512)
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc1_avx512))

FUNCTION(G(caml_alloc2))
CFI_STARTPROC
Make_alloc(24,)
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc2))

FUNCTION(G(caml_alloc2_avx))
CFI_STARTPROC
Make_alloc(24,_avx)
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc2_avx))

FUNCTION(G(caml_alloc2_avx512))
CFI_STARTPROC
Make_alloc(24,_avx512)
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc2_avx512))

FUNCTION(G(caml_alloc3))
CFI_STARTPROC
Make_alloc(32,)
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc3))

FUNCTION(G(caml_alloc3_avx))
CFI_STARTPROC
Make_alloc(32,_avx)
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc3_avx))

FUNCTION(G(caml_alloc3_avx512))
CFI_STARTPROC
Make_alloc(32,_avx512)
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc3_avx512))

FUNCTION(G(caml_allocN))
CFI_STARTPROC
        cmpq    Caml_state(young_limit), %r15
        jb      LBL(caml_call_gc)
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_allocN))

FUNCTION(G(caml_allocN_avx))
CFI_STARTPROC
        cmpq    Caml_state(young_limit), %r15
        jb      LBL(caml_call_gc_avx)
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_allocN_avx))

FUNCTION(G(caml_allocN_avx512))
CFI_STARTPROC
        cmpq    Caml_state(young_limit), %r15
        jb      LBL(caml_call_gc_avx512)
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_allocN_avx512))

#define Make_call_local_realloc(SAVE_SIMD_REGS, RESTORE_SIMD_REGS) \
        /* Touch the stack to trigger a recoverable segfault \
           if insufficient space remains */ \
        subq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(STACK_PROBE_SIZE); \
        movq    %r11, 0(%rsp); \
        addq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(-STACK_PROBE_SIZE); \
        /* Build array of registers, save it into Caml_state->gc_regs */ \
        SAVE_FP; \
        pushq   %r11; CFI_ADJUST (8); \
        pushq   %r10; CFI_ADJUST (8); \
        pushq   %r13; CFI_ADJUST (8); \
        pushq   %r12; CFI_ADJUST (8); \
        pushq   %r9; CFI_ADJUST (8); \
        pushq   %r8; CFI_ADJUST (8); \
        pushq   %rcx; CFI_ADJUST (8); \
        pushq   %rdx; CFI_ADJUST (8); \
        pushq   %rsi; CFI_ADJUST (8); \
        pushq   %rdi; CFI_ADJUST (8); \
        pushq   %rbx; CFI_ADJUST (8); \
        pushq   %rax; CFI_ADJUST (8); \
        movq    %rsp, Caml_state(gc_regs); \
        /* Save young_ptr */ \
        movq    %r15, Caml_state(young_ptr); \
        /* Save floating-point registers */ \
        PASTE SAVE_SIMD_REGS; \
        /* Call the garbage collector */ \
        PREPARE_FOR_C_CALL; \
        call    GCALL(caml_local_realloc); \
        CLEANUP_AFTER_C_CALL; \
        /* Restore young_ptr */ \
        movq    Caml_state(young_ptr), %r15; \
        /* Restore all regs used by the code generator */ \
        PASTE RESTORE_SIMD_REGS; \
        popq    %rax; CFI_ADJUST(-8); \
        popq    %rbx; CFI_ADJUST(-8); \
        popq    %rdi; CFI_ADJUST(-8); \
        popq    %rsi; CFI_ADJUST(-8); \
        popq    %rdx; CFI_ADJUST(-8); \
        popq    %rcx; CFI_ADJUST(-8); \
        popq    %r8; CFI_ADJUST(-8); \
        popq    %r9; CFI_ADJUST(-8); \
        popq    %r12; CFI_ADJUST(-8); \
        popq    %r13; CFI_ADJUST(-8); \
        popq    %r10; CFI_ADJUST(-8); \
        popq    %r11; CFI_ADJUST(-8); \
        RESTORE_FP; \
        /* Return to caller */ \
        ret;

FUNCTION(G(caml_call_local_realloc))
CFI_STARTPROC
Make_call_local_realloc((SAVE_XMM_REGS), (RESTORE_XMM_REGS))
CFI_ENDPROC
ENDFUNCTION(G(caml_call_local_realloc))

FUNCTION(G(caml_call_local_realloc_avx))
CFI_STARTPROC
Make_call_local_realloc((SAVE_YMM_REGS), (RESTORE_YMM_REGS))
CFI_ENDPROC
ENDFUNCTION(G(caml_call_local_realloc_avx))

FUNCTION(G(caml_call_local_realloc_avx512))
CFI_STARTPROC
Make_call_local_realloc((SAVE_ZMM_REGS), (RESTORE_ZMM_REGS))
CFI_ENDPROC
ENDFUNCTION(G(caml_call_local_realloc_avx512))

/* Call a C function from OCaml */

FUNCTION(G(caml_c_call))
CFI_STARTPROC
LBL(caml_c_call):
    /* Record lowest stack address and return address */
        popq    Caml_state(last_return_address); CFI_ADJUST(-8)
        movq    %rsp, Caml_state(bottom_of_stack)
    /* equivalent to pushing last return address */
        subq    $8, %rsp; CFI_ADJUST(8)
    /* Touch the stack to trigger a recoverable segfault
       if insufficient space remains */
        subq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(STACK_PROBE_SIZE);
        movq    %rax, 0(%rsp)
        addq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(-STACK_PROBE_SIZE);
    /* Make the alloc ptr available to the C code */
        movq    %r15, Caml_state(young_ptr)
    /* Call the function (address in %rax) */
    /* No need to PREPARE_FOR_C_CALL since the caller already
       reserved the stack space if needed (cf. amd64/proc.ml) */
        jmp    *%rax
CFI_ENDPROC
ENDFUNCTION(G(caml_c_call))

#define Make_c_call_copy_stack_args(name, ...) \
    /* Set up a frame pointer even without WITH_FRAME_POINTERS, \
       which we use to pop an unknown number of arguments later */ \
        pushq   %rbp; CFI_ADJUST(8); \
        movq    %rsp, %rbp; \
        .cfi_def_cfa_register 6; \
    /* Copy arguments to aligned stack */ \
        cmpq   %r13, %r12; \
        je      LBL(name##_1); \
        __VA_ARGS__; \
LBL(name##_0): \
        subq    $8, %r12; \
        cmpq    %r13, %r12; \
        jb      LBL(name##_1); \
        push    (%r12); \
        jmp     LBL(name##_0); \
LBL(name##_1): \
    /* Call the function (address in %rax) */ \
        call    *%rax; \
    /* Pop arguments back off the stack */ \
        movq    %rbp, %rsp; \
        .cfi_def_cfa_register 7; \
        popq    %rbp; CFI_ADJUST(-8); \
        ret

FUNCTION(G(caml_c_call_copy_stack_args_avx))
CFI_STARTPROC
Make_c_call_copy_stack_args(caml_c_call_copy_stack_args_avx, andq $-32, %rsp)
CFI_ENDPROC
ENDFUNCTION(G(caml_c_call_copy_stack_args_avx))

FUNCTION(G(caml_c_call_copy_stack_args_avx512))
CFI_STARTPROC
Make_c_call_copy_stack_args(caml_c_call_copy_stack_args_avx512, andq $-64, %rsp)
CFI_ENDPROC
ENDFUNCTION(G(caml_c_call_copy_stack_args_avx512))

#define Make_c_call_stack_args(alignment) \
    /* Arguments: \
        C arguments         : %rdi, %rsi, %rdx, %rcx, %r8, and %r9 \
        C function          : %rax \
        C stack args        : begin=%r13 end=%r12 */ \
    /* Record lowest stack address and return address */ \
        popq    Caml_state(last_return_address); CFI_ADJUST(-8); \
        movq    %rsp, Caml_state(bottom_of_stack); \
    /* equivalent to pushing last return address */ \
        subq    $8, %rsp; CFI_ADJUST(8); \
    /* Touch the stack to trigger a recoverable segfault \
       if insufficient space remains */ \
        subq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(STACK_PROBE_SIZE); \
        movq    %rax, 0(%rsp); \
        addq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(-STACK_PROBE_SIZE); \
    /* Make the alloc ptr available to the C code */ \
        movq    %r15, Caml_state(young_ptr); \
    /* Copy the arguments and call */ \
        call   (GCALL(caml_c_call_copy_stack_args##alignment)); \
    /* Prepare for return to OCaml */ \
        movq    Caml_state(young_ptr), %r15; \
    /* Return to OCaml caller */ \
        ret

FUNCTION(G(caml_c_call_stack_args_avx))
CFI_STARTPROC
Make_c_call_stack_args(_avx)
CFI_ENDPROC
ENDFUNCTION(G(caml_c_call_stack_args_avx))

FUNCTION(G(caml_c_call_stack_args_avx512))
CFI_STARTPROC
Make_c_call_stack_args(_avx512)
CFI_ENDPROC
ENDFUNCTION(G(caml_c_call_stack_args_avx512))

/* Start the OCaml program */

FUNCTION(G(caml_start_program))
       CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Load Caml_state into r14 (was passed as an argument from C) */
        movq    C_ARG_1, %r14
    /* Initial entry point is G(caml_program) */
        LEA_VAR(caml_program, %r12)
    /* Common code for caml_start_program and caml_callback* */
LBL(caml_start_program):
    /* Build a callback link */
        pushq   Caml_state(async_exception_pointer); CFI_ADJUST (8)
        /* Stack is 16-aligned at this point */
        pushq   Caml_state(gc_regs); CFI_ADJUST(8)
        pushq   Caml_state(last_return_address); CFI_ADJUST(8)
        pushq   Caml_state(bottom_of_stack); CFI_ADJUST(8)
    /* Setup alloc ptr */
        movq    Caml_state(young_ptr), %r15
    /* Build an exception handler */
        lea     LBL(108)(%rip), %r13
        pushq   %r13; CFI_ADJUST(8)
        pushq   Caml_state(exn_handler); CFI_ADJUST(8)
        movq    %rsp, Caml_state(exn_handler)
        movq    %rsp, Caml_state(async_exception_pointer)
    /* Call the OCaml code */
        call    *%r12
LBL(107):
    /* Pop the exception handler */
        popq    Caml_state(exn_handler); CFI_ADJUST(-8)
        popq    %r12; CFI_ADJUST(-8)   /* dummy register */
LBL(109):
    /* Update alloc ptr */
        movq    %r15, Caml_state(young_ptr)
    /* Pop the callback link, restoring the global variables */
        popq    Caml_state(bottom_of_stack); CFI_ADJUST(-8)
        popq    Caml_state(last_return_address); CFI_ADJUST(-8)
        popq    Caml_state(gc_regs); CFI_ADJUST(-8)
        popq    Caml_state(async_exception_pointer); CFI_ADJUST(-8)
    /* Restore callee-save registers. */
        POP_CALLEE_SAVE_REGS
    /* Return to caller. */
        ret
LBL(108):
    /* Exception handler*/
    /* Mark the bucket as an exception result and return it */
        orq     $2, %rax
        jmp     LBL(109)
CFI_ENDPROC
ENDFUNCTION(G(caml_start_program))

/* Raise an exception from OCaml */

FUNCTION(G(caml_raise_exn))
CFI_STARTPROC
        testq   $1, Caml_state(backtrace_active)
        jne     LBL(110)
        movq    Caml_state(exn_handler), %rsp
        popq    Caml_state(exn_handler); CFI_ADJUST(-8)
        ret
LBL(110):
        movq    %rax, %r12            /* Save exception bucket */
        movq    %rax, C_ARG_1         /* arg 1: exception bucket */
#ifdef WITH_FRAME_POINTERS
        ENTER_FUNCTION
        movq    8(%rsp), C_ARG_2      /* arg 2: pc of raise */
        leaq    16(%rsp), C_ARG_3     /* arg 3: sp at raise */
#else
        popq    C_ARG_2               /* arg 2: pc of raise */
        movq    %rsp, C_ARG_3         /* arg 3: sp at raise */
#endif
        /* arg 4: sp of handler */
        movq    Caml_state(exn_handler), C_ARG_4
        /* PR#5700: thanks to popq above, stack is now 16-aligned */
        /* Thanks to ENTER_FUNCTION, stack is now 16-aligned */
        PREPARE_FOR_C_CALL            /* no need to cleanup after */
        call    GCALL(caml_stash_backtrace)
        movq    %r12, %rax            /* Recover exception bucket */
        movq    Caml_state(exn_handler), %rsp
        popq    Caml_state(exn_handler); CFI_ADJUST(-8)
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_raise_exn))

/* Raise an exception from C */

FUNCTION(G(caml_raise_exception))
CFI_STARTPROC
        movq    C_ARG_1, %r14   /* Caml_state */
        testq   $1, Caml_state(backtrace_active)
        jne     LBL(112)
        movq    C_ARG_2, %rax
        movq    Caml_state(exn_handler), %rsp  /* Cut stack */
        /* Recover previous exception handler */
        popq    Caml_state(exn_handler); CFI_ADJUST(-8)
        movq    Caml_state(young_ptr), %r15 /* Reload alloc ptr */
        ret
LBL(112):
#ifdef WITH_FRAME_POINTERS
        ENTER_FUNCTION          ;
#endif
        /* Save exception bucket. Caml_state in r14 saved across C calls. */
        movq    C_ARG_2, %r12
        /* arg 1: exception bucket */
        movq    C_ARG_2, C_ARG_1
        /* arg 2: pc of raise */
        movq    Caml_state(last_return_address), C_ARG_2
        /* arg 3: sp of raise */
        movq    Caml_state(bottom_of_stack), C_ARG_3
        /* arg 4: sp of handler */
        movq    Caml_state(exn_handler), C_ARG_4
#ifndef WITH_FRAME_POINTERS
        subq    $8, %rsp              /* PR#5700: maintain stack alignment */
#endif
        PREPARE_FOR_C_CALL            /* no need to cleanup after */
        call    GCALL(caml_stash_backtrace)
        movq    %r12, %rax            /* Recover exception bucket */
        movq    Caml_state(exn_handler), %rsp
     /* Recover previous exception handler */
        popq    Caml_state(exn_handler); CFI_ADJUST(-8)
        movq    Caml_state(young_ptr), %r15 /* Reload alloc ptr */
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_raise_exception))

/* Raise a Stack_overflow exception on return from segv_handler()
   (in runtime/signals_nat.c).  On entry, the stack is full, so we
   cannot record a backtrace.
   No CFI information here since this function disrupts the stack
   backtrace anyway. */

FUNCTION(G(caml_stack_overflow))
        movq    C_ARG_1, %r14                 /* Caml_state */
        LEA_VAR(caml_exn_Stack_overflow, %rax)
        movq    Caml_state(async_exception_pointer), %rsp /* cut the stack */
     /* Recover previous exn handler */
        popq    Caml_state(exn_handler)
        ret                                   /* jump to handler's code */
ENDFUNCTION(G(caml_stack_overflow))

/* Callback from C to OCaml */

FUNCTION(G(caml_callback_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r14      /* Caml_state */
        movq    C_ARG_2, %rbx      /* closure */
        movq    0(C_ARG_3), %rax   /* argument */
        movq    0(%rbx), %r12      /* code pointer */
        jmp     LBL(caml_start_program)
CFI_ENDPROC
ENDFUNCTION(G(caml_callback_asm))

FUNCTION(G(caml_callback2_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r14      /* Caml_state */
        movq    C_ARG_2, %rdi      /* closure */
        movq    0(C_ARG_3), %rax   /* first argument */
        movq    8(C_ARG_3), %rbx   /* second argument */
        LEA_VAR(caml_apply2, %r12) /* code pointer */
        jmp     LBL(caml_start_program)
CFI_ENDPROC
ENDFUNCTION(G(caml_callback2_asm))

FUNCTION(G(caml_callback3_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r14      /* Caml_state */
        movq    0(C_ARG_3), %rax   /* first argument */
        movq    8(C_ARG_3), %rbx   /* second argument */
        movq    C_ARG_2, %rsi      /* closure */
        movq    16(C_ARG_3), %rdi  /* third argument */
        LEA_VAR(caml_apply3, %r12) /* code pointer */
        jmp     LBL(caml_start_program)
CFI_ENDPROC
ENDFUNCTION(G(caml_callback3_asm))

FUNCTION(G(caml_ml_array_bound_error))
CFI_STARTPROC
        LEA_VAR(caml_array_bound_error, %rax)
        jmp     LBL(caml_c_call)
CFI_ENDPROC
ENDFUNCTION(G(caml_ml_array_bound_error))

FUNCTION(G(caml_ml_array_align_error))
CFI_STARTPROC
        LEA_VAR(caml_array_align_error, %rax)
        jmp     LBL(caml_c_call)
CFI_ENDPROC
ENDFUNCTION(G(caml_ml_array_align_error))

        TEXT_SECTION(caml_system__code_end)
        .globl  G(caml_system__code_end)
G(caml_system__code_end):

        .data
        .globl  G(caml_system__frametable)
        .align  EIGHT_ALIGN
G(caml_system__frametable):
        .quad   1           /* one descriptor */
        .4byte   LBL(107) - .    /* return address into callback */
        .value  -1          /* negative frame size => use callback link */
        .value  0           /* no roots here */
        .align  EIGHT_ALIGN

#if defined(SYS_macosx)
        .literal16
#elif defined(SYS_mingw64) || defined(SYS_cygwin)
        .section .rdata,"dr"
#else
        .section    .rodata.cst16,"aM",@progbits,16
#endif
        .globl  G(caml_negf_mask)
        .align  SIXTEEN_ALIGN
G(caml_negf_mask):
        .quad   0x8000000000000000, 0
        .globl  G(caml_absf_mask)
        .align  SIXTEEN_ALIGN
G(caml_absf_mask):
        .quad   0x7FFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF
        .globl  G(caml_negf32_mask)
        .align  SIXTEEN_ALIGN
G(caml_negf32_mask):
        .quad   0x80000000, 0
        .globl  G(caml_absf32_mask)
        .align  SIXTEEN_ALIGN
G(caml_absf32_mask):
        .quad   0xFFFFFFFF7FFFFFFFL, 0xFFFFFFFFFFFFFFFF

#if defined(SYS_linux)
    /* Mark stack as non-executable, PR#4564 */
        .section .note.GNU-stack,"",%progbits
#endif
