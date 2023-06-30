[@@@ocaml.warning "+a-4-30-40-41-42"]

open Regalloc_utils

(** Splits the live ranges of registers by introducing new registers
    at destruction points. Destructions points are locations where
    all hardware registers are clobbered. This means that all registers
    live at such points would need to be spilled before and reloaded after.

    This function inserts spills and reloads, but uses new names (hence
    introducing new registers) when reloading. This results in more registers
    but a less dense interference graph that would make register allocation
    slightly easier.

    The algorithm is an adaptation of the one rewriting a CFG to put it in
    SSA form: we simply consider that new names are introduced at destruction
    points. *)
val split_live_ranges : Cfg_with_infos.t -> cfg_infos -> Regalloc_stack_slots.t
