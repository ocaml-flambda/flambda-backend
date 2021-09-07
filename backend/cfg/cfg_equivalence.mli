val check_cfg_with_layout : Mach.fundecl -> Cfg_with_layout.t -> Cfg_with_layout.t -> unit
(** [check_cfg_with_layout f expected result] checks whether [expected] and
    [result] are equivalent, failing (through [Misc.fatal_errorf]) if they
    are not.

    The passed CFGs are deemed equivalent iff:
    - the graphs are isomorphic;
    - the layouts are equal, up to the function mapping the vertices in the
      isomorphism above;
    and blocks are deemed equivalent iff:
    - they hold equivalent instruction lists;
    - they end with equivalent terminators;
    - they have the same value for their `trap_depth`, `can_raise`, `dead`,
      and `is_trap_handler` fields;
    - their successor and predecessor sets are equivalent up to the function
      mapping the vertices in the isomorphism above.
    (Instruction identifiers are ignored when comparing both mere instructions,
    and terminators.)

    Caveat: the check currently ignores the live sets and debug information
    on all terminators, and the input registers on `Always _` terminators. *)
