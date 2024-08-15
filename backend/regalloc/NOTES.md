# Register allocators

There are currently 3 register allocators available when using the CFG pipeline.
The allocator is selected through the `-regalloc` command-line parameter
followed by:

- `irc` for the IRC allocator;
- `ls` for the linscan allocator;
- `gi` for the greedy-inspired allocator.

The `-regalloc-validate` command-line parameter controls whether the validator
should be run; it must be followed by either `on` or `off`.

In addition to these two main command-line parameters, each allocator provides a
number of additional parameters to control its behaviour. These parameters are
passed through the `-regalloc-param` command-line parameter followed by a
`NAME:VALUE` binding. The parameters available for the various allocators are
described in the sections below.


## Common elements

### Pre- and post-lude

A number of elements are shared by all allocators: *pre* and *post* passes, and
miscellaneous utilities. The *pre* passes are mainly run from the
`Regalloc_rewrite.prelude` function:

- invariant checks (`Regalloc_invariants.precondition`);
- the split/rename phase splits the live ranges of temporaries by introducing
  new names at destruction points (at such points, all live temporaries will
  spill, so reloading them under a new name is likely beneficial);
- (A snapshot for the validator may be taken by calling the
  `Regalloc_validate.Description.create` function from
  `Asmgen.compile_fundecl`);

and the *post* passes are run from the `Regalloc_rewrite.postlude` function:

- optimization of stack slots (`Regalloc_stack_slots.optimize`): if stack slots
  are used on disjoint intervals, they can be merged;
- invariant checks (`Regalloc_invariants.postcondition_liveness`).


### Rewrite

The `Regalloc_rewrite` module also defines a `rewrite_gen` function that
implements the rewrite function of IRC, but is also used in other allocators.
The purpose of the rewrite function is to introduce spill and reload instructions
for a list of registers the allocator has decided should be spilled. This
means that the registers will have their values stored on the stack. In turn,
it implies that before each read (resp. after each write) of the value, a reload
(resp. spill) instruction needs to be inserted. The value is reloaded into
(resp. spilled from) a fresh *temporary*, whose live range is covers only
the instruction reading and/or writing the value. After rewrite, the spilled
registers no longer appear in the CFG. All occurrences have been replaced with
temporaries with very short live ranges, thus making the allocation problem
easier to solve.

A drawback of this transformation is that we may introduce too many temporaries,
for instance reloading the very same value repeatedly. Indeed, the `rewrite_gen`
function operates at the instruction level as described above, so if two
instructions need to read the same spilled register, two reloads will be
inserted, even when we can easily determine the value has not changed on the
stack between the two instructions. For this reason, an option has been added to
the function, that will in effect introduce temporaries at the *block* level
rather than at the *instruction* level.

The trade-off between *block* and *instruction* temporaries is of course
that the former introduce fewer temporaries but whose live ranges are longer,
making the problem more difficult to solve. The allocator must thus decide
whether to enable the optimization, and retain the possibility to later
effectively turn *block* temporaries into *instruction* temporaries.


### Split/rename

The split/rename preprocessing phase is implemented by mainly by 2 modules:

- `Regalloc_split_state`, which is in charge of determining the destruction
  points, optimizing where the spills and reloads will happen, and computing
  where phi moves should be inserted;
- `Regalloc_split`, which is in charge of computing and applying the
  substitution and inserting the actual spill, reload, and phi moves to the
  basic blocks, using the information computed by `Regalloc_split_state`.

Since we know that at a destruction point all registers will be destroyed,
spills and reloads will need to be inserted around the destruction for all
the registers live across the destruction point. It is thus beneficial to do
so as a preprocessing phase rather than during allocation per se:

- if we don't, the first round of the allocator will simply "discover" that
  the registers need to be spilled/reloaded, thus essentially wasting a
  round;
- worse, an allocator such as IRC will decide that the location of the
  register is on the stack (and insert reloads and spills as described
  above in the "Rewrite" section) while by introducing new names after
  destruction points we are splitting the live ranges, making the problem
  easier to solve and allowing the allocator to make finer-grained
  decisions.


### Stack operands

The last element common to all register allocators is the
`Regalloc_stack_operands` module, whose actual implementation is
architecture-dependent. The module is used by the allocator to determine when a
stack operand can be used instead of moving the data into a register beforehand.


## IRC

### Overview

The IRC allocator is described in the paper from George and Appel:

  Iterated register coalescing
  George, L., & Appel, A. W. (1996)
  ACM Transactions on Programming Languages and Systems (TOPLAS)
  [ACM link](https://dl.acm.org/doi/abs/10.1145/229542.229546)

and in the book from Appel:

  Modern Compiler Implementation in ML
  Appel, A. W. (1998)
  Cambridge University Press
  [Book page](https://www.cs.princeton.edu/~appel/modern/ml/)

The implementation is a direct port of the algorithm described in these
resources. It can be controlled by the following parameters:

- `IRC_SPILLING_HEURISTICS` (`set-choose`, `flat-uses`, or `hierarchical-uses`):
  the heuristics used to determine which temporary to spill, with `set-choose`
  selecting a temporary by using the `Set.choose` function, and the other
  options selecting the temporary with the lowest estimated spilling cost (both
  are counting the number of uses, the hierarchical option using the information
  about loops to give more weight to a use inside a loop);
- `IRC_VERBOSE` (`on` or `off`): whether to produce a log describing each and
  every step of the algorithm;
- `IRC_INVARIANTS` (`on` or `off`): whether to check the invariants.


### Modules and types

`Regalloc_irc_utils` defines the logging function, as well as sets represented
as dynamic sorted arrays (`ArraySet.S`). The state (`Regalloc_irc_state.t`)
simply maintains the various work lists used by the algorithm, and described in
the paper and book. `Regalloc_irc` also follows the description of the algorithm
by defines the same functions as the paper/book.


## Linscan

### Overview

The implementation is a direct port of upstream's algorithm (`Interval` and
`Linscan` modules), only adapted to the CFG pipeline. The main difference is the
use of the `Regalloc_rewrite.rewrite_gen` function (from IRC) when temporaries
need to be spilled. It can be controlled by the following parameters:

- `LS_VERBOSE` (`on` or `off`): whether to produce a log describing each and
  every step of the algorithm;
- `LS_INVARIANTS` (`on` or `off`): whether to check the invariants.


### Modules and types

`Regalloc_ls_utils` defines the logging functions, and the range and interval
types. A range (`Range.t`) represents the continuous use of a temporary, while
an interval (`Interval.t`) is a sequence of ranges, thus representing all the
uses of a temporary.

The state (`Regalloc_ls_state.t`) maintains the list of intervals that need to
be assigned, as well as per-class list of already assigned intervals.


## Greedy

### Overview

The "greedy" allocator is loosely based on LLVM's greedy allocator. A nice
high-level description of the allocator is provided by the following slide deck:

  LLVM Greedy Register Allocator – Improving Region Split Decisions
  Marina Yatsina
  2018 European Developers Meeting
  [LLVM link](https://llvm.org/devmtg/2018-04/slides/Yatsina-LLVM%20Greedy%20Register%20Allocator.pdf)

The central element of the algorithm is a priority queue. At the start of the
algorithm, all temporaries are added to the queue using a heuristics to
determine their priorities. The main loop of the algorithm then extracts the
temporary with the highest priority and tries to assign it to a hardware
register. If no hardware registers are available for the temporary, there are
three possible ways to progress towards a solution:

1. the temporary is given a hardware register by evicting a set of temporaries
   that conflict with it, putting back the evicted temporaries into the priority
   queue;
2. the temporary's interval is split and each part is given a new name, thus
   creating new temporaries, then all the new temporaries are added to the
   priority queue;
3. the temporary is spilled.

The current version of the algorithm does not support split, and relies on IRC's
rewrite function for spilling. It loops until the priority queue is empty,
putting the temporaries in a *spilling* list if no hardware registers are
available and no eviction is deemed beneficial. Then, it actually spills the
temporaries from the *spilling* list and restarts the algorithm.

It can be controlled by the following parameters:

- `GI_PRIORITY_HEURISTICS` (only `interval-length` at this point) selects the
  heuristics to use to compute the priority of a register when adding it to the
  queue;
- `GI_SELECTION_HEURISTICS` (`first-available`, `best-fit`, or `worst-fit`) sets
  the strategy to use when trying to find a free register;
- `GI_SPILLING_HEURISTICS` (`flat-uses`, or `hierarchical-uses`), with the same
  semantics as with IRC (See above);
- `GI_VERBOSE` (`on` or `off`): whether to produce a log describing each and
  every step of the algorithm;
- `GI_INVARIANTS` (`on` or `off`): whether to check the invariants.


### Modules and types

`Regalloc_gi_utils` defines the logging functions, and the `Range.t` and
`Interval.t` types with essentially the same semantics as for linscan (See
above). The also provides an implementation of a "max" priority queue backed by
an array. Finally, `Hardware_register.t` is defined to represent a single
register with its location (class and index in class), and `Hardware_registers`
is defined to represent all registers. `Hardware_registers.find_available` is
used to find a free register, or a valuable eviction.

The loop of the algorithm described above is implemented in `Regalloc_gi.main`.
The current version restarts from scratch when spilling (after calling the
`rewrite` function of IRC), but eventually we should enter the main loop of the
algorithm only once.

The code currently contains copy/paste of elements from Linscan, because we
expect to diverge from linscan in some places and it seems easier to refactor
both linscan and greedy to share elements once the dust settles. It is also
likely we should engage in larger-scale refactoring of all three allocators once
the code of greedy stabilizes (e.g. share the logging functions, and the
"verbose" and "invariant" parameters).
