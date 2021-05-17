# The enthusiast's guide to Memtrace

Memtrace is a tracing system for profiling the memory usage and
allocation patterns of OCaml programs. This guide explains how it
works internally, and describes some of the efficiency tricks it
uses. (If your goal is learning to use Memtrace rather than learning
how it works, this is not the document for you).

Memtrace files consist of a sequence of timestamped GC events:
allocations, promotions and collections. Each allocation event carries
a backtrace, captured at the time of the allocation. Each promotion
or collection event is associated with a specific allocation, allowing
analysis of object lifetimes.

A common source of trouble with tracing systems is that the resulting
trace files can grow large, making them annoying to transport and slow
to process. Traces can also be fragile, requiring the exact original
binary (and any loaded shared libraries or plugins) to interpret
correctly.

Memtrace was specifically designed to avoid these issues, producing
compact self-contained traces. It's instructive to compare the trace
sizes with those produced by `perf record`. Perf is a very different
system from memtrace, sampling based on time rather than GC
behaviour. However, the resulting trace files are similar, as they
also consist of a sequence of timestamped events with associated
backtraces.

The graph below shows the trace sizes in average bytes / sample,
across several runs of a benchmark with
different sample rates. The comparison is a little unfair to memtrace:
first, memtrace stores more information per sample than perf
(e.g. object length, whether an allocation was in the major heap).
Second, each sample in memtrace may produce up to three concrete
timestamped events (allocation + promotion + collection), and the
sizes shown below are the sum of the three, compared to perf's single
event per sample.

![Sizes of memtrace vs. perf traces](memtrace-vs-perf-sizes.png)

As we can see, at the same sample rate memtrace traces are more than
an order of magnitude smaller than perf's, no matter which of perf's
three backtrace mechanisms (`lbr`, `fp` or `dwarf`) is in use. When
perf's backtraces are disabled entirely (`perf-none` above), perf
still produces somewhat larger files than memtrace, due to a less
efficient binary format.

To analyse these traces, perf requires access to the original 167 MB
binary. Memtrace, on the other hand, stores enough debug info inline
in the trace to recover full location information without needing the
binary. In the benchmarks above, the total size of this debug info
ranges from 10 KB to 70 KB (and is not shown in the graph above).

There are two aspects to tracing with Memtrace: sampling allocations,
and efficiently encoding the sampled data.


## Sampling allocations with Gc.Memprof

The sampling engine behind memtrace is OCaml's new `Gc.Memprof`
module.

`Gc.Memprof` accepts a *sampling rate* and a set of callbacks. The
sampling rate is in units of probability: at rate `1e-5`, each
allocated word has a one in 10^5 chance of being sampled. Since the
sampling rate is per word rather than per allocation, larger
allocations are more likely to be sampled.

Since each word allocated has an independent chance of being
sampled, the number of words until the next sample is distributed
according to a geometric distribution. `Gc.Memprof` draws from this
geometric distribution, and takes a sample after that much memory has
been allocated.

OCaml uses a *bump-pointer* allocator on its minor heap: allocations
work by subtracting the desired amount of memory from the minor heap
pointer, and comparing to the minor heap limit. Rather than
introducing an extra branch, `Gc.Memprof` works by changing the minor
heap limit to the position of the next sample, as though the minor
heap ended at that point.

Eventually, an allocation will fail when it hits this limit, using
OCaml's existing heap-limit check. `Gc.Memprof` then invokes a callback,
and resets the minor heap limit by re-drawing from the geometric
distribution. This minimises overhead: the only branch needed is the
heap-limit check that's already present.

When an allocation is sampled, it's added to an internal GC data
structure so that `Gc.Memprof` can invoke callbacks when it is
promoted to the major heap and/or collected. This way, the entire
lifetime of a sampled object is visible.


### Comballoc

OCaml's `comballoc` optimisation introduces a major complication to
this scheme. Multiple consecutive allocations can be combined into a
single allocation, with a single heap-limit test. Although these are
allocated simultaneously, these allocations may not have the same
lifetime. They may not even have the same backtrace, as they can be
the result of different levels of inlining.

The debug information used to convert backtraces to readable form was
extended to deal with this case, maintaining multiple debug entries
for a single allocation instruction when allocations are combined.
This means that if an allocation of 2 words is combined with an
adjacent allocation of 3 words, about 40% of the samples will be
associated with the smaller object and about 60% with the larger.


### Collecting backtraces efficiently

`Gc.Memprof` collects a backtrace on each sampled allocation, which is
something that needs to be efficient.

Collecting a backtrace efficiently at an arbitrary point in an
arbitrary program is hard. There are three standard approaches,
available as options in `perf record`:

  - `--call-graph=dwarf` uses the DWARF debugging information
    
  - `--call-graph=fp` follows a chain of frame pointers
  
  - `--call-graph=lbr` uses the Last Branch Record hardware support

However, all of these have disadvantages:

  - DWARF exists to support debuggers, and so is designed for
    flexibility rather than speed.
    
    This flexibility is necessary to handle the hard cases of C stack
    frames: for instance, a C program can define a variable-length
    array of ints on the stack, and store its length (in ints, not
    bytes) in a callee-save register that later gets spilled. So, in
    order to work out the length of a C stack frame, you may need to
    walk several *other* frames to find the register and do some
    arbitrary arithmetic to compute the length.
    
    So, the DWARF format allows stack frames to attach an arbitrary
    program in the DWARF bytecode language to express how to find the
    next frame. Decoding these is slow and complicated, and perf
    doesn't try: when sampling, it simply copies a chunk of stack (8KB
    by default) to the trace, to decode offline. This results in very
    large traces, which can still be incomplete as OCaml programs often
    use more than 8KB of stack.
    
  - Frame pointers change the function calling convention to
    continually maintain a linked list of stack frames so that it's
    available should a backtrace be taken. This adds a small, but
    nonzero overhead to every function call and return, and reserves a
    register. It also requires a special build, as OCaml does not use
    frame pointers by default.
    
  - LBR is generally the best option, but has a couple of limitations:
    First, it requires hardware support which is not currently available on
    UID boxes, and second, where available it is implemented with a fixed-size
    hardware ring buffer (32 entries on Skylake). Should the ring
    buffer overflow, entries are dropped, so deep stacks cannot be
    accurately recorded. (However, there is some very recent
    experimental support in perf for [heuristically stitching together
    LBR stacks from different
    samples](https://lwn.net/Articles/802821/), which may help).

Happily, `Gc.Memprof` doesn't have to collect a backtrace at an
arbitrary point in an arbitrary program, but only a backtrace at an
allocation site in an OCaml program.  Because OCaml is a
garbage-collected language with precise marking, it already needs to
be able to traverse the stack at allocation sites. This traversal needs
to be accurate and efficient, because it's done at every GC. It's
implemented with a large hashtable, which maps every return address
and allocation site to a `frame_descr`, which contains the length of
the stack frame (amongst other information). Traversing the stack
using this hashtable is almost as efficient as following frame
pointers, but without the runtime overhead.


## Compressing traces with memtrace

The job of the memtrace library is to take the information collected
by `Gc.Memprof` and stream it efficiently to a file. The largest
portion of this data by far is the backtraces associated with each
allocation.

Backtraces arrive as a sequence of entries (opaque 64-bit integers),
which you can think of as return addresses. (They're actually pointers
to a static structure generated by the OCaml compiler, which includes
the return address and a couple of other bits of information). Each of
these entries can be *decoded* into locations, which contain a source
filename, line/column position, and function name. Note that a single
entry may yield several locations, due to inlining.

Naively encoding these backtraces would be expensive. OCaml programs
often have deep stacks, with dozens or hundreds of entries in a
backtrace. Even though the entries themselves are only 8 bytes long,
the location information can be much larger. (Remember, it is a design
goal of memtrace to include all relevant location information in the
trace, rather than relying on the user to hold onto the original binary)

Instead, memtrace uses a series of optimisations to encode backtraces
efficiently.

### Common prefixes

The first optimisation is to observe that the backtraces from
consecutive samples often have a common prefix. There are two reasons
for this:

  - Consecutive samples are close together in time. Whatever the
    program was doing during the last sample, there's a good chance
    it's still doing it.

  - Many programs have some driver structure wrapping the main
    function, causing there to be the same few entries at
    the start of nearly every single backtrace.

So, the first thing memtrace does is compare the start of each
backtrace to the start of the last. The number of common entries is
written to the trace file, and those entries are skipped entirely,
saving space.


### Location information and caching

Since the location information is much larger than the 8-byte
backtrace entry, it's important to avoid encoding it redundantly.
Memtrace keeps a cache of previously-seen backtrace entries, and
location information is only decoded and written to the trace file on
cache misses. In other words, location information need only be
written to a trace file the first time a given location is seen. Since
most backtraces hit a relatively small set of locations, this cache
has an extremely high hit rate.

There are also some minor optimisations in writing the location
information: first, the line and column numbers are written in the
same bit-packed format that OCaml uses internally, and second, file
and function names use a simple 31-element move-to-front coder, so
that file and function names need not be written if they are among the
31 most recently used names.


### Cache design

In order to ensure good performance and to bound memory use, memtrace
uses a fixed-size cache rather than letting it grow without bound.
This does mean that it's possible for the location information about
an entry to be written redundantly: if an entry is evicted from the
cache then its locations will have to be written again the next time
it's seen. However, this does not happen much in practice.

For speed, the cache is a not a full LRU cache, but rather a two-way
skewed-associative cache. In this cache design, each entry is hashed
twice, giving two cache buckets. If either contains the entry, it's a
hit, otherwise the older of the two is evicted.


### Encoding cache indices

The fixed-size cache has 16384 (i.e. `2^14`) entries. In the
overwhelmingly common case of a cache hit, memtrace writes the bucket
index rather than the entry to the trace file. This means that the
64-bit backtrace entry is instead encoded in 14 bits.

The full 64-bit backtrace entries need only be written during cache
misses, along with the cache bucket index into which they're being
inserted. This ensures there's enough information in the trace for the
trace reader to reconstruct the cache state and know which buckets
map to which backtrace entries.

The 14-bit cach bucket index word is written in two bytes, leaving a
two-bit tag having four possible states. One of these states is used
for cache misses, but the other three are used for a further
optimisation: prediction.


### Prediction

It is often possible to predict the next entry in a backtrace: most of
the time, the target of a function call is the same as it was last
time.  So, memtrace maintains an extra field in its cache: in each
cache bucket, we also store the cache bucket of the entry that
previously followed this one. This is used as a prediction: if
correct, we can encode backtraces more compactly.

Specifically, after every cache hit, we follow the chain of
predictions until it mispredicts something. The cache is then encoded
as one of three cases: 0 correct predictions, 1 correct prediction, or
up to 256 correct predictions as specified by a supplemental byte.

This encoding is extremely effective for long chains of
non-tail-recursive functions. For instance, suppose we are encoding a
backtrace that's using the OCaml stdlib's non-tail-recursive
`List.map`, where the backtrace consists of 200 frames of `List.map`
followed by one frame of some other function doing an allocation.

This backtrace gets encoded in 7 bytes:

  - 2 bytes: the first `List.map` frame and 0 correct predictions.
    (assuming we get unlucky with the prediction here)
    
  - 2 bytes: the next `List.map` frame. The prediction for `List.map`
    was updated by the previous frame, so now predicts correctly.
    
  - 1 byte: the number 198, indicating 198 correct predictions
    following the previous frame.
    
  - 2 bytes: the actual function doing the allocation.
  
(Of course, it's possible that we get lucky on the first frame, and
encode this backtrace in 5 bytes)


### More tricks?

There is a lot more that could be done here. The prediction model is
very simplistic, always predicting whichever way it went last time.
There is much room for improvement:

  - we could allow multiple possible predictions, allowing more than 
    one possibility to be encoded efficiently (eventually leading to
    PPM-style compressors)

  - we could use more than one previous frame as context for the next,
    or do LZ77-style substring matches.
    
  - we could allow weak/strong predictions as used in CPU branch
    predictors, where a prediction that's worked in the past must be
    wrong more than once to be replaced.

However, even the relatively simple caching / prediction model
currently in use is highly effective: the average encoded size is
on the order of 10 bytes for an entire backtrace.


### Other optimisations

In fact, with the optimisations above backtraces became sufficiently
short that the fixed-size fields of the trace (timestamps, etc.)
started to become a significant fraction of the total file size. So,
memtrace currently does a number of packing tricks to keep size down
there too:


  - Integers that are usually small use a variable-length encoding,
    meaning most of them fit in one byte.
  
  - Allocation IDs are used in promotion and collection events to
    specify which block was collected. They have a relative encoding:
    instead of "collect allocation N", they encode "collect the Nth
    most recent allocation". This means that N is usually small and
    profits from the variable-length integer encoding.
  
  - The most common cases (small allocations on the minor heap, with
    only one sample, and <= 256 backtrace entries) are given special
    event codes to save space.

  - Timestamps are truncated, allowing the timestamp and the event
    type field to fit in the same 32-bit word. Short timestamps
    overflow after just over 30 seconds, but the format ensures there
    is at least one full 64-bit timestamp every 30 seconds (in a CTF
    packet header), so all timestamps can be decoded unambiguously.


### For more

If you're curious about the exact format, go look at
`src/memtrace.tsdl` in the memtrace sources, which specifies the whole
format in CTF's TSDL language.
