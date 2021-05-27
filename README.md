# memtrace

A streaming client for OCaml's Memprof, which generates compact traces
of a program's memory use.

To profile the memory use of a program, start by putting this line
somewhere at the program startup:

    Memtrace.trace_if_requested ~context:"my program" ();;

If the `MEMTRACE` environment variable is present, tracing begins to
the filename it specifies. (If it's absent, nothing happens)

The ~context parameter is optional, and can be set to any string that
helps to identify the trace file.

If the program daemonises, the call to `trace_if_requested` should
occur *after* the program forks, to ensure the right process is
traced.

The resulting trace files can be analysed with some simple
command-line tools in bin/, but the recommended interface is the
memtrace viewer, which lives at:

    https://github.com/janestreet/memtrace_viewer
