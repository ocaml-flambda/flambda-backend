# memtrace support for `ocamlopt`

The `ocamlopt.exe` program in this directory is a version of
`ocamlopt` that supports tracing with memtrace.

Use the environment variable `MEMTRACE` to specify a trace file
location. If desire, the sampling rate can be adjusted with
`MEMTRACE_RATE`.