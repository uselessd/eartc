eartc
=====

eartc is a simple Erlang port/cmdline interface to the Approximate
Replay-Trace Compiler, a tool used for I/O benchmarking and replaying
strace logs. Exported functions are init_listing/1 (for the geninit
phase), trace/1 (for running strace-artc), compile/1 (for compiling
the .init and .strace files into a directory plus the resulting bench.so
in one go) and replay for rerunning a trace from a bench.so with all
debug options enabled.

All files are stored in /usr/share/eartc/.

It is still very rudimentary and will progressively be evolving.

Build
-----

    $ rebar3 compile

License
-----

MPLv2.
