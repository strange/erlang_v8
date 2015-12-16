
    erlang_v8_vm_sup + hash_ring
    erlang_v8_vm

    PROC1 -> OS1
    PROC2 -> OS2

    {ok, Context} = create_context().
    {ok, Value} = eval(Context, Source).
    {ok, Value} = call(Context, Fun, Args).
    ok = destroy_context(Context).

1. Timeouts
2. Report invalid contexts
3. Collect "dead" contexts

# `erlang_v8`

Run JavaScript from Erlang in an external OS process.

This is just an experiment to see if embedding v8 in an actual OS process is
more predictable than using a port driver or NIF. I will give the project
proper attention if the experiment works out.

The most notable features: 

- You can `eval/3` things like "while (true) {}" with a timeout and have the
  v8 VM actually terminate when it times out (the OS process is killed and
  restarted).
- The context of the VM can then be reset (the current implementation is
  pretty naive; I'm working on a more elegant solution). This is useful when
  you want multiple parties to share the same VM(s).
- A VM can be initialized with pre-defined source that's loaded into the OS
  process and automatically evaluated when the VM is reset or restarted.

I'm also planning two-way communication (i.e. passing messages back to the
controlling process from JS) and a few other things.

## Building

Subversion, and Python 2.6-2.7 (needed by GYP) are required to build v8.

Build using make:

    make

GYP is not compatible with Python3. If `python3` is the default, you can
symlink `python2` to `~/bin` and set your path temporarily before compiling:

    ln -s /usr/bin/python2 ~/bin/python
    PATH=$HOME/bin:$PATH make

## Tests

You can run a few tests to verify basic functionality:

    make tests

## Usage

Start a VM:

    {ok, VM} = erlang_v8:start_vm().

Define a function:

    {ok, undefined} =
        erlang_v8:eval(VM, <<"function sum(a, b) { return a + b }">>).

Call the function: 

    {ok, 2} = erlang_v8:call(VM, <<"sum">>, [1, 1]).

You can reset the VM:

    ok = erlang_v8:reset_vm(VM).
    {error, <<"ReferenceError: sum is not defined">>} =
        erlang_v8:call(VM, <<"sum">>, [1, 1]).

Stop the VM:

    ok = erlang_v8:stop_vm(VM).

VMs can be initialized with code that is automatically reloaded when the VM is
reset or restarted:

    {ok, VM} = erlang_v8:start_vm([{source, <<"var x = 1;">>}]).

    {ok, 1} = erlang_v8:eval(VM, <<"x;">>).

    ok = erlang_v8:reset_vm(VM).
    {ok, 1} = erlang_v8:eval(VM, <<"x;">>).

    {ok, 2} = erlang_v8:eval(VM, <<"x = 2;">>).
    {ok, 2} = erlang_v8:eval(VM, <<"x;">>).
    ok = erlang_v8:reset_vm(VM).
    {ok, 1} = erlang_v8:eval(VM, <<"x;">>).

    ok = erlang_v8:stop_vm(VM).

You can also initialize the VMs using paths to source files:

    {ok, VM} = erlang_v8:start_vm([{file, "a.js"}, {file, "b.js"}]).

Set a custom timeout (defaults to `5000`):

    {error, timeout} =  erlang_v8:eval(VM, <<"while (true) {}">>, 500).

## Pooling

You might want to use some kind of pooling mechanism as the VMs are real OS
processes. I've had much success using
[devinus/poolboy](https://github.com/devinus/poolboy) for this purpose in the
past (I considered including the application, but decided against it as it
might not always be desirable to have a pool. Besides, poolboy is easy to set
up).

I'm also working on
[strange/erlang-v8-lib](https://github.com/strange/erlang-v8-lib), a little
framework that, among other things, implements a pool.

## TODO

- Use custom protocol to support more data types (binary, dates etc
- Refactor the API
- Experiment with calling Erlang from v8 synchronously
