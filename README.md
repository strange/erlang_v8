# `erlang_v8`

Run JavaScript from Erlang in an external OS process.

This is an experiment to see if embedding v8 in an actual OS process is more
predictable than using a port driver or NIF. I will give the project proper
attention if the experiment works out.

The most notable features:

- You can `eval/3` things like "while (true) {}" with a timeout and have the
  v8 VM actually terminate when it times out.
- Multiple separate contexts per VM. This is useful when you want multiple
  parties to share the same VM(s).
- A VM can be initialized with pre-defined source that's loaded into the OS
  process and available in all contexts (CURRENTLY BROKEN).

I'm also planning two-way communication (i.e. passing messages back to the
controlling process from JS) and a few other things.

## Building

Subversion, pkg-config, libtinfo and Python 2.6-2.7 (needed by GYP) are
required to build v8.

Build using make:

    make

GYP is not compatible with Python3. If `python3` is the default, you can
symlink `python2` to `~/bin` and set your path temporarily before compiling:

    ln -s /usr/bin/python2 ~/bin/python
    PATH=$HOME/bin:$PATH make

## Tests

You can run a few tests to verify basic functionality:

    make test

## Usage

Start a VM:

    {ok, VM} = erlang_v8:start_vm().

Create a context:

    {ok, Context} = erlang_v8:create_context(VM).

Define a function:

    {ok, undefined} =
        erlang_v8:eval(VM, Context, <<"function sum(a, b) { return a + b }">>).

Call the function: 

    {ok, 2} = erlang_v8:call(VM, Context, <<"sum">>, [1, 1]).

Destroy the Context:

    erlang_v8:destroy_context(VM, Context).

Stop the VM:

    ok = erlang_v8:stop_vm(VM).

VMs can be initialized with code that is automatically available in all
contexts (CURRENTLY BROKEN):

    {ok, VM} = erlang_v8:start_vm([{source, <<"var x = 1;">>}]).
    {ok, Context1} = erlang_v8:create_context(VM).

    {ok, 1} = erlang_v8:eval(VM, Context1, <<"x;">>).
    {ok, 2} = erlang_v8:eval(VM, Context1, <<"x = 2;">>).
    {ok, 2} = erlang_v8:eval(VM, Context1, <<"x;">>).

    {ok, Context2} = erlang_v8:create_context(VM).
    {ok, 1} = erlang_v8:eval(VM, Context2, <<"x;">>).

    erlang_v8:destroy_context(VM, Context1).
    erlang_v8:destroy_context(VM, Context2).

    ok = erlang_v8:stop_vm(VM).

You can also initialize the VMs using paths to source files (CURRENTLY BROKEN):

    {ok, VM} = erlang_v8:start_vm([{file, "a.js"}, {file, "b.js"}]).

Set a custom timeout (defaults to `5000`):

    {error, timeout} =  erlang_v8:eval(VM, Context, <<"while (true) {}">>, 500).

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

## Updating V8

There are new tagged releases from the official V8 repo every four weeks;
however, not all are *stable*.  Keep reading...

However, DO NOT merely pull from their git repo.

First, find the latest *stable* tagged version
https://gist.github.com/domenic/aca7774a5d94156bfcc1

For Linux and Mac, look for the `stable` channel of each within the
[OmahaProxy](https://omahaproxy.appspot.com/) table.  (If your browser
window is too narrow, be sure to scroll *right* to expose the `v8_version`
column after locating the appropriate row.)

Ideally, that version string may be inserted into our
[Makefile](c_src/Makefile) as the value of `V8_VERSION`, and a local build
is successful.

### Building V8 The Hard Way

**Only if the preceding approach fails** consistently, a full build of V8
requires building from their sources directly, and confirm that the tagged
*stable* release actually builds and passes its own test.

Their instruction cover building on both
[Linux and Mac](https://v8.dev/docs/source-code).

Expanding upon the instructions linked above:

- They neglect mentioning that the initial fetch is time-consuming:
  + e.g., 50+ minutes via gigabit fiber
  + while that's downloading, also install
    [bazel](https://bazel.build/);
    e.g., as binary release of
    [bazelisk](https://github.com/bazelbuild/bazelisk/releases)
- Their `gclient` command apparently requires a subcommand:
  + `gclient sync`

once fetched, start with the latest *stable* tagged version from the
Gist instructions linked above, and iterate backwards.

Confirm that their tagged release actually builds and passes its own tests,
and assuming one of the flavors of [bazel](https://bazel.build/) has been
installed, build V8:

    bazel build v8
    bazel test v8

Once a build passes its own tests, a Makefile may be generated from Bazel,
in theory.  Then, splice it into ours.  See
[SO](https://stackoverflow.com/questions/58035752/building-makefile-using-bazel).

## TODO

- Use custom protocol to support more data types (binary, dates etc)
- Refactor the API
- Experiment with calling Erlang from v8 synchronously
- Fix or remove support for pre-defined code loading
- Investigate communicating with the port via networking rather than stdin/out
