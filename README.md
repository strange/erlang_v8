# erlang_v8

Run JavaScript from Erlang in an external OS process.

This is just an experiment to see if embedding v8 in an actual OS process is
more predictable than using a port driver or NIF. I will give the project
proper attention if the experiment works out.

The most notable feature is that you can eval things like "while (true) {}"
and have the v8 VM actually terminate when it times out. I'm also planning
two-way communication, loading scripts etc.

## Building

Subversion, and Python 2.6-2.7 (by GYP) are required to build v8.

Build using make:

    make

Or with rebar:

    rebar get-deps
    rebar compile

## Tests

You can run a few tests to verify basic functionality:

    make tests

## Usage

Start a VM:

    {ok, VM} = erlang_v8:start_vm().

Define a function:

    {ok, undefined} =
        erlang_v8:eval(VM, <<"function sum(a, b) { return a + b }">>).

Run the function: 

    {ok, 2} = erlang_v8:call(VM, <<"sum">>, [1, 1]).

You can reset the VM:

    ok = erlang_v8:reset_vm(VM).
    {error, <<"ReferenceError: sum is not defined">>} =
        erlang_v8:call(VM, <<"sum">>, [1, 1]).

Stop the VM:

    ok = erlang_v8:stop_vm(VM).

## TODO

Lots of testing, improve the communication protocol, clean up api, experiment
with calling Erlang from JS, load initial context from args, supervisor
strategy, experiment with different ways of passing args to calls (maybe via
the communication protocol) etc.
