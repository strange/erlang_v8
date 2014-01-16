# erlang_v8

Run JavaScript code in an external OS process.

This is just an experiment to see if embedding v8 in a OS process is more
predictable than using a port driver or NIF. I will give the project proper
attention if the experiment works out.

The most notable feature is that you can eval things like "while (true) {}"
and have the v8 VM actually terminate when it times out. I'm also planning
two-way communication, resetting context etc.

## Building

Git and Python 2 (for GYP) are required to build v8. Build scripts are
currently hardcoded to ia32 and the app might require a little fiddling to
build properly on other systems:

    rebar get-deps
    rebar compile

## Examples

Start a VM:

    {ok, VM} = erlang_v8:start_vm().

Define a function:

    {ok, undefined} = erlang_v8:eval(VM, <<"function sum(a, b) { return a + b }">>).

Run the function: 
    {ok, 2} = erlang_v8:call(VM, <<"sum">>, [1, 1]).

Stop the VM:

    ok = erlang_v8:stop_vm(VM).

## TODO

Lots of testing, improve the communication protocol, clean up api, add
features like reset context and stuff, experiment with calling Erlang from JS,
load initial context from args, supervisor strategy, experiment with different
ways of passing args to calls (maybe via the communication protocol) etc.
