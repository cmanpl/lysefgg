-module(linkmon).
-compile(export_all).

myproc() ->
    timer:sleep(5000),
    exit(myproc_exit).

chain(0) ->
    receive
        _ -> ok
    after 2000 ->
        exit(chain_exit)
    end;

chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive
        Message ->
            io:format("Chained process ~s received: ~s~n", [N, io_lib:write(Message)])
    end.

main() ->
    link(spawn(fun myproc/0)),
    link(spawn(fun() -> chain(10) end)),
    receive
        Message1 ->
            io:format("received: ~s~n", [io_lib:write(Message1)])
    end,
    receive
        Message2 ->
            io:format("received: ~s~n", [io_lib:write(Message2)])
    end.

start_critic() ->
    spawn(?MODULE, critic, []).

critic() ->
    receive
        {From, {"Led Zeppelin", _}} ->
            From ! {self(), "Right on!"};
        {From, {_, _}} ->
            From ! {self(), "Meh ..."}
    end,
    critic().

judge(Pid, Band, Album) ->
    Pid ! {self(), {Band, Album}},
    receive
        {Pid, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

start() ->
    Critic = start_critic(),
    Criticism = judge(Critic, "Led Zeppelin", "Led Zeppelin IV"),
    io:format("The criticism is: ~s~n", [Criticism]),
    Criticism2 = judge(Critic, "Pink Floyd", "Dark Side Of The Moon"),
    io:format("The criticism is: ~s~n", [Criticism2]),
    exit(Critic, heart_attack),
    Criticism3 = judge(Critic, "Led Zeppelin", "Led Zeppelin III"),
    io:format("The criticism is: ~s~n", [Criticism3]).
    
