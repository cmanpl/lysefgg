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
