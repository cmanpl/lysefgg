-module(linkmon).
-compile(export_all).

myproc() ->
    timer:sleep(5000),
    exit(reason).

main() ->
    link(spawn(fun myproc/0)),
    receive
        Message ->
            io:format("received: ~s~n", [io_lib:write(Message)])
    end,
    erlang:halt().
