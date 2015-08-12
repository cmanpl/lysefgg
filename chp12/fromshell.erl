-module(fromshell).
-compile(export_all).

fun1() ->
    io:format("fun1 pid is  ~s~n", [io_lib:write(self())]),
    spawn_link(fun() -> exit(fail_from_fun1) end),
    receive
        _ -> ok
    end,
    io:format("fun1 never runs~n", []).

fun2() ->
    io:format("fun2 pid is  ~s~n", [io_lib:write(self())]),
    spawn_link(fun fun1/0),
    receive
        Message -> 
            io:format("fun2 received  ~s~n", [io_lib:write(Message)])
        after 1000 ->
            timeout
    end.

fun3() ->
    io:format("fun3 pid is  ~s~n", [io_lib:write(self())]),
    spawn_link(fun fun2/0),
    receive
        Message -> 
            io:format("fun3 received  ~s~n", [io_lib:write(Message)])
        after 1000 ->
            timeout
    end.
