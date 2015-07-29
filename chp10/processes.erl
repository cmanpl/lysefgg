-module(processes).
-compile(export_all).

many_processes() ->
    % demonstrates that the processes will exit in a psuedo random order based on scheduler
    G = fun(X) -> timer:sleep(100), io:format("~p~n", [X]) end,
    [spawn(fun() -> G(X) end) || X <- lists:seq(1, 100)].

main() ->
    F = fun() -> io:format("In a function!~n") end,
    spawn(F),
    io:format("Hello world!~n"),
    many_processes(),
    timer:sleep(5000),
    erlang:halt(0).
