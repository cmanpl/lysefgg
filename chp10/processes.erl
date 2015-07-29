-module(processes).
-compile(export_all).

main() ->
    F = fun() -> io:format("In a function!~n") end,
    spawn(F),
    io:format("Hello world!~n"),
    erlang:halt(0).
