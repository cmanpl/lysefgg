-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

sub(X, Y) -> X() - Y().

increment([]) -> [];
increment([H|T]) -> [H + 1|increment(T)].

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].

main() ->
    Result = add(fun one/0, fun two/0),
    io:format("Result is ~B~n", [Result]), % using io format with an int
    io:format("Result is ~s~n", [io_lib:write(Result)]), % using io_lib:write to convert to a string
    Result2 = sub(fun one/0, fun two/0),
    io:format("Result is ~s~n", [io_lib:write(Result2)]),
    Result3 = increment([1,2,3,4]),
    io:format("Result is ~s~n", [io_lib:write(Result3)]),
    Result4 = map(fun(2) -> -1; (X) -> X * X end, [1,2,3,4]), %s creating an anonymous function with multiple function heads
    io:format("Result is ~s~n", [io_lib:write(Result4)]),
    erlang:halt(0).
