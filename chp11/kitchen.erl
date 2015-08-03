-module(kitchen).
-compile(export_all).

printmsg() ->
    receive
        {_, Message} ->
            io:format("Message was: ~s~n", [Message])
    end.

fridge1() ->
    receive
        {From, {store, _Food}} ->
            From ! {self(), ok},
            fridge1();
        {From, {take, _Food}} ->
            From ! {self(), not_found},
            fridge1();
        terminate ->
            ok
    end.

main() ->
    Fridge1 = spawn(kitchen, fridge1, []),
    Fridge1 ! {self(), {store, cake}},
    printmsg(),
    Fridge1 ! {self(), {take, cake}},
    printmsg(),
    erlang:halt().
