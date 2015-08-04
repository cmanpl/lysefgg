-module(kitchen).
-compile(export_all).

printmsg() ->
    receive
        {_, {ok, Food}} ->
            io:format("Food was was: ~s~n", [Food]);
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

fridge2(FoodList) ->
    receive
        {From, {store, Food}} ->
            From ! {self(), ok},
            fridge2([Food|FoodList]);
        {From, {take, Food}} ->
            case lists:member(Food, FoodList) of
                true ->
                    From ! {self(), {ok, Food}},
                    fridge2(lists:delete(Food, FoodList));
                false ->
                    From ! {self(), not_found},
                    fridge2(FoodList)
            end;
        terminate ->
            ok
    end.

fridge2() ->
    fridge2([]).

main() ->
    Fridge1 = spawn(kitchen, fridge1, []),
    Fridge1 ! {self(), {store, cake}},
    printmsg(),
    Fridge1 ! {self(), {take, cake}},
    printmsg(),
    Fridge2 = spawn(kitchen, fridge2, []),
    Fridge2 ! {self(), {take, cake}},
    printmsg(),
    Fridge2 ! {self(), {store, cake}},
    printmsg(),
    Fridge2 ! {self(), {store, cake}},
    printmsg(),
    Fridge2 ! {self(), {store, cake}},
    printmsg(),
    Fridge2 ! {self(), {take, cake}},
    printmsg(),
    Fridge2 ! {self(), {take, cake}},
    printmsg(),
    Fridge2 ! {self(), {take, cake}},
    printmsg(),
    Fridge2 ! {self(), {take, cake}},
    printmsg(),
    erlang:halt().
