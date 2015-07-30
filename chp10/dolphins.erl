-module(dolphins).
-compile(export_all).

dolphin1() ->
    receive
        do_a_flip ->
            io:format("How about no?~n");
        fish ->
            io:format("So long and thanks for the fish!~n");
        _ ->
            io:format("Heh, we're smarter than humans.~n")
    end.

main() ->
    Dolphin = spawn(dolphins, dolphin1, []),
    Dolphin ! "Oh! Hello dolphin",
    timer:sleep(1000),
    erlang:halt().
