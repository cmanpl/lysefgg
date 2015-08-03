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

dolphin2() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?";
        {From, fish} ->
            From ! "So long and thanks for the fish!";
        _ ->
            io:format("Heh, we're smarter than humans.~n")
    end.

dolphin3() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?",
            dolphin3();
        {From, fish} ->
            From ! "So long and thanks for all the fish!";
        _ ->
            io:format("Heh, we're smarter than humans.~n"),
            dolphin3()
    end.

print_message() ->
    receive
        Message ->
            io:format("Message was: ~s~n", [Message])
    end.

main() ->
    Dolphin = spawn(dolphins, dolphin1, []),
    Dolphin ! "Oh! Hello dolphin",
    Dolphin2 = spawn(dolphins, dolphin2, []),
    Dolphin2 ! {self(), fish},
    print_message(),
    Dolphin3 = spawn(dolphins, dolphin3, []),
    Dolphin3 ! {self(), do_a_flip},
    Dolphin3 ! {self(), do_a_flip},
    Dolphin3 ! {self(), unknown_command},
    print_message(),
    print_message(),
    Dolphin3 ! {self(), fish},
    print_message(),
    erlang:halt().
