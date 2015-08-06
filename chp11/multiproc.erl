-module(multiproc).
-compile(export_all).

sleep(T) ->
    receive
    after T ->
        ok
    end.

flush() ->
    receive
        _ -> flush()
    after 0 ->
        ok
    end.

% Demonstrates selective receive
%
% Note: An invocation of receive and can result in poor performance if there are many low 
% priority messages in the maibox before high priority messages. Low priority messages must
% first be taken from the mailbox until a match is found, and then put back in the mailbox
important() ->
    receive
        {Priority, Message} when Priority > 10 ->
            [Message|important()]
    after 0 ->
        normal()
    end.

normal() ->
    receive
        {_, Message} ->
            [Message|normal()]
    after 0 ->
        []
    end.

main() ->
    Pid = self(),
    Pid ! message1,
    Pid ! message2,
    Pid ! message3,
    Result = flush(),
    io:format("flush says ~s~n", [Result]),
    Pid ! {20, high},
    Pid ! {2, low},
    Pid ! {30, high},
    Pid ! {3, low},
    Prioritized = important(),
    io:format("Prioritized messages are ~s~n", [io_lib:write(Prioritized)]),
    io:format("Sleeping 2 seconds~n"),
    sleep(2000),
    io:format("Done!~n"),
    erlang:halt().
