-module(linkmon).
-compile(export_all).

myproc() ->
    timer:sleep(5000),
    exit(myproc_exit).

chain(0) ->
    receive
        _ -> ok
    after 2000 ->
        exit(chain_exit)
    end;

chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive
        Message ->
            io:format("Chained process ~s received: ~s~n", [N, io_lib:write(Message)])
    end.

main() ->
    link(spawn(fun myproc/0)),
    link(spawn(fun() -> chain(10) end)),
    receive
        Message1 ->
            io:format("received: ~s~n", [io_lib:write(Message1)])
    end,
    receive
        Message2 ->
            io:format("received: ~s~n", [io_lib:write(Message2)])
    end.

start_critic() ->
    spawn(?MODULE, critic, []).

start() ->
    Critic = start_critic(),
    Criticism = judge(Critic, "Led Zeppelin", "Led Zeppelin IV"),
    io:format("The criticism is: ~s~n", [Criticism]),
    Criticism2 = judge(Critic, "Pink Floyd", "Dark Side Of The Moon"),
    io:format("The criticism is: ~s~n", [Criticism2]),
    exit(Critic, heart_attack),
    Criticism3 = judge(Critic, "Led Zeppelin", "Led Zeppelin III"),
    io:format("The criticism is: ~s~n", [Criticism3]).
    
critic() ->
    receive
        {From, {"Led Zeppelin", _}} ->
            From ! {self(), "Right on!"};
        {From, {_, _}} ->
            From ! {self(), "Meh ..."}
    end,
    critic().

judge(Pid, Band, Album) ->
    Pid ! {self(), {Band, Album}},
    receive
        {Pid, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

% Slight improvements using a restarter process

start_critic2() ->
    spawn(?MODULE, restarter, []).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    % Make the Pid of the critic available through the atom 'critic'
    register(critic, Pid),
    io:format("Pid: ~s, critic: ~s~n", [io_lib:write(Pid), io_lib:write(whereis(critic))]),
    receive
        {'EXIT', Pid, normal} -> % Normal completion
            ok;
        {'EXIT', Pid, shutdown} -> % Critical was shutdown normally
            ok;
        {'EXIT', Pid, Reason} ->
            io:format("Restarting for reason: ~s~n", [io_lib:write(Reason)]),
            restarter() % Tail recursive call to restart process
    end.

judge2(Band, Album) ->
    % Sends message to process named 'critic'
    timer:sleep(500),
    critic ! {self(), {Band, Album}},
    Pid = whereis(critic),
    receive
        {Pid, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

start2() ->
    start_critic2(),
    timer:sleep(1000),
    Criticism = judge2("Led Zeppelin", "Led Zeppelin IV"),
    io:format("The criticism is: ~s~n", [Criticism]),
    exit(whereis(critic), indigestion),
    Criticism2 = judge2("Pink Floyd", "Dark Side Of The Moon"),
    io:format("The criticism is: ~s~n", [Criticism2]),
    exit(whereis(critic), heart_attack),
    Criticism3 = judge2("Led Zeppelin", "Led Zeppelin III"),
    io:format("The criticism is: ~s~n", [Criticism3]).

% Use references instead of matching on critic PID in case the critic dies

wait_for_critic() ->
    wait_for_critic(100).

wait_for_critic(N) when N > 0 ->
    monitor(process, critic),
    receive
        {'DOWN', _, process, _, noproc} ->
            timer:sleep(1),
            wait_for_critic(N - 1)
    after 10 ->
        ok
    end;
wait_for_critic(_) ->
    failure.

judge3(Band, Album) ->
    % Create a reference to match messages against
    Ref = make_ref(),
    wait_for_critic(),
    % Sends message to process named 'critic' using the reference
    critic ! {self(), Ref, {Band, Album}},
    receive
        {Ref, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

critic3() ->
    receive
        {From, Ref, {"Led Zeppelin", _}} ->
            From ! {Ref, "Right on!"};
        {From, Ref, {_, _}} ->
            From ! {Ref, "Meh ..."}
    end,
    critic3().

restarter3() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic3, []),
    % Make the Pid of the critic available through the atom 'critic'
    register(critic, Pid),
    io:format("Pid: ~s, critic: ~s~n", [io_lib:write(Pid), io_lib:write(whereis(critic))]),
    receive
        {'EXIT', Pid, normal} -> % Normal completion
            ok;
        {'EXIT', Pid, shutdown} -> % Critical was shutdown normally
            ok;
        {'EXIT', Pid, Reason} ->
            io:format("Restarting for reason: ~s~n", [io_lib:write(Reason)]),
            restarter3() % Tail recursive call to restart process
    end.

start_critic3() ->
    spawn(?MODULE, restarter3, []).

start3() ->
    start_critic3(),
    Criticism = judge3("Led Zeppelin", "Led Zeppelin IV"),
    io:format("The criticism is: ~s~n", [Criticism]),
    exit(whereis(critic), indigestion),
    Criticism2 = judge3("Pink Floyd", "Dark Side Of The Moon"),
    io:format("The criticism is: ~s~n", [Criticism2]),
    exit(whereis(critic), heart_attack),
    Criticism3 = judge3("Led Zeppelin", "Led Zeppelin III"),
    io:format("The criticism is: ~s~n", [Criticism3]).

% Demonstrating the difference between links and monitors

monitor_example() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, linking, []),
    receive
        Result ->
           io:format("Pid ~s exited normally with result ~s~n", [io_lib:write(Pid), io_lib:write(Result)])
    after 5000 ->
        io:format("Timed out after 5 seconds~n")
    end.

linking() ->
    spawn_link(?MODULE, monitoring, []),
    receive
        _ ->
            % Linked to a process - will not receive a message when it dies unless it's a system process
            io:format("Should never execute~n")
    end.

monitoring() ->
    Ref = monitor(process, spawn(fun() -> timer:sleep(1000), io:format("I, process ~s, died~n", [io_lib:write(self())]) end)),
    receive
        {'DOWN', Ref, process,  Pid, Reason} ->
            % monitored process has died
            io:format("Monitored process died, Ref=~s, Pid=~s, Reason=~s~n", [io_lib:write(Ref), io_lib:write(Pid),io_lib:write(Reason)]);
        _ ->
            io:format("Received some other message~n")
    after 2000 ->
        timeout
    end,
    exit(monkey_brains).
