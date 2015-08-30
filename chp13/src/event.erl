-module(event).
-compile(export_all).
-record(
    state, {
        server,
        name="",
        to_go=0 }
).

normalize(Time) ->
    Limit = 49 * 24 * 60 * 60,
    [Time rem Limit | lists:duplicate(Time div Limit, Limit)].

loop(S = #state{server=Server, to_go=[T|Next]}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after T * 1000 ->
        if Next =:= [] ->
            Server ! {done, S#state.name};
        true ->
            loop(S#state{to_go=Next})
        end
    end;
loop(S = #state{to_go=T}) ->
    loop(S#state{to_go=normalize(T)}).
