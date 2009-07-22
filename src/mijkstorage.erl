-module(mijkstorage).

-export([init_ets/2 ]).

init_ets(_, [])    -> void;
init_ets(TableId, [H|T]) ->
    case H of
        {Name, Value} ->
            %io:format("~-15w name: ~p value: ~p ~n", ['DEBUG', Name, Value]),
            ets:insert(TableId, {Name, Value});
        _ ->
            io:format("~-15w ~p ~n", ['ERROR', H]),
            void
    end,
    init_ets(TableId, T).