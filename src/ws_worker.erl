-module(ws_worker).
-author('mijkenator@gmail.com').

-export([start_link/1]).
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([do_job/2, trim_slash/1, get_urls/3]).

%% FSM States
-export([
    'WAIT_FOR_JOB'/2,
    'WAIT_FOR_WORK'/2
]).

-export([get_job/0, save_url/1]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jobrec.hrl").

%-behaviour(gen_server).
-behaviour(gen_fsm).

-record(worker_state, {
        workername,
        wokrnumber,
        configitem,
        url,
        counter=0}).

start_link(WorkerName) ->
    io:format("wsworker started ~p ~n", [WorkerName]),
    Result = gen_fsm:start_link({local, WorkerName},?MODULE, #worker_state{workername = WorkerName, counter=0}, []),
    io:format("wsworker started Result ~p ~n", [Result]),
    Result.
    
init(Args) ->
  io:format("ws work init callback launched ~p ~n", [Args]),
  {ok, 'WAIT_FOR_JOB', Args, 10}.
  
'WAIT_FOR_JOB'(_Other, State) ->
    NewUrl = get_job(),
    io:format("'WAIT_FOR_JOB' ~p ~n", [NewUrl]),
    gen_server:cast(ws_job, {ping, self(), NewUrl}),
    {next_state, 'WAIT_FOR_WORK', State#worker_state{url=NewUrl}, 10}.

'WAIT_FOR_WORK'(_Data, #worker_state{workername = WorkerName, counter=Counter, url=U} = State) ->
    Url = binary_to_list(U),
    { memory, M } = erlang:process_info (self (), memory),
    io:format("MEMORY0:~p ~p~n", [WorkerName, M]),
    case do_job(Url, State) of
        {ok, _}       ->
            { memory, M1 } = erlang:process_info (self (), memory),
            io:format("MEMORY1:~p ~p~n", [WorkerName, M1]),
            mnesia:transaction(fun() ->  mnesia:write(#jobrec{url=list_to_binary(Url), state=done}) end),
            { memory, M2 } = erlang:process_info (self (), memory),
            io:format("MEMORY2:~p ~p~n", [WorkerName, M2]),
            { memory, M3 } = erlang:process_info (self (), memory),
            io:format("MEMORY3:~p ~p~n", [WorkerName, M3]);
        {error, Reason} ->
            mnesia:transaction(fun() ->  mnesia:write(#jobrec{url=list_to_binary(Url), state=fail}) end),
            io:format("job error: ~p~n",  [Reason])
    end,
    case Counter of
        Counter when Counter < 50 ->
            {next_state, 'WAIT_FOR_JOB', State#worker_state{workername = WorkerName, counter=Counter+1}, 10};
        _ ->
            {stop, normal, State}
            %{next_state, 'WAIT_FOR_JOB', State#worker_state{workername = WorkerName, counter=Counter+1}, 10}
    end.
  
% These are just here to suppress warnings.

handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
handle_info(Info, StateName, StateData) ->
    io:format("nexpected message ~p ~p ~p ~n", [Info, StateName, StateData]),
    {stop, normal, StateData}.
terminate(_Reason, _StateName, _State) ->
    io:format("Terminate ~n"),
    ok.


do_job(Url, State) ->
    io:format("Do job -> ~p ~p ~n", [State, Url]),
    case http:request(get, {Url, []}, [{timeout, 20000}, {autoredirect, true}], [{body_format, binary}]) of
        {ok, {Status, _Headers, Body}} ->
            io:format("job ~p result status -> ~p  ~n", [Url, Status]),
            %o:format("job ~p headers -> ~p bytes ~n",  [Url, Headers]),
            %io:format("job ~p body length -> ~p bytes ~n", [Url, string:len(Body)]),
            io:format("job ~p body length -> ~p bytes ~n", [Url, size(Body)]),
            case get_urls(Body, Url, mochi) of
            %case get_urls(mochiweb_html:parse(Body), Url) of
                {ok, M} -> {ok, M};
                {error, M} -> {error, M}
            end;
        {error, Reason} ->
            io:format("job ~p failed -> ~p ~n", [Url, Reason]),
            {error, Reason}
    end.
    
get_urls(Html, MainUrl, Type) when Type =:= mochi ->
    try mochiweb_html:parse(Html) of
        Tree -> finding(<<"a">>,<<"href">>, Tree, MainUrl), {ok, done}
    catch
        _:_  -> {error, parser_error}
    end;
get_urls(HtmlBinary, MainUrl, Type) when Type =:= regex ->
    Html = binary_to_list(HtmlBinary),
    Complement = fun(Url) ->
        case regexp:matches(Url, "^(http|https:\/\/)") of
            {match, []} -> string:join([trim_slash(MainUrl), trim_slash(Url)], "/");
            _ -> Url
        end
    end,
    GrepHttp = fun(Url) ->
        case regexp:matches(Url, "^(http|https:\/\/)") of
            {match, []} -> false;
            {match, _A} -> case regexp:matches(Url, "\.(pdf|mp3|doc|tar|rar|zip|tgz|tar.gz|js|css|tar.bz2|bz2)$") of
                                {match, []} -> true;
                                {match, _A} -> false;
                                _           -> true
                           end;
            _           -> false
        end
    end,
    ProcessUrl = fun([{Start, Length}]) ->
        Url = string:substr(Html, Start+1, Length),
        U1 = Complement(Url),
        case GrepHttp(U1) of
            true -> save_url(list_to_binary(U1))
        end   
    end,
    Reg = "<a\s+\.*?href=['\"]*([^'\"\s]+)['\"]*\s*\.*>",
    case re:run(Html, Reg, [global, {capture,[1]}, {newline,any}]) of
        {match, A} when is_list(A) -> lists:map(fun(Url) -> ProcessUrl(Url) end, A), {ok, done};
        _   -> {error, get_url_error}
    end.

  
trim_slash(Str) ->
    {_,LS,_} = regexp:sub(Str, "\/*$", ""),
    {_,RS,_} = regexp:sub(LS, "^\/*", ""),
    RS.

save_url(Url) ->
    case check_not_exists(Url) of
        true ->     Row = #jobrec{url=Url, state=new},
                    F = fun() -> mnesia:write(Row) end,
                    mnesia:transaction(F), true;
        _    ->     false
    end.


check_not_exists(Url) ->
    Ans = mnesia:transaction(fun() -> mnesia:select(jobrec, [{#jobrec{state='$1', url=Url}, [], ['$1']}], 1, read) end ),
    case Ans of
        {atomic, {[], _}}-> true;
        {atomic, {_, _}} -> false;
        _                -> true
    end.
    
get_job() ->
    Ans = mnesia:transaction(fun() -> mnesia:select(jobrec, [{#jobrec{state=new, url='$1'}, [], ['$1']}], 1, read) end ),
    case Ans of
        {atomic, {[Url|_], _}} ->
            mnesia:transaction(fun() ->  mnesia:write(#jobrec{url=Url, state=processing}) end),Url;
        _                      ->
            io:format("GET JOB failed -> ~p ~n", [Ans]),
            get_job()
    end.

%------------------------------------------------------

finding(Pattern, Attribute, Tree, MainUrl) when is_binary(Attribute)->
    Complement = fun(Url) ->
        case regexp:matches(Url, "^(http|https:\/\/)") of
            {match, []} -> string:join([trim_slash(MainUrl), trim_slash(Url)], "/");
            _ -> Url
        end
    end,
    GrepHttp = fun(Url) ->
        case regexp:matches(Url, "^(http|https:\/\/)") of
            {match, []} -> false;
            {match, _A} -> case regexp:matches(Url, "\.(pdf|mp3|doc|tar|rar|zip|tgz|tar.gz|js|css|tar.bz2|bz2)$") of
                                {match, []} -> true;
                                {match, _A} -> false;
                                _           -> true
                           end;
            _           -> false
        end
    end,
    ProcessUrl = fun(Url) ->
        U1 = Complement(Url),
        case GrepHttp(U1) of
            true -> save_url(list_to_binary(U1))
        end   
    end,
    GetAttr = fun(Found) ->
        {Pattern, Attributes, _} = Found,
        M = lists:filter(fun(Attr) -> case Attr of {Attribute, _} -> true; _ -> false end end, Attributes),
        case M of
            [{Attribute, FoundAttribute} | _] -> ProcessUrl(binary_to_list(FoundAttribute)), {ok};
            _ -> {error}
        end
    end,  
    lists:map(fun(X) -> GetAttr(X) end, finding(Pattern, [Tree], [])).
  
finding(_, [], Collected) ->  
  Collected;  
  
finding(Pattern, [Next | Siblings], Collected) ->  
    case Next of  
      {Element, _, Children} ->  
      case Element of  
        Pattern ->  
          finding(Pattern, Siblings ++ Children, Collected ++ [Next]);  
        _ ->  
          finding(Pattern, Siblings ++ Children, Collected)  
      end;  
    _ ->  
      finding(Pattern, Siblings, Collected)  
    end.
