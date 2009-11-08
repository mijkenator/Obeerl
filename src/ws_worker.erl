-module(ws_worker).
-author('mijkenator@gmail.com').

-export([start_link/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([do_job/1, get_urls/2, finding/3, trim_slash/1]).


-behaviour(gen_server).

-record(worker_state, {
        workername,
        wokrnumber,
        configitem}).

start_link(WorkerName) ->
    io:format("wsworker started ~p ~n", [WorkerName]),
    gen_server:start_link({local, WorkerName}, ?MODULE, #worker_state{workername = WorkerName}, []).
    
init(Args) ->
  io:format("ws work init callback launched ~p ~n", [Args]),
  gen_server:cast(ws_job, {getjob, self()}),
  {ok, Args}.
  
handle_cast({job, Url}, State) ->
    io:format("cast  !!!! ~p ~p ~n", [Url, State]),
    Ret = do_job(Url),
    io:format("ret of job: ~p~n", [Ret]),
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("cast unknown !!!! ~p ~p ~n", [Msg, State]),
    {noreply, State}. 
  
% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


do_job(Url) ->
    io:format("Do job -> ~p ~n", [Url]),
    case http:request(get, {Url, []}, [], []) of
        {ok, {Status, Headers, Body}} ->
            io:format("job ~p result status -> ~p  ~n", [Url, Status]),
            io:format("job ~p headers -> ~p bytes ~n",  [Url, Headers]),
            io:format("job ~p body length -> ~p bytes ~n", [Url, string:len(Body)]),
            {ok, get_urls(mochiweb_html:parse(Body), Url)};
        {error, Reason} ->
            io:format("job ~p failed -> ~p ~n", [Url, Reason]),
            {error, Reason}
    end.

get_urls(Tree, MainUrl) ->
    Complement = fun(Url) ->
        case regexp:matches(Url, "^(http|https:\/\/)") of
            {match, []} -> string:join([trim_slash(MainUrl), trim_slash(Url)], "/");
            _ -> Url
        end
    end,
    GrepHttp = fun(Url) ->
        case regexp:matches(Url, "^(http|https:\/\/)") of
            {match, _A}  -> true;
            {match, []} -> false;
            _           -> false
        end
    end,
    lists:filter(GrepHttp ,[Complement(M) || M <- finding(<<"a">>,<<"href">>, Tree)]).
    
finding(Pattern, Attribute, Tree) when is_binary(Attribute)->  
 GetAttr = fun(Found) ->  
    {Pattern, Attributes, _} = Found,  
    [{Attribute, FoundAttribute} | _] = lists:filter(fun(Attr) -> case Attr of {Attribute, _} -> true; _ -> false end end, Attributes),  
    binary_to_list(FoundAttribute)  
  end,  
  [GetAttr(M) || M <- finding(Pattern, [Tree], [])];  
  
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
  
trim_slash(Str) ->
    {_,LS,_} = regexp:sub(Str, "\/*$", ""),
    {_,RS,_} = regexp:sub(LS, "^\/*", ""),
    RS.