-module(ws_worker).
-author('mijkenator@gmail.com').

-export([start_link/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([do_job/2, trim_slash/1, get_urls2/2]).

-export([get_job/0, save_job/1, job_done/1]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jobrec.hrl").

-behaviour(gen_server).

-record(worker_state, {
        workername,
        wokrnumber,
        configitem}).

start_link(WorkerName) ->
    io:format("wsworker started ~p ~n", [WorkerName]),
    Result = gen_server:start_link({local, WorkerName}, ?MODULE, #worker_state{workername = WorkerName}, []),
    io:format("wsworker started Result ~p ~n", [Result]),
    Result.
    
init(Args) ->
  io:format("ws work init callback launched ~p ~n", [Args]),
  gen_server:cast(ws_job, {getjob, self()}),
  {ok, Args}.
  
handle_cast({job, Url}, State) ->
    %io:format("cast  !!!! ~p ~p ~n", [Url, State]),
    case do_job(Url, State) of
        {ok, Ret}       ->
            %io:format("ret of job: ~p~n", [Ret]),
            %gen_server:cast(ws_job, {jobdone, self(), Url}),
            job_done(Url),
            %gen_server:cast(ws_job, {savejob, self(), Ret}),
            save_job(Ret),
            %gen_server:cast(ws_job, {getjob, self()});
            NewUrl = get_job(),
            gen_server:cast(ws_job, {ping, self(), NewUrl});
        {error, Reason} ->
            gen_server:cast(ws_job, {jobfail, self(), Url}),
            io:format("job error: ~p~n",  [Reason]),
            gen_server:cast(ws_job, {getjob, self()})
    end,
    {noreply, State};
    %{stop, "work is done", State};
handle_cast(Msg, State) ->
    io:format("cast unknown !!!! ~p ~p ~n", [Msg, State]),
    {noreply, State}. 
  
% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


do_job(Url, State) ->
    io:format("Do job -> ~p ~p ~n", [State, Url]),
    case http:request(get, {Url, []}, [{timeout, 10000}, {autoredirect, true}], []) of
        {ok, {Status, _Headers, Body}} ->
            io:format("job ~p result status -> ~p  ~n", [Url, Status]),
            %o:format("job ~p headers -> ~p bytes ~n",  [Url, Headers]),
            io:format("job ~p body length -> ~p bytes ~n", [Url, string:len(Body)]),
            case get_urls2(Body, Url) of
            %case get_urls(mochiweb_html:parse(Body), Url) of
                {ok, M} -> {ok, M};
                {error, M} -> {error, M}
            end;
        {error, Reason} ->
            io:format("job ~p failed -> ~p ~n", [Url, Reason]),
            {error, Reason}
    end.

get_urls2(Html, MainUrl) ->
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
    Reg = "<a\s+\.*?href=['\"]*([^'\"\s]+)['\"]*\s*\.*>",
    U = case re:run(Html, Reg, [global, {capture,[1]}, {newline,any}]) of
        {match, A} when is_list(A) -> [ string:substr(Html, Start+1, Length) || [{Start, Length}] <- A];
        _   -> []
    end,
    try lists:filter(GrepHttp ,[Complement(M) || M <- U]) of
        M -> {ok, M}
    catch
        _ : Error -> {error, Error}
    end.
    
%get_urls(Tree, MainUrl) ->
%    Complement = fun(Url) ->
%        case regexp:matches(Url, "^(http|https:\/\/)") of
%            {match, []} -> string:join([trim_slash(MainUrl), trim_slash(Url)], "/");
%            _ -> Url
%        end
%    end,
%    GrepHttp = fun(Url) ->
%        case regexp:matches(Url, "^(http|https:\/\/)") of
%            {match, []} -> false;
%            {match, _A} -> case regexp:matches(Url, "\.(pdf|mp3|doc|tar|rar|zip|tgz|tar.gz)$") of
%                                {match, []} -> true;
%                                {match, _A} -> false;
%                                _           -> true
%                           end;
%            _           -> false
%        end
%    end,
%    try lists:filter(GrepHttp ,[Complement(M) || M <- finding(<<"a">>,<<"href">>, Tree)]) of
%        M -> {ok, M}
%    catch
%        _ : Error -> {error, Error}
%    end.
%    
%finding(Pattern, Attribute, Tree) when is_binary(Attribute)->  
% GetAttr = fun(Found) ->  
%    {Pattern, Attributes, _} = Found,  
%    [{Attribute, FoundAttribute} | _] = lists:filter(fun(Attr) -> case Attr of {Attribute, _} -> true; _ -> false end end, Attributes),  
%    binary_to_list(FoundAttribute)  
%  end,  
%  [GetAttr(M) || M <- finding(Pattern, [Tree], [])];  
%  
%finding(_, [], Collected) ->  
%  Collected;  
%  
%finding(Pattern, [Next | Siblings], Collected) ->  
%  case Next of  
%    {Element, _, Children} ->  
%    case Element of  
%      Pattern ->  
%        finding(Pattern, Siblings ++ Children, Collected ++ [Next]);  
%      _ ->  
%        finding(Pattern, Siblings ++ Children, Collected)  
%    end;  
%  _ ->  
%    finding(Pattern, Siblings, Collected)  
%  end.
  
trim_slash(Str) ->
    {_,LS,_} = regexp:sub(Str, "\/*$", ""),
    {_,RS,_} = regexp:sub(LS, "^\/*", ""),
    RS.
    
job_done(Url) ->
    io:format("job ~p done . ~n", [Url]),
    mnesia:transaction(fun() ->  mnesia:write(#jobrec{url=Url, state=done}) end).

save_job(Results) ->
    lists:foreach(fun(Url) ->
            case check_not_exists(Url) of
                true ->
                    Row = #jobrec{url=Url, state=new},
                    F = fun() -> mnesia:write(Row) end,
                    mnesia:transaction(F);
                _    -> false
            end
        end, Results),
    io:format("NOW IN Mnesia new : ~p ~n", [length(do(qlc:q([X || X <- mnesia:table(jobrec), X#jobrec.state == new])))]),
    io:format("NOW IN Mnesia done : ~p ~n", [length(do(qlc:q([X || X <- mnesia:table(jobrec), X#jobrec.state == done])))]).

check_not_exists(Url) ->
    Ans = mnesia:transaction(fun() -> mnesia:select(jobrec, [{#jobrec{state='$1', url=Url}, [], ['$1']}], 1, read) end ),
    case Ans of
        {atomic, {[], _}}-> true;
        {atomic, {_, _}} -> false;
        _                -> true
    end.

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
    
get_job() ->
    Ans = mnesia:transaction(fun() -> mnesia:select(jobrec, [{#jobrec{state=new, url='$1'}, [], ['$1']}], 1, read) end ),
    case Ans of
        {atomic, {[Url|_], _}} ->
            mnesia:transaction(fun() ->  mnesia:write(#jobrec{url=Url, state=processing}) end),Url;
        _                      ->
            io:format("GET JOB failed -> ~p ~n", [Ans]),
            get_job()
    end.
