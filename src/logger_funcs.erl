-module(logger_funcs).

-export([get_loglevel/1, get_logger/1, logprint/4]).

get_loglevel(CfgPid) ->
    case gen_server:call(CfgPid, {search, loglevel}) of
        [{loglevel, Response}|_] ->
            case Response of
                Response when
                            Response == "DEBUG";
                            Response == "ERROR";
                            Response == "WARNING" -> Response
            end;
        _ -> "DEBUG"
    end.
    
get_logger(CfgPid) ->
    Logname = case gen_server:call(CfgPid, {search, logfilepath}) of
        [{logfilepath, Lfn}|_] -> Lfn;
        LopenError -> exit({error, "Log file name is required in configuration file", LopenError})
    end,
    case gen_server:call(CfgPid, {search, logopentype}) of
        [{logopentype, Response}|_] when
                Response == append;
                Response == write
            ->
            case file:open(Logname, [Response]) of
                {ok, S} -> S;
                Error   -> exit({error, Logname, Response ,Error})
            end;
        _ -> exit({error, Logname, "Log open type error"})
    end.

logprint(S, LogLevel, Type, Message) ->
    PrintFlag = case LogLevel of
        "ERROR" when Type == 'ERROR'  -> true;
        "DEBUG"                       -> true;
        "WARNING" when
            Type == 'ERROR';
            Type == 'WARNING'         -> true;
        _                             -> false
    end,
    case PrintFlag of
        true  ->
            {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_datetime(erlang:now()),
            io:format(S, "~-15w ~2B/~2B/~4B ~2B:~2.10.0B:~2.10.0B  ~p ~n",
            [Type, Month, Day, Year, Hour, Min, Sec, Message]);
        false -> void
    end.