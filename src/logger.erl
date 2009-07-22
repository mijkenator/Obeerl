%%%-------------------------------------------------------------------
%%% File    : logger.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : logger server
%%%-------------------------------------------------------------------
-module(logger).

-behaviour(gen_server).

%% API
-export([start_link/1, log/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,
    {
        cfgpid,
        loglevel,
        filehandle,
        requests = 0
    }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(CfgPid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CfgPid], []).


log({LogLevel, LogMessage}) ->
    gen_server:call(?MODULE, {LogLevel, LogMessage}).
    
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([CfgPid]) ->
    {ok, #state{ cfgpid     = CfgPid,
                 filehandle = logger_funcs:get_logger(CfgPid),
                 loglevel   = logger_funcs:get_loglevel(CfgPid)}};
init([]) ->
    {stop, "Pid of cfg server is required"}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({MessageLevel, Message}, _From, #state{
        cfgpid=CfgPid, filehandle=FileHandler,
        loglevel=LogLevel, requests=ReqCount} = _State) when
            MessageLevel == debug;
            MessageLevel == warn;
            MessageLevel == error
        ->
    CurrentLogLevel = case MessageLevel of
        debug   -> 'DEBUG';
        warn    -> 'WARNING';
        error   -> 'ERROR'
    end,
    logger_funcs:logprint(FileHandler, LogLevel, CurrentLogLevel, Message),
    {reply, ok, #state{
        cfgpid=CfgPid, filehandle=FileHandler,
        loglevel=LogLevel, requests=ReqCount+1
    }};
handle_call({change_log_level, NewLogLevel}, _From, #state{
        cfgpid=CfgPid, filehandle=FileHandler,
        loglevel=_LogLevel, requests=ReqCount} = _State) when
            NewLogLevel == "DEBUG";
            NewLogLevel == "ERROR";
            NewLogLevel == "WARNING"
        ->
    {reply, ok, #state{
        cfgpid=CfgPid, filehandle=FileHandler,
        loglevel=NewLogLevel, requests=ReqCount+1
    }};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
