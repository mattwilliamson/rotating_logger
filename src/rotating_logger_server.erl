%%-------------------------------------------------------------------
%% @author Kelly McLaughlin <mclaughlin77@gmail.com>
%% @doc The gen_server callback module for the rotating_logger application.
%% @end
%%-------------------------------------------------------------------
-module(rotating_logger_server).

-behaviour(gen_server).

%% API
-export([start_link/1, shutdown/0, error/2, error/1, warn/2, warn/1, info/2, info/1, console/2, console/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

shutdown() -> 
    application:stop(rotating_logger),
    init:stop().

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initiates the server
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({Name, Dir, MaxFiles, MaxBytes}) ->
    rotating_logger:add_handler([{name, Name},{dir, Dir}, {max_files, MaxFiles}, {max_bytes, MaxBytes}]),
    {ok, ready}.

%%--------------------------------------------------------------------
%% @doc Handling call messages
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({info, Fmt}, _From, State) ->
    Reply = rotating_logger:info(Fmt),
    {reply, Reply, State};
handle_call({info, Fmt, Args}, _From, State) ->
    Reply = rotating_logger:info(Fmt, Args),
    {reply, Reply, State};
handle_call({warn, Fmt}, _From, State) ->
    Reply = rotating_logger:warn(Fmt),
    {reply, Reply, State};
handle_call({warn, Fmt, Args}, _From, State) ->
    Reply = rotating_logger:warn(Fmt, Args),
    {reply, Reply, State};
handle_call({error, Fmt}, _From, State) ->
    Reply = rotating_logger:error(Fmt),
    {reply, Reply, State};
handle_call({error, Fmt, Args}, _From, State) ->
    Reply = rotating_logger:error(Fmt, Args),
    {reply, Reply, State};
handle_call({console, Fmt}, _From, State) ->
    Reply = rotating_logger:console(Fmt),
    {reply, Reply, State};
handle_call({console, Fmt, Args}, _From, State) ->
    Reply = rotating_logger:console(Fmt, Args),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @doc Handling cast messages
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handling all non call/cast messages
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Convert process state when code is changed
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Exported functions. 
%%--------------------------------------------------------------------
info(Format) ->
    gen_server:call({global, ?MODULE}, {info, Format}, infinity).
info(Format, Args) ->
    gen_server:call({global, ?MODULE}, {info, Format, Args}, infinity).
warn(Format) ->
    gen_server:call({global, ?MODULE}, {warn, Format}, infinity).
warn(Format, Args) ->
    gen_server:call({global, ?MODULE}, {warn, Format, Args}, infinity).
error(Format) ->
    gen_server:call({global, ?MODULE}, {error, Format}, infinity).
error(Format, Args) ->
    gen_server:call({global, ?MODULE}, {error, Format, Args}, infinity).
console(Format) ->
    gen_server:call({global, ?MODULE}, {console, Format}, infinity).
console(Format, Args) ->
    gen_server:call({global, ?MODULE}, {console, Format, Args}, infinity).

