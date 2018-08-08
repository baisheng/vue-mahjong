
-module(ws_handler).

-define(CHECK_INTERVAL,   1000).

-export([
         init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3
        ]).

%%--------------------------------------------------------------------
init(Req, Opts) ->
    io:format("gate init ~p~n", [{self()}]),
	{cowboy_websocket, Req, Opts}.

%%--------------------------------------------------------------------
websocket_init(State) ->
    io:format("gate websocket init ~p~n", [{self()}]),
    lib_gate:timer(?CHECK_INTERVAL, self(), {'check_valid'}),
	{ok, State}.

%%--------------------------------------------------------------------
websocket_handle({binary, BinData}, State) ->
    socket_callback:on_socket_receive(self(), BinData, 'ClientSocket', 'ClientIp', 'PlayerPid', <<>>),
    {ok, State};
websocket_handle({text, Msg}, State) ->
	logger:warning_msg("~p websocket handle text: ~p~n", [?MODULE, Msg]),
    {ok, State};
websocket_handle(Data, State) ->
    logger:warning_msg("~p websocket handle WARNING! ~p~n", [?MODULE, Data]),
    {ok, State}.

%%--------------------------------------------------------------------
websocket_info({timeout, Ref, Msg}, State) ->
	logger:warning_msg("~p websocket info timeout WARNING! ~p~n", [?MODULE, {Ref, Msg}]),
    % {reply, {text, Msg}, State};
    {ok, State};
websocket_info({'ws_cast', Info}, State) ->
    do_cast(Info, State);
websocket_info({'ws_call', Info}, State) ->
    From = 1,
    {ok, State, Reply} = do_call(Info, From, State),
    erlang:send(From, Reply),
    {ok, State};
websocket_info({'ws_info', Info}, State) ->
    do_info(Info, State);
websocket_info(Info, State) ->
    logger:warning_msg("~p websocket info WARNING! ~p~n", [?MODULE, Info]),
    {ok, State}.

terminate(Reason, Req, _State) ->
    lib_gate:terminate(Reason, Req).

%% ------------------------------------------------------------------
do_cast({stop}, State) ->
    {stop, State};

do_cast({'send_data', BinData}, State) ->
    {reply, {binary, BinData}, State};

do_cast(Msg, State) ->
    logger:warning_msg("~p cast no match! info = ~p~n", [?MODULE, Msg]),
    {ok, State}.

%% ------------------------------------------------------------------
do_call(Msg, From, State) ->
    logger:warning_msg("~p call no match! info = ~p~n", [?MODULE, {Msg, From}]),
    Reply = ok,
    {ok, State, Reply}.

%% ------------------------------------------------------------------
do_info({stop}, State) ->
    {stop, State};

do_info({'check_valid'}, State) ->
    lib_gate:timer(?CHECK_INTERVAL, self(), {'check_valid'}),
    lib_gate:check_valid(),
    {ok, State};

do_info(Msg, State) ->
    logger:warning_msg("~p info no match =~p ~p", [?MODULE, Msg]),
    {ok, State}.
