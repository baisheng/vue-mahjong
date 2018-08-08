%% @author zouv
%% @doc 玩家进程

-module(mod_player).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("player.hrl").
-include_lib("db/include/record.hrl").

-define(TIMER_TIME,             60).         % 玩家进程定时器

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         start/1,
         start_link/1
        ]).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

start_link([PlayerId, IsNewPlayer, ClientIP, RLoginData]) ->
    gen_server:start_link(?MODULE, [PlayerId, IsNewPlayer, ClientIP, RLoginData], [{spawn_opt, [{fullsweep_after, 5000}]}]).

start([PlayerId, IsNewPlayer, ClientIP, RLoginData]) ->
    gen_server:start(?MODULE, [PlayerId, IsNewPlayer, ClientIP, RLoginData], [{spawn_opt, [{fullsweep_after, 500}]}]).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([PlayerId, IsPlayerNew, ClientIP, RLoginData]) ->
    case mod_player_manager:get_pid(PlayerId) of
        {ok, OldPlayerPid} ->
            logger:warning_msg("start login second: ~p", [{OldPlayerPid, PlayerId, IsPlayerNew, ClientIP, RLoginData}]),
            {stop, "start login second"};
        _ ->
            process_flag(trap_exit, true),
            ProcessName = misc:get_player_process_name(PlayerId),
            util_process:register_global(ProcessName, self()),
            util:init_rand_seed(),
            PlayerInfo = load_player_info(PlayerId, ClientIP, RLoginData),
            PlayerInfo1 = load_player_info_after(PlayerInfo),
            mod_player_manager:add_role_pid(PlayerId, self(), PlayerInfo1#player_info.account, PlayerInfo1#player_info.uid),
            {ok, #state{}}
    end.

%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch
        Error:Reason ->
            logger:warning_msg("mod call Error! info = ~p~n, stack = ~p~n", [{Error, Reason, Request}, erlang:get_stacktrace()]),
            {reply, ok, State}
    end.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(Msg, State) ->
    try
        do_cast(Msg, State)
    catch
        Error:Reason ->
            logger:warning_msg("mod cast Error! info = ~p~n, stack = ~p~n", [{Error, Reason, Msg}, erlang:get_stacktrace()]),
            {noreply, State}
    end.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch
        Error:Reason ->
            logger:warning_msg("mod cast Error! info = ~p~n, stack = ~p~n", [{Error, Reason, Info}, erlang:get_stacktrace()]),
            {noreply, State}
    end.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, _State) ->
    NowSeconds = mod_time:now_seconds(),
    PlayerInfo = lib_player_procdict:get_dic_player_info(),
    NewPlayerInfo = PlayerInfo#player_info{logout_time = NowSeconds},
    %%io:format("~p terminated! ~p~n", [?MODULE, {_Reason, PlayerInfo#player_info.id, PlayerInfo#player_info.uid}]),
    unload_player_info(NewPlayerInfo).

%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% 同步逻辑处理
do_call({'handle', Module, Function, Args}, _From, State) ->
    Return = erlang:apply(Module, Function, Args),
    {reply, Return, State};

do_call({'stop', Reason}, _From, State) ->
    %%io:format("call player stop proc : ~p~n", [Reason]),
    {stop, normal, ok, State};
do_call(Request, From, State) ->
    logger:warning_msg("mod call bad match! info = ~p~n", [{?MODULE, Request, From}]),
    {reply, ok, State}.

%% ====================================================================
%% 异步逻辑处理
do_cast({'handle', Module, Function, Args}, State) ->
    erlang:apply(Module, Function, Args),
    {noreply, State};

do_cast({'stop', Reason}, State) ->
    %%io:format("cast player stop proc : ~p~n", [Reason]),
    {stop, normal, State};

do_cast({'send_data', Msg}, State) ->
    lib_player:send_msg_to_self(Msg),
    {noreply, State};

do_cast({'change_game_room_id', RoomId, GameRoomId}, State) ->
    PlayerInfo = lib_player_procdict:get_dic_player_info(),
    %%io:format("change_game_room_id___ ~p~n", [{GameRoomId, PlayerInfo#player_info.id}]),
    MyRoomId = PlayerInfo#player_info.game_room_id,
    if
        GameRoomId =/= 0 orelse MyRoomId == 0 orelse MyRoomId == RoomId -> 
            %% 如果不等于0 一定是开始，如果有开始，那一定变化；否则就是roomId与MyRoomId 是同一房间改变
            NewPlayerInfo = PlayerInfo#player_info{game_room_id = GameRoomId},
            lib_player:save_player_info(NewPlayerInfo);
        true ->
            skip
    end,
    {noreply, State};
do_cast(Msg, State) ->
    logger:warning_msg("mod cast bad match! info = ~p~n", [{?MODULE, Msg}]),
    {noreply, State}.

%% ====================================================================
do_info({'handle', Module, Function, Args}, State) ->
    erlang:apply(Module, Function, Args),
    {noreply, State};

do_info(Info, State) ->
    logger:warning_msg("mod info bad match! info = ~p~n", [{?MODULE, Info}]),
    {noreply, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%% 从数据库加载玩家数据
load_player_info(PlayerId, ClientIP, RLoginData) ->
    NowSeconds = mod_time:now_seconds(),
    PlayerInfo = lib_player:init_player_info(NowSeconds, PlayerId, ClientIP, RLoginData),
    %% 加载麻将数据
    lib_mahjong_fight_player:init_player_mahjong_fight(PlayerId),
    lib_player:save_player_info(PlayerInfo),
    PlayerInfo.

%% 数据初始化后的一些处理
load_player_info_after(PlayerInfo) ->
    PlayerInfo.

%% 卸载玩家数据
unload_player_info(PlayerInfo) ->
    PlayerId = PlayerInfo#player_info.id,
    %% 关闭网关
    GatePid = lib_player_procdict:get_dic_gate_pid(),
    case is_pid(GatePid) andalso is_process_alive(GatePid) of
        true ->
            lib_gate:timer(500, GatePid, {'stop'}),
            NewPlayerInfo = lib_player:close_gate_proc_handle(PlayerInfo);
        _ ->
            NewPlayerInfo = PlayerInfo
    end,
    mod_player_manager:del_role_pid(PlayerId),

    %% 更新麻将数据
    % lib_mahjong_fight_player:save_player_mahjong_fight(),
    % save_player_data(?PLAYER_DATA_SAVE_FLAG_UNLOAD, NewPlayerInfo),  % 是使用mnesia，数据都实时保存
    ok.
