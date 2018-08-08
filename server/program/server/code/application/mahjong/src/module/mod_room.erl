%% @author zouv
%% @doc @todo 房间管理进程

-module(mod_room).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("core.hrl").

-define(TIMER_INTERVAL,                 1000).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         start_link/3,
         start/3,
         get_mod_pid/0,

         create/1,
         create_local/1
        ]).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {
        unique_flag = "",
        room_id = 0
    }).

start_link(ProcessName, UniqueId, Id) ->
    gen_server:start_link(?MODULE, [ProcessName, UniqueId, Id], []).

start(ProcessName, UniqueId, Id) ->
    gen_server:start(?MODULE, [ProcessName, UniqueId, Id], []).

get_mod_pid() ->
    misc:get_local_server_mod_pid(?MODULE).

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
init([ProcessName, MapUniqueFlag, RoomId]) ->
    case mod_room_manager:get_pid_only(RoomId) of
        MapPid when is_pid(MapPid) ->
            logger:warning_msg("Error! create room repeated ! ~p~n", [{ProcessName, MapUniqueFlag, RoomId}]),
            {stop, "create room repeated"};
        _ ->
            util_process:register_global(ProcessName, self()),
            NowSeconds = mod_time:now_seconds(),
            %% 初始化
            erlang:send_after(?TIMER_INTERVAL, self(), {'timer', NowSeconds}),
            lib_room:init(MapUniqueFlag, RoomId),
            lib_room:add_ets_room_pid(MapUniqueFlag, RoomId, self()),
            {ok, #state{unique_flag = MapUniqueFlag, room_id = RoomId}}
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
terminate(_Reason, State) ->
    lib_room:del_ets_room_pid(State#state.unique_flag),
    logger:warning_msg("~p terminated ~n", [?MODULE]),
    ok.


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
do_call(Request, From, State) ->
    logger:warning_msg("mod call bad match! info = ~p~n", [{?MODULE, Request, From}]),
    {reply, ok, State}.

%% ====================================================================
do_cast({'handle', Module, Function, Args}, State) ->
    erlang:apply(Module, Function, Args),
    {noreply, State};
% do_cast({'mahjong_quit', PlayerId}, State) ->
%     lib_mahjong_fight:quit_battle(PlayerInfo),
%     {noreply, State};
do_cast(Msg, State) ->
    logger:warning_msg("mod cast bad match! info = ~p~n", [{?MODULE, Msg}]),
    {noreply, State}.

%% ====================================================================
do_info({'handle', Module, Function, Args}, State) ->
    erlang:apply(Module, Function, Args),
    {noreply, State};
do_info({'timer', _OldSeconds},State) ->
    NowSeconds = mod_time:now_seconds(),
    erlang:send_after(?TIMER_INTERVAL, self(), {'timer', NowSeconds}),
    {noreply,State};
do_info('stop', State) ->
    lib_room:del_ets_room_pid(State#state.unique_flag),
    %%io:format("room mod stop: ~p~n", [self()]),
    {stop, normal, State};
do_info(Info, State) ->
    logger:warning_msg("mod info bad match! info = ~p~n", [{?MODULE, Info}]),
    {noreply, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
create(RoomId) ->
    case misc:is_main_node() of
        true ->
            case misc:get_fight_node_list() of
                [] ->
                    create_local(RoomId);
                FightNodeList ->
                    create_load_balance(RoomId, FightNodeList)
            end;
        _ ->
            throw("create Error!")
    end.

create_local(RoomId) ->
    UniqueFlag = lib_room:make_room_unique_flag(RoomId),
    case get_room_pid(UniqueFlag, RoomId) of
        Pid when is_pid(Pid) ->
            {UniqueFlag, Pid};
        R ->
            logger:msg("create local Error! ~p~n", [{RoomId, UniqueFlag, R}]),
            []
    end.

create_load_balance(RoomId, FightNodeList) ->
    Index = util:rand(1, length(FightNodeList)),
    Node = lists:nth(Index, FightNodeList),
    case rpc:call(Node, ?MODULE, create_local, [RoomId]) of
        {badrpc, Reason} ->
            logger:msg("create load balance Error! ~p~n", [{RoomId, Node, Reason}]),
            [];
        R ->
            R
    end.

%% 获取地图Pid
get_room_pid(UniqueFlag, Id) ->
    ProcessName = misc:get_room_process_name(UniqueFlag),
    case util_process:get_global_pid(ProcessName) of
        ModPid when is_pid(ModPid) ->
            case misc:is_process_alive(ModPid) of
                true ->
                    ModPid;
                _ ->
                    get_room_pid1(ProcessName, UniqueFlag, Id)
            end;
        _ ->
            get_room_pid1(ProcessName, UniqueFlag, Id)
    end.

get_room_pid1(ProcessName, UniqueFlag, Id) ->
    global:set_lock({ProcessName, ?UNDEFINED}),
    ModPid =
        case ?MODULE:start(ProcessName, UniqueFlag, Id) of
            {ok, Pid} ->
                Pid;
            Error ->
                logger:msg("get pid1 Error! ~p~n", [{ProcessName, UniqueFlag, Id, Error}]),
                ?UNDEFINED
        end,
    global:del_lock({ProcessName, ?UNDEFINED}),
    ModPid.
