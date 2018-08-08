%% @author zouv
%% @doc @todo group组管理进程

-module(mod_room_info).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include_lib("db/include/record.hrl").
-define(TIMER_INTERVAL,                 600).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         start_link/1,
         get_mod_pid/0
        ]).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {
    ref = undefined,
    line_index = []
    }).

start_link(ModProcessName) ->
    gen_server:start_link(?MODULE, [ModProcessName], []).

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
init([ModProcessName]) ->
    process_flag(trap_exit, true),
    util_process:register_global(ModProcessName, self()),
    {ok, #state{}}.


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
do_call({'player_enter_room', RoomId, RRoomMember}, _From, State) ->
    Return = lib_room_info:enter_room(RoomId, RRoomMember),
    {reply, Return, State};

do_call({'player_sit', RoomId, Position, RRoomMember}, _From, State) ->
    case lib_room_info:pre_player_sit_check(RoomId, Position, RRoomMember) of
        {true, RRoomInfo} ->
            NewRRoomInfo = lib_room_info:update_room_member(RRoomMember, RRoomInfo),
            lib_room_info:update_room_info(NewRRoomInfo),
            Return = {true, NewRRoomInfo};
        Return ->
            ok
    end,
    {reply, Return, State};

%% 玩家退出房间
do_call({'player_quit', RoomId, PlayerId}, _From, State) ->
    Return = lib_room_info:player_quit(RoomId, PlayerId),
    {reply, Return, State};

do_call(Request, From, State) ->
    logger:warning_msg("mod call bad match! info = ~p~n", [{?MODULE, Request, From}]),
    {reply, ok, State}.

%% ====================================================================
do_cast({'handle', Module, Function, Args}, State) ->
    erlang:apply(Module, Function, Args),
    {noreply, State};

%% 创建房间
do_cast({'create_room_info', RRoomInfo}, State) ->
    #ets_room_info{
                wait_long_time = WaitLongTime,
                id = RoomId
    } = RRoomInfo,
    Ref = erlang:send_after(WaitLongTime * 1000, self(), {'clean_room', RoomId}),
    lib_room_info:update_room_info(RRoomInfo#ets_room_info{ref = Ref}),
    {noreply, State};
%% 改变启动过状态
do_cast({'change_room_march', RoomId, IsMarch}, State) ->
    [RRoomInfo] = ets:lookup(?ETS_ROOM_INFO, RoomId),
    #ets_room_info{
                % is_march = OldIsMarch,
                ref = Ref
    } = RRoomInfo,
    %% 先取消定时
    ?CONDITIONAL_EXPRESSION(erlang:is_reference(Ref),  erlang:cancel_timer(Ref), ok),
    case IsMarch of
        true ->
            NewRef = undefined;
        _ -> %% 如果牌局所有人都退出改成false 然后房间还有15分钟
            NewRef = erlang:send_after(?ROOM_WAIT_START_TIME * 1000, self(), {'clean_room', RoomId})
    end,
    NewRRoomInfo = RRoomInfo#ets_room_info{is_march = IsMarch, ref = NewRef},
    %%io:format("change_room_march___1 ~p~n", [{IsMarch, NewRef}]),
    lib_room_info:update_room_info(NewRRoomInfo),
    {noreply, State};
%% 改变房间状态
do_cast({'change_room_state', RoomId, GameRoomId, IsStart}, State) ->
    [RRoomInfo] = ets:lookup(?ETS_ROOM_INFO, RoomId),
    RRoomInfo1 = RRoomInfo#ets_room_info{is_start = IsStart, start_time = mod_time:now_seconds()},
    %%io:format("change_room_state___1 ~p~n", [{RoomId, GameRoomId, IsStart, length(RRoomInfo1#ets_room_info.members)}]),
    NewRRoomInfo = 
        lists:foldl(fun(E, Acc) ->
            if E#r_room_member.position > 0 ->
                gen_server:cast(E#r_room_member.player_pid, {'change_game_room_id', RoomId, GameRoomId});
            true ->
                skip
            end,
            if E#r_room_member.is_leave == true ->
                lib_room_info:delete_room_member(E#r_room_member.player_id, Acc);
            true ->
                Acc
            end
        end,
        RRoomInfo1,
        RRoomInfo1#ets_room_info.members),
    %%io:format("change_room_state___2 ~p~n", [{length(NewRRoomInfo#ets_room_info.members)}]),
    lib_room_info:update_room_info(NewRRoomInfo),
    lib_room:send_all_room_update(NewRRoomInfo),
    {noreply, State};
do_cast({'clean_room', RoomId},State) ->
    %%io:format("clean_room__ ~p~n", [RoomId]),
    lib_room:send_room_clean(RoomId),
    lib_room_info:delete_room_info(RoomId),
    {noreply,State};
do_cast(Msg, State) ->
    logger:warning_msg("mod cast bad match! info = ~p~n", [{?MODULE, Msg}]),
    {noreply, State}.

%% ====================================================================
do_info({'timer'},State) ->
    erlang:send_after(?TIMER_INTERVAL * 1000, self(), {'timer'}),
    {noreply,State};
do_info({'clean_room', RoomId},State) ->
    lib_room:send_room_clean(RoomId),
    lib_room_info:delete_room_info(RoomId),
    {noreply,State};
do_info(Info, State) ->
    logger:warning_msg("mod info bad match! info = ~p~n", [{?MODULE, Info}]),
    {noreply, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
