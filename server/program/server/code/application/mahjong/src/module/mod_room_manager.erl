%% @author zouv
%% @doc @todo 房间管理进程

-module(mod_room_manager).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("core.hrl").

-define(TIMER_INTERVAL,                 1000).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         start_link/1,
         get_mod_pid/0,

         get_pid_only/1,
         get_pid/1
        ]).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {
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
do_call(Request, From, State) ->
    logger:warning_msg("mod call bad match! info = ~p~n", [{?MODULE, Request, From}]),
    {reply, ok, State}.

%% ====================================================================
do_cast({'handle', Module, Function, Args}, State) ->
    erlang:apply(Module, Function, Args),
    {noreply, State};
%% 创建新分线
do_cast({'create_new_line', _MapId}, State) ->
    % #state{
    %     line_index = LineIndex
    % } = State,
    % {NewIndex, NewLineIndex} = repair_state_line_index(MapId, LineIndex),   % 修复ets_map_line与state.line_index不一致
    % mod_map:create_map(MapId, NewIndex),
    % NewState = State#state{line_index = NewLineIndex},
    {noreply, State};

do_cast(Msg, State) ->
    logger:warning_msg("mod cast bad match! info = ~p~n", [{?MODULE, Msg}]),
    {noreply, State}.

%% ====================================================================
do_info({'timer'},State) ->
    erlang:send_after(?TIMER_INTERVAL, self(), {'timer'}),
    {noreply,State};

do_info(Info, State) ->
    logger:warning_msg("mod info bad match! info = ~p~n", [{?MODULE, Info}]),
    {noreply, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% 查找Pid
get_pid_only(UniqueFlag) ->
    ProcessName = misc:get_room_process_name(UniqueFlag),
    util_process:get_global_pid(ProcessName).

%% 查找RoomPid
get_pid(RoomId) ->
    Pattern = #ets_room_pid{room_id = RoomId, _ = '_'},
    List = ets:match_object(?ETS_ROOM_PID, Pattern),
    case List of
        [] ->
            {UniqueFlag, _Pid} = mod_room:create(RoomId);
        [RRomePid] ->
            UniqueFlag = RRomePid#ets_room_pid.unique_flag
    end,
    case get_pid_only(UniqueFlag) of
        Pid when is_pid(Pid) ->
            {Pid, UniqueFlag};
        R ->
            logger:warning_msg("get pid Error! info = ~p~n", [{R, UniqueFlag, RoomId, length(List)}]),
            []
    end.
