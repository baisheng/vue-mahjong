%% 玩家进程Pid管理

-module(mod_player_manager).
-behaviour(gen_server).

-include("common.hrl").

-define(TIMER_TIME,                 180 * 1000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([
         start_link/1,
         get_mod_pid/0,

         add_role_pid/4,
         del_role_pid/1,
         get_pid/1,
         get_pid_only/1,

         get_player_pid_for_login/5
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-record(state, {}).

start_link(ModProcessName) ->
    gen_server:start_link(?MODULE, [ModProcessName], []).

get_mod_pid() ->
    misc:get_local_server_mod_pid(?MODULE).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([ModProcessName]) ->
    process_flag(trap_exit, true),
    util_process:register_global(ModProcessName, self()),
    erlang:send_after(?TIMER_TIME, self(), {'timer'}),
    {ok, #state{}}.

%% ========================= call =========================
handle_call(Msg, _From, State) ->
    logger:warning_msg("~n ~p ca;; no match =~p", [?MODULE, Msg]),
    {reply, ok, State}.

%% ========================= cast =========================
handle_cast(Msg, State) ->
    logger:warning_msg("~n ~p cast no match =~p", [?MODULE, Msg]),
    {noreply, State}.

%% ========================= info =========================
handle_info('timer', State) ->
    erlang:send_after(?TIMER_TIME, self(), {'timer'}),
    ets:foldl(fun(E, Acc) ->
                  EPlayerId = E#ets_player_pid.player_id,
                  case get_pid(EPlayerId) of
                      {ok, _EPlayerPid} -> ok;
                      _ -> del_role_pid(EPlayerId)
                  end,
                  Acc
              end,
              [],
              ?ETS_PLAYER_PID),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    lib_player:kick_player_all(),
    logger:warning_msg("~p terminated ~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
login_in(PlayerId, IsNewPlayer, ClientIP, RLoginData) ->
    case get_pid(PlayerId) of
        {ok, _OldPlayerPid} ->
            {error, "repeated_1"};
        _ ->
            ResourceId = {mod_player, PlayerId},
            LockRequesterId = erlang:make_ref(),
            case global:set_lock({ResourceId,  LockRequesterId}, [node()], 3) of
                true ->
                    case get_pid(PlayerId) of
                        {ok, _OldPlayerPid} ->
                            Retrun = {error, "repeated_2"};
                        _ ->
                            %case supervisor:start_child(mod_player_sup, [[PlayerId, IsNewPlayer, ClientIP, RLoginData]]) of
                            Retrun = mod_player:start([PlayerId, IsNewPlayer, ClientIP, RLoginData])
                    end,
                    global:del_lock({ResourceId,  LockRequesterId}, [node()]),
                    case Retrun of
                        {ok, PlayerPid} ->
                            {ok, PlayerPid};
                        {ok, PlayerPid, _} ->
                            {ok, PlayerPid};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    {error, "role processor is locked"}
            end
    end.

%% 添加（仅在玩家进程调用）
add_role_pid(PlayerId, PlayerPid, Account, Uid) ->
    EtsRolePid = #ets_player_pid{player_id = PlayerId, player_pid = PlayerPid, account = Account, uid = Uid},
    ets:insert(?ETS_PLAYER_PID, EtsRolePid),
    PlayerPid.

%% 删除
del_role_pid(PlayerId) ->
    ets:delete(?ETS_PLAYER_PID, PlayerId).

%% 获取
get_pid_only(PlayerId) ->
    ProcessName = misc:get_player_process_name(PlayerId),
    util_process:get_global_pid(ProcessName).

get_pid(PlayerId) ->
    case get_pid_only(PlayerId) of
        PlayerPid when is_pid(PlayerPid) ->
            case misc:is_process_alive(PlayerPid) of
                true ->
                    {ok, PlayerPid};
                _ ->
                    {error, not_exist}
            end;
        _ ->
            {error, not_exist}
    end.

%% 获取角色进程pid
get_player_pid_for_login(PlayerId, Account, IsNew, ClientIP, RLoginData) ->
    case lib_player:get_ets_player_pid_by_account(Account) of
        [] ->
            skip;
        EtsPlayerPid ->
            lib_player:call_close_player_proc(EtsPlayerPid#ets_player_pid.player_pid, "login_replace")
    end,
    {ok, PlayerPid} = get_player_pid_for_login1(PlayerId, Account, IsNew, ClientIP, RLoginData, 3),
    {ok, PlayerPid}.

get_player_pid_for_login1(PlayerId, Account, IsNew, ClientIP, RLoginData, RepeatCount) ->
    if
        RepeatCount > 0 ->
            case login_in(PlayerId, IsNew, ClientIP, RLoginData) of
                {ok, PlayerPid} ->
                    {ok, PlayerPid};
                ErrorReason ->
                    logger:warning_msg(" get player pid login repeat1 Warnning !~n ~p~n", [{ErrorReason, PlayerId, ClientIP, RepeatCount}]),
                    get_player_pid_for_login1(PlayerId, Account, IsNew, ClientIP, RLoginData, RepeatCount - 1)
            end;
        true ->
            []
    end.
