
-module(lib_gate).

-include_lib("protob/include/p01_login_pb.hrl").

-define(DIC_INFO, dic_info).
-define(HEARTBEAT_INTERVAL,                     30).            % 允许丢失客户端心跳的间隔
-define(LOST_PLAYER_PID_INTERVAL,               60).            % 允许丢失玩家进程Pid的间隔

-record(r_info, {
    account = "",
    player_id = 0,
    player_pid = undefined,
    login_time = 0,
    heartbeat_time = 0,
    lost_player_pid_time = 0,
    client_ip = "",
    recon_key = ""
 }).

-export([
    %% 消息接口
    cast/2,
    call/2,
    call/3,
    timer/3,

    %% 协议处理
    ok/3,
    login/3,
    logoff/3,
    login_reconnection/3,
    heartbeat/3,
    player_msg/3,

    %% 其他
    terminate/2,
    send_data/2,
    check_valid/0
 ]).

%% -------------------------------------------------- 消息接口
%% cast
cast(Pid, Info) ->
    erlang:send(Pid, {'ws_cast', Info}).

%% call
call(Pid, Info) ->
    case catch gen:call(Pid, 'ws_call', Info) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            exit({Reason, {?MODULE, call, [Pid, Info]}})
    end.

call(Pid, Info, Timeout) ->
    case catch gen:call(Pid, 'ws_call', Info, Timeout) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            exit({Reason, {?MODULE, call, [Pid, Info, Timeout]}})
    end.

timer(Delay, Pid, Info) ->
    erlang:send_after(Delay, Pid, {'ws_info', Info}).

%% -------------------------------------------------- 协议处理
ok(_ProtoModule, _Pid, _Msg) ->
    ok.

login(_ProtoModule, _Pid, Msg) ->
    #cs_login{
        account = Account,
        iconurl = IconUrl,
        name = Name
    } = Msg,
    Info = get_info(),
    if
        Account == "" ->
            send_data(package_sc_login(0, "账号无效", ""));
        Info#r_info.account /= "" ->
            logger:warning_msg("~p request login repeated ~p~n", [?MODULE, Info]),
            send_data(package_sc_login(0, "重复的登录协议", ""));
        true ->
            NowSeconds = mod_time:now_seconds(),
            {PlayerId, IsNew, RLoginData} = lib_login:login_query_account_info(Account, IconUrl, Name),
            {ok, PlayerPid} = mod_player_manager:get_player_pid_for_login(PlayerId, Account, IsNew, "", RLoginData),
            ReconKey = integer_to_list(mod_time:now_long_seconds()),
            NewInfo = 
                #r_info{
                    account = Account,
                    player_id = PlayerPid,
                    player_pid = PlayerPid,
                    login_time = NowSeconds,
                    recon_key = ReconKey
                },
            update_info(NewInfo),
            put(dic_account, Account),
            gen_server:cast(NewInfo#r_info.player_pid, {'handle', lib_player, set_gate_proc, [self()]}),
            send_data(package_sc_login(1, "", ReconKey))
    end.

logoff(_ProtoModule, _Pid, _Msg) ->
    Info = get_info(),
    NewInfo = 
        Info#r_info{
            player_pid = undefined
        },
    update_info(NewInfo),
    call_player_close_player_proc(),
    lib_gate:timer(500, self(), {'stop'}).

login_reconnection(_ProtoModule, _Pid, Msg) ->
    io:format("login_reconnection___ ~p", [{_ProtoModule, _Pid, Msg}]),
    #cs_login_reconnection{
        account = _Account,
        reconnectkey = _ReconnectKey
    } = Msg,
    % Info = get_info(),
    ok.

player_msg(ProtoModule, _Pid, Msg) ->
    Info = get_info(),
    [_, _, _ | File] = ProtoModule,
    Module = list_to_atom(lists:concat(["ge", File])),
    gen_server:cast(Info#r_info.player_pid, {'handle', Module, handle, [Msg]}).

heartbeat(_ProtoModule, _Pid, _Msg) ->
    NowSeconds = mod_time:now_seconds(),
    Info = get_info(),
    update_info(Info#r_info{heartbeat_time = NowSeconds}),
    send_data(#sc_login_heartbeat{}).

%% -------------------------------------------------- 其他
get_info() ->
    case get(?DIC_INFO) of
        undefined ->
            #r_info{};
        Info ->
            Info
    end.

update_info(Info) ->
    put(?DIC_INFO, Info).

terminate(Reason, Req) ->
    io:format("~p terminate ~p~n", [?MODULE, {Reason, Req, self()}]),
    cast_player_gate_proc_closed().

send_data(Msg) ->
    send_data(self(), Msg).

send_data(undefined, _Msg) -> ok;
send_data(GatePid, Msg) ->
    BinData = socket_buffer:encode(Msg),
    lib_gate:cast(GatePid, {'send_data', BinData}).

%% 检测连接的持续有效性
check_valid() ->
    NowSeconds = mod_time:now_seconds(),
    Info = get_info(),
    #r_info{
        heartbeat_time = HeartbeatTime
    }  = Info,
    if 
        HeartbeatTime == 0 ->
            update_info(Info#r_info{heartbeat_time = NowSeconds});
        true ->
            case NowSeconds - HeartbeatTime > ?HEARTBEAT_INTERVAL of
                true ->
                    io:format("tcp alive check, lost heartbeat ! ~p~n", [{NowSeconds - HeartbeatTime, Info#r_info.account, self()}]),
                    cast_player_gate_proc_closed(),
                    lib_gate:timer(500, self(), {'stop'});
                _ ->
                    check_valid1(Info, NowSeconds)
            end
    end.

check_valid1(Info, NowSeconds) ->
    #r_info{
        player_pid = PlayerPid,
        lost_player_pid_time = LostPlayerPidTime
    }  = Info,
    case is_pid(PlayerPid) andalso misc:is_process_alive(PlayerPid) of
        true when LostPlayerPidTime == 0 ->
            skip;
        true ->
            update_info(Info#r_info{lost_player_pid_time = 0});
        _ when LostPlayerPidTime == 0 ->
            update_info(Info#r_info{lost_player_pid_time = NowSeconds});
        _ when NowSeconds - LostPlayerPidTime > ?LOST_PLAYER_PID_INTERVAL ->
            io:format("tcp alive check, lost player pid ! ~p~n", [{NowSeconds - LostPlayerPidTime, Info#r_info.account}]),
            lib_gate:timer(500, self(), {'stop'});
        _ ->
            skip
    end.

%% 通知网关中断
cast_player_gate_proc_closed() ->
    Info = get_info(),
    #r_info{
        player_pid = PlayerPid
    }  = Info,
    io:format("cast player close gate proc___ ~p~n", [{PlayerPid, is_pid(PlayerPid), misc:is_process_alive(PlayerPid)}]),
    case is_pid(PlayerPid) andalso misc:is_process_alive(PlayerPid) of
        true ->
            gen_server:cast(PlayerPid, {'handle', lib_player, gate_proc_closed, []});
        _ ->
            skip
    end.

%% 通知关闭玩家进程
call_player_close_player_proc() ->
    Info = get_info(),
    #r_info{
        player_pid = PlayerPid
    }  = Info,
    io:format("call player close player proc___ ~p~n", [{self(), PlayerPid, is_pid(PlayerPid), misc:is_process_alive(PlayerPid)}]),
    case is_pid(PlayerPid) andalso misc:is_process_alive(PlayerPid) of
        true ->
            gen_server:call(PlayerPid, {'stop', "clean gate proc"});
        _ ->
            skip
    end.

package_sc_login(Result, Reason, _ReconnectKey) ->
    #sc_login{result = Result, reason = Reason}.
