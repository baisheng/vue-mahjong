%% @author 
%% @doc 玩家

-module(lib_player).

-include("common.hrl").
-include_lib("db/include/record.hrl").
-include_lib("protob/include/p04_player_pb.hrl").
-include_lib("protob/include/p05_common_pb.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
    create_role/1,
    init_player_info/4,
    call_close_player_proc/2,
    cast_close_player_proc/2,
    get_ets_player_pid_by_account/1,
    gate_proc_closed/0,
    close_gate_proc_handle/1,
    set_gate_proc/1,
    save_player_info/1,
    kick_player_all/0,
    send_msg_to_self/1,
    send_msg_by_player_id/2,
    send_player_info/0
 ]).

create_role(Account) ->
    NowSeconds = util:now_seconds(),
    PlayerInfo = #player_info{},
    PlayerId = db_auto_increment:get_table_auto_increment(PlayerInfo),
    _PlayerName = make_player_name_for_num_str(PlayerId),
    NewPlayerInfo = 
        PlayerInfo#player_info{
            id = PlayerId,
            account = Account,
            uid = init_uid(PlayerId),
            % name = PlayerName, 
            diamond = 100000,
            % name = Account, 
            create_time = NowSeconds
        },
    db_agent_player_info:update(NewPlayerInfo),
    NewPlayerInfo.

init_uid(_PlayerId) ->
    "".

init_player_info(NowSeconds, PlayerId, ClientIP, RLoginData) ->
    [PlayerInfo] = db_agent_player_info:get(PlayerId),
     io:format("init_player_info~p~n",[RLoginData#r_login_data.player_name]),
    Diamond = ?CONDITIONAL_EXPRESSION(PlayerInfo#player_info.diamond == 0, 10000, PlayerInfo#player_info.diamond),
    PlayerInfo#player_info{
        last_login_ip = ClientIP,
        login_time = NowSeconds,
        icon = RLoginData#r_login_data.player_icon,
        name = RLoginData#r_login_data.player_name,
        diamond = Diamond,
        touch_room_id = 0
    }.

call_close_player_proc(PlayerPid, Reason) ->
    try 
        gen_server:call(PlayerPid, {'stop', Reason}, 5000)
    catch
        E:R ->
            logger:msg("~p close player proc Error! info = ~p~n ", [?MODULE, {E, R}]),
            case R of
                {timeout, _} ->
                    logger:msg("pid : ~p, info : ~p~n", [PlayerPid, {erlang:process_info(PlayerPid, message_queue_len), erlang:process_info(PlayerPid, current_function)}]);
                _ ->
                    skip
            end,
            erlang:throw("close player proc Error!")
    end.

cast_close_player_proc(PlayerPid, Reason) ->
    gen_server:cast(PlayerPid, {'stop', Reason}).

%% 发生网关中断
gate_proc_closed() ->
    PlayerInfo = lib_player_procdict:get_dic_player_info(),
    _NewPlayerInfo = close_gate_proc_handle(PlayerInfo),
    %%io:format("~p gate_proc_closed___1 ~p~n", [?MODULE, mod_time:time_string()]),
    ok.

%% 关闭网关处理
close_gate_proc_handle(PlayerInfo) ->
    #player_info{
                 id = PlayerId
                } = PlayerInfo,
    NowSeconds = mod_time:now_seconds(),
    lib_player_procdict:update_dic_gate_pid(?UNDEFINED),
    lib_player_procdict:update_dic_gate_proc_close_time(NowSeconds),
    %% 在线统计扣除
    del_ets_online(PlayerId),
    lib_room:logout_quit(),
    PlayerInfo.

%% 设置网关
set_gate_proc(GatePid) ->
    OldGatePid = lib_player_procdict:get_dic_gate_pid(),
    lib_player_procdict:update_dic_gate_pid(GatePid),
    PlayerInfo = lib_player_procdict:get_dic_player_info(),
    update_ets_online(PlayerInfo),
    %%io:format("set_gate_proc___ ~p~n", [{OldGatePid, GatePid}]),
    if 
        OldGatePid == ?UNDEFINED ->
            send_login_init();
        OldGatePid /= GatePid ->
            % TODO 设置了新网关，关闭旧网关
            ok;
        true ->
            ok
    end.

%% 从ets_player_pid中获取
get_ets_player_pid_by_account(Account) ->
    case ets:match_object(?ETS_PLAYER_PID, #ets_player_pid{account = Account, _ = '_'}) of
        [EtsPlayerPid | L] ->
            if
                length(L) > 0 ->
                    logger:warning_msg("get ets player pid by account WARNING 1 ! ~p~n", [[EtsPlayerPid | L]]);
                true ->
                    skip
            end,
            case mod_player_manager:get_pid(EtsPlayerPid#ets_player_pid.player_id) of
                {ok, _PlayerPid} ->
                    EtsPlayerPid;
                _ ->
                    logger:warning_msg("get ets player pid by account WARNING 2 ! ~p~n", [Account]),
                    mod_player_manager:del_role_pid(EtsPlayerPid#ets_player_pid.player_id),
                    []
            end;
        _ ->
            []
    end.

save_player_info(NewPlayerInfo) ->
    lib_player_procdict:update_dic_player_info(NewPlayerInfo),
    db_agent_player_info:update(NewPlayerInfo),
    ok.

update_ets_online(PlayerInfo) ->
    GatePid = lib_player_procdict:get_dic_gate_pid(),
    if
        is_pid(GatePid) ->
            #player_info{
                         account = Account,
                         uid = Uid,
                         id = PlayerId
                        } = PlayerInfo,
            NewEtsOnline =
                #ets_online{
                            account = Account,
                            uid = Uid,
                            player_id = PlayerId,
                            player_pid = self(),
                            gate_pid = GatePid
                           },
            ets:insert(?ETS_ONLINE, NewEtsOnline);
        true ->
            skip
    end.

del_ets_online(PlayerId) ->
    ets:delete(?ETS_ONLINE, PlayerId).

send_login_init() ->
    send_player_info(),
    send_common_game_init(),
    lib_record:cs_query_game_record(0),
    ok.

%% 发送玩家信息
send_player_info() ->
    GatePid = lib_player_procdict:get_dic_gate_pid(),
    PlayerInfo = lib_player_procdict:get_dic_player_info(),
    Msg = 
        #sc_player_info{
            id = PlayerInfo#player_info.id,
            name = PlayerInfo#player_info.name,
            diamond = PlayerInfo#player_info.diamond,
            iconurl = PlayerInfo#player_info.icon
        },
    lib_gate:send_data(GatePid, Msg).

send_common_game_init() ->
    GatePid = lib_player_procdict:get_dic_gate_pid(),
    PlayerInfo = lib_player_procdict:get_dic_player_info(),
    GameRoomId = PlayerInfo#player_info.game_room_id,
    if GameRoomId > 0 ->
        case ets:lookup(?ETS_ROOM_INFO, GameRoomId) of
            [RRoomInfo] when RRoomInfo#ets_room_info.is_start == true ->
                Flag = ?GAME_INIT_FLAG_ROOM;
            _ ->
                Flag = ?GAME_INIT_FLAG_MAIN,
                lib_player:save_player_info(PlayerInfo#player_info{game_room_id = 0})
        end;
    true ->
        Flag = ?GAME_INIT_FLAG_MAIN
    end,
    Msg = 
        #sc_common_game_init{
            flag = Flag,
            roomid = GameRoomId
        },
    ?PRINT(["send_common_game_init___", Msg, PlayerInfo#player_info.touch_room_id]),
    lib_gate:send_data(GatePid, Msg).

%% 玩家名称是否存在
is_player_name_exists(PlayerName) ->
    case db_agent_player_info:get_by_name(PlayerName) of
        [PlayerInfo | _] ->
            {true, PlayerInfo};
        _ ->
            false
    end.

make_player_name_for_num_str(PlayerId) ->
    make_player_name_for_num_str(PlayerId, 0).

make_player_name_for_num_str(PlayerId, Index) ->
    TempPlayerName = erlang:integer_to_list(PlayerId * util:rand(11, 99)),
    case is_player_name_exists(TempPlayerName) of
        %% 昵称已存在
        {true, _PlayerInfo} ->
            if
                Index < 10 ->
                    make_player_name_for_num_str(PlayerId, Index + 1);
                true ->
                    logger:warning_msg("make player name for num str Overflow ! ~p~n", [erlang:localtime()]),
                    erlang:integer_to_list(util:now_seconds() * 10)
            end;
        _ ->
            TempPlayerName
    end.

%% 踢下线，所有玩家（关服操作前需先踢所有玩家下线）
%lib_player:kick_player_all().
kick_player_all() ->
    logger:msg("kick player all ___size ~p~n", [ets:info(?ETS_PLAYER_PID, size)]),
    WaitIndex = 100,
    KickN =
        ets:foldl(fun(E, Acc) ->
                      EPlayerId = E#ets_player_pid.player_id,
                      EPlayerPid = E#ets_player_pid.player_pid,
                      if
                          is_pid(EPlayerPid) ->
                              case is_process_alive(EPlayerPid) of
                                  true ->
                                      if 
                                          %% 没踢100个等待1s
                                          Acc rem WaitIndex == 0 andalso Acc /= 0 ->
                                              timer:sleep(1000);
                                          true ->
                                              skip
                                      end,
                                      cast_close_player_proc(EPlayerPid, "kick_all"),
                                      Acc + 1;
                                  _ ->
                                      mod_player_manager:del_role_pid(EPlayerId),
                                      Acc
                              end;
                          true ->
                              mod_player_manager:del_role_pid(EPlayerId),
                              Acc
                      end
                  end,
                  0,
                  ?ETS_PLAYER_PID),
    LeftN = ets:info(?ETS_PLAYER_PID, size),
    logger:msg("kick player all___end ~p~n", [{lists:concat(["kick_num:", KickN]), lists:concat(["left_num:", LeftN])}]),
    {lists:concat(["kick_num:", KickN]), lists:concat(["left_num:", LeftN])}.

%% 发送消息给自己  
send_msg_to_self(Msg) ->
    case lib_player_procdict:get_dic_gate_pid() of
        undefined ->
            ok;
        GatePid ->
            lib_gate:send_data(GatePid, Msg)
    end.

send_msg_by_player_id(PlayerId, Msg) ->
    case ets:lookup(?ETS_PLAYER_PID, PlayerId) of
        [EtsPlayerPid] ->
            gen_server:cast(EtsPlayerPid#ets_player_pid.player_pid, {'send_data', Msg});
        _ ->
            skip
    end.
