%% @author 
%% @doc 房间相关

-module(lib_room).

-include("mahjong_fight.hrl").
-include_lib("db/include/record.hrl").
-include_lib("protob/include/p02_room_pb.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
    init/2,
    add_ets_room_pid/3,
    del_ets_room_pid/1,
    make_room_unique_flag/1,

    cs_room_find/1,
    cs_room_sit/1,
    cs_room_quit/0,
    cs_room_start_mahjong/1,
    logout_quit/0,
    send_all_room_update/1,
    send_room_clean/1
 ]).

init(_UniqueFlag, _RoomId) ->
    ok.

add_ets_room_pid(UniqueFlag, RoomId, Pid) ->
    RRoomPid =
        #ets_room_pid{
                     unique_flag = UniqueFlag,
                     room_id = RoomId,
                     pid = Pid
                    },
    ets:insert(?ETS_ROOM_PID, RRoomPid).
%% 删除
del_ets_room_pid(UniqueFlag) ->
    % UniqueFlag = make_room_unique_flag(RoomId),
    ets:delete(?ETS_ROOM_PID, UniqueFlag).

%% 获取唯一标识
make_room_unique_flag(RoomId) ->
    NodeId = config:get_node_id(),
    lists:concat([NodeId, "_", RoomId]).

%% 查找房间并进入
cs_room_find(Msg) ->
    PlayerInfo = lib_player_procdict:get_dic_player_info(),
    % GameRoomId = PlayerInfo#player_info.game_room_id,
    % EnterRoomId = Msg#cs_room_find.roomid,
    case can_change_room(PlayerInfo) of
        false ->
            ResponseMsg = package_sc_room_find_response(0, "不能切换房间"),
            GatePid = lib_player_procdict:get_dic_gate_pid(),
            lib_gate:send_data(GatePid, ResponseMsg);
        _ ->
            cs_room_find1(Msg, PlayerInfo)
    end.

cs_room_find1(Msg, PlayerInfo) ->
    #cs_room_find{
                roomid = EnterRoomId,
                type = Type,
                longtime = LongTime
    } = Msg,
    % EnterRoomId = Msg#cs_room_find.roomid,
    % Type = Msg#cs_room_find.type,

    %% 进入房间
    if EnterRoomId > 0 ->
        case lib_room_info:find_room(EnterRoomId) of
            {true, RRoomInfo} ->
                Result = 1,
                Reason = "",
                PlayerInfo1 = PlayerInfo;
            {false, Reason} ->
                Result = 0,
                RRoomInfo = [],
                PlayerInfo1 = PlayerInfo
        end;
    %% 创建房间
    true ->
        case pre_room_find_create(Type, LongTime, PlayerInfo) of
            {true, Dict} ->
                Result = 1,
                Reason = "",
                PlayerInfo1 = dict:fetch('player_info', Dict),
                NewType = dict:fetch('type', Dict),
                NewLongTime = dict:fetch('long_time', Dict),
                RRoomInfo = lib_room_info:create_room(PlayerInfo, NewType, NewLongTime);
                %% 为了方便查找，直接把战绩;
            {false, Reason} ->
                Result = 0,
                RRoomInfo = [],
                PlayerInfo1 = PlayerInfo
        end
    end,
    %%io:format("cs_room_find___1 ~p~n", [{Result, RRoomInfo}]),
    if Result == 1 ->
        RRoomMember = make_r_room_member(PlayerInfo1, 0),
        case lib_room_info:call_enter_room(RRoomInfo, RRoomMember) of
            {true, NewRRoomInfo} ->
                NewResult = 1,
                NewReason = "",
                NewPlayerInfo = PlayerInfo1#player_info{touch_room_id = NewRRoomInfo#ets_room_info.id},
                lib_player:save_player_info(NewPlayerInfo),
                %% 增加记录玩家开放的记录
                lib_mahjong_fight_player:update_open_record(NewRRoomInfo),
                send_room_init(NewPlayerInfo, NewRRoomInfo);
            {false, NewReason} ->
                NewResult = 0
        end;
    true ->
        NewResult = Result,
        NewReason = Reason
    end,
    %%io:format("cs_room_find___2 ~p~n", [{NewResult, NewReason}]),
    ResponseMsg = package_sc_room_find_response(NewResult, NewReason),
    GatePid = lib_player_procdict:get_dic_gate_pid(),
    lib_gate:send_data(GatePid, ResponseMsg).

pre_room_find_create(Type, LongTime, PlayerInfo) ->
    % TODO 检测钻石消耗
    CheckFlagList = [
                    'check_type',
                    'check_longtime',
                    'check_cost'
                    ],
    Dict = dict:from_list([
                        {'type', Type},
                        {'long_time', LongTime},
                        {'player_info', PlayerInfo}
                        ]),
    pre_room_find_create_check(Dict, CheckFlagList).

pre_room_find_create_check(Dict, []) ->
    {true, Dict};
pre_room_find_create_check(Dict, ['check_type'|Z]) ->
    Type = dict:fetch('type', Dict),
    case lists:member(Type, ?ROOM_TYPE_LIST) of
        true ->
            NewType = Type;
        false ->
            NewType = ?ROOM_TYPE_1
    end,
    Dict1 = dict:store('type', NewType, Dict),
    pre_room_find_create_check(Dict1, Z);
pre_room_find_create_check(Dict, ['check_longtime'|Z]) ->
    LongTime = dict:fetch('long_time', Dict),
    case lists:keyfind(LongTime, 1, ?ROOM_TIME_COST_LIST) of
        {NewLongTime, Cost} ->
            ok;
        _ ->
            NewLongTime = ?ROOM_TIME_3,
            Cost = ?ROOM_COST_3
    end,
    Dict1 = dict:store('long_time', NewLongTime, Dict),
    Dict2 = dict:store('cost', Cost, Dict1),
    pre_room_find_create_check(Dict2, Z);
pre_room_find_create_check(Dict, ['check_cost'|Z]) ->
    PlayerInfo = dict:fetch('player_info', Dict),
    Cost = dict:fetch('cost', Dict),
    Diamond = PlayerInfo#player_info.diamond,
    %%io:format("pre_room_find_create_check~p~n",[Diamond]),
    if
        Diamond < Cost ->
            {false, "龙玉不足"};
        true ->
            % NewPlayerInfo = PlayerInfo#player_info{diamond = Diamond - Cost},
            % Dict1 = dict:store('player_info', NewPlayerInfo, Dict),
            pre_room_find_create_check(Dict, Z)
    end.

%% 坐下
cs_room_sit(Msg) ->
    EnterRoomId = Msg#cs_room_sit.roomid,
    Position = Msg#cs_room_sit.position,
    %%io:format("cs_room_sit___1 ~p~n", [{EnterRoomId, Position}]),
    PlayerInfo = lib_player_procdict:get_dic_player_info(),
    case pre_room_sit(EnterRoomId, Position, PlayerInfo) of
        {true, DataDict} ->
            RRoomInfo = dict:fetch(room_info, DataDict),
            send_all_room_update(RRoomInfo),
            %% 更新金钱
            NewPlayerInfo = dict:fetch('player_info', DataDict),
            lib_player:save_player_info(NewPlayerInfo),
            lib_player:send_player_info(),
            %% 进入牌局
            lib_mahjong_fight:enter_battle(PlayerInfo, RRoomInfo),
     
            Result = 1,
            Reason = "";
        {false, Reason} ->
            Result = 0
    end,
    ResponseMsg = #sc_room_sit_response{result = Result, reason = Reason},
    GatePid = lib_player_procdict:get_dic_gate_pid(),
    %%io:format("cs_room_sit___3 ~p~n", [{ResponseMsg}]),
    % GatePid = lib_player_procdict:get_dic_gate_pid(),
    lib_gate:send_data(GatePid, ResponseMsg).

pre_room_sit(EnterRoomId, Position, PlayerInfo) ->
    CheckFlagList = [
                     check_room_id,
                     check_position,
                     check_room
                    ],
    RRoomMember = make_r_room_member(PlayerInfo, Position),
    PlayerInfo = lib_player_procdict:get_dic_player_info(),
    DataDict = dict:store(enter_room_id, EnterRoomId, dict:new()),
    DataDict1 = dict:store(position, Position, DataDict),
    DataDict2 = dict:store(member_info, RRoomMember, DataDict1),
    DataDict3 = dict:store(player_info, PlayerInfo, DataDict2),
    pre_room_sit_check(DataDict3, CheckFlagList).

pre_room_sit_check(DataDict, []) ->
    {true, DataDict};
pre_room_sit_check(DataDict, [check_room_id | L]) ->
    EnterRoomId = dict:fetch(enter_room_id, DataDict),
    PlayerInfo = dict:fetch(player_info, DataDict),
    if EnterRoomId /= PlayerInfo#player_info.touch_room_id ->
        {false, "尚未进入该房间"};
    true ->
        pre_room_sit_check(DataDict, L)
    end;
pre_room_sit_check(DataDict, [check_position | L]) ->
    EnterRoomId = dict:fetch(enter_room_id, DataDict),
    Position = dict:fetch(position, DataDict),
    PlayerInfo = dict:fetch(player_info, DataDict),
    #player_info{
                id = PlayerId,
                diamond = PlayerDiamond
    } = PlayerInfo,
    case ets:lookup(?ETS_ROOM_INFO, EnterRoomId) of
        [] ->
            {false, "房间不存在"};
        [RRoomInfo] when RRoomInfo#ets_room_info.is_start == true ->
            {false, "牌局正在进行，不可转换位置"};
        [RRoomInfo] when ?IS_POSITION(Position) ->
            case lists:member(PlayerId, RRoomInfo#ets_room_info.cost_list) of
                true ->
                    Diamond = 0;
                _ ->
                    case lists:keyfind(RRoomInfo#ets_room_info.long_time, 1, ?ROOM_TIME_COST_LIST) of
                        false ->
                            Diamond = ?ROOM_COST_1;
                        {_, Diamond} ->
                            ok
                    end
            end,
            if
                PlayerDiamond < Diamond ->
                    {false, io:format("龙玉不足， 坐下房间需要花费~p龙玉", [Diamond])};
                true ->
                    DataDict1 = dict:store('player_info', PlayerInfo#player_info{diamond = PlayerDiamond - Diamond}, DataDict),
                    pre_room_sit_check(DataDict1, L)
            end;
        [RRoomInfo] ->
            case lists:keyfind(PlayerInfo#player_info.id, #r_room_member.player_id, RRoomInfo#ets_room_info.members) of
                Member when ?IS_POSITION(Member#r_room_member.position) -> %%v站起
                    pre_room_sit_check(DataDict, L);
                _ ->
                    {false, "你没有在位置上，不可站起"}
            end;
        _ ->
            {false, "位置不合理"}
    end;
pre_room_sit_check(DataDict, [check_room | L]) ->
    EnterRoomId = dict:fetch(enter_room_id, DataDict),
    Position = dict:fetch(position, DataDict),
    RRoomMember = dict:fetch(member_info, DataDict),
    case lib_room_info:call_player_sit(EnterRoomId, Position, RRoomMember) of
        {true, RRoomInfo} ->
            %%io:format("cs_room_sit___12 ~p~n", [{RRoomInfo}]),
            DataDict1 = dict:store(room_info, RRoomInfo, DataDict),
            pre_room_sit_check(DataDict1, L);
        {false, _Reason} = R ->
            R
    end.

%% 手动退出房间
cs_room_quit() ->
    case player_quit() of
        true ->
            Result = 1,
            Reason = "";
        {false, Reason} ->
            Result = 0
    end,
    ResponseMsg = #sc_room_quit_response{result = Result, reason = Reason},
    %%io:format("cs_room_quit___4 ~p~n", [{ResponseMsg}]),
    GatePid = lib_player_procdict:get_dic_gate_pid(),
    lib_gate:send_data(GatePid, ResponseMsg).

%% 登出时退出房间
logout_quit() ->
    player_quit(),
    ok.

player_quit() ->
    PlayerInfo = lib_player_procdict:get_dic_player_info(),
    RoomId = PlayerInfo#player_info.touch_room_id,
    %%io:format("player_quit___30 ~p~n", [{RoomId, PlayerInfo#player_info.id}]),
    case pre_player_quit(RoomId) of
        {true, DataDict} ->
            NewPlayerInfo = PlayerInfo#player_info{touch_room_id = 0},
            lib_player:save_player_info(NewPlayerInfo),
            _RRoomInfo = dict:fetch('room_info', DataDict),
            %%io:format("player_quit___31 ~p~n", [{RRoomInfo}]),
            %% 如果游戏已经开始，玩家退出会进入离线挂机状态
            case lib_room_info:call_player_quit(RoomId, PlayerInfo#player_info.id) of
                {true, NewRRoomInfo} ->
                    %%io:format("player_quit___32 ~p~n", [{NewRRoomInfo}]),
                    send_all_room_update(NewRRoomInfo),
                    lib_mahjong_fight:quit_battle(NewPlayerInfo),
                    true;
                {false, _Reason} = R ->
                    R
            end;
        R ->
            R
    end.

pre_player_quit(RoomId) ->
    CheckFlagList = [
        check_room_id,
        check_room
    ],
    DataDict = dict:store(room_id, RoomId, dict:new()),
    pre_player_quit_check(DataDict, CheckFlagList).

pre_player_quit_check(DataDict, []) ->
    {true, DataDict};
pre_player_quit_check(DataDict, [check_room_id | L]) ->
    RoomId = dict:fetch(room_id, DataDict),
    if RoomId =< 0 ->
        {false, "尚未进入房间"};
    true ->
        pre_player_quit_check(DataDict, L)
    end;
pre_player_quit_check(DataDict, [check_room | L]) ->
    RoomId = dict:fetch(room_id, DataDict),
    case ets:lookup(?ETS_ROOM_INFO, RoomId) of
        [RRoomInfo] ->
            DataDict1 = dict:store('room_info', RRoomInfo, DataDict),
            pre_player_quit_check(DataDict1, L);
        _ ->
            {false, "房间不存在"}
    end.

cs_room_start_mahjong(RoomId) ->
    case pre_room_start_mahjong(RoomId) of
        {true, _DataDict} ->
            Result = true,
            Reason = "",
            ok;
        {false, Reason} ->
            Result = false
    end,
    ResponseMsg = #sc_room_start_mahjong{result = Result, reason = Reason},
    %%io:format("cs_room_quit___4 ~p~n", [{ResponseMsg}]),
    GatePid = lib_player_procdict:get_dic_gate_pid(),
    lib_gate:send_data(GatePid, ResponseMsg).

pre_room_start_mahjong(RoomId) ->
    CheckFlagList = [
                    'check_room'
                    ],
    PlayerInfo = lib_player_procdict:get_dic_player_info(),
    DataDict = dict:from_list([
                            {'player_info', PlayerInfo},
                            {'room_id', RoomId}
                            ]),
    pre_room_start_mahjong_check(DataDict, CheckFlagList).
pre_room_start_mahjong_check(DataDict, []) ->
    {true, DataDict};
pre_room_start_mahjong_check(DataDict, ['check_room'|Z]) ->
    RoomId = dict:fetch('room_id', DataDict),
    PlayerInfo = dict:fetch('player_info', DataDict),
    PlayerId = PlayerInfo#player_info.id,
    case lib_room_info:start_mahjong(RoomId, PlayerId) of
        {false, Reason} ->
            {false, Reason};
        _ ->
            pre_room_start_mahjong_check(DataDict, Z)
    end;
pre_room_start_mahjong_check(DataDict, [_|Z]) ->
    pre_room_start_mahjong_check(DataDict, Z).

make_r_room_member(PlayerInfo, Position) ->
    GatePid = lib_player_procdict:get_dic_gate_pid(),
    #r_room_member{
        player_id = PlayerInfo#player_info.id,
        player_name = PlayerInfo#player_info.name,
        player_pid = self(),
        gate_pid = GatePid,
        position = Position,
        player_icon = PlayerInfo#player_info.icon,
        score = PlayerInfo#player_info.score,
        count = PlayerInfo#player_info.count,
        total_count = PlayerInfo#player_info.total_count
    }.

transfer_to_pb_room_member(MmberList, CostList) ->
    lists:map(fun(E) ->
        IsPay = lists:member(E#r_room_member.player_id, CostList),
        #pb_room_member{
            playerid = E#r_room_member.player_id,
            playername = E#r_room_member.player_name,
            position = E#r_room_member.position,
            iconurl = E#r_room_member.player_icon,
            score = E#r_room_member.score,
            wincount = E#r_room_member.count,
            totalcount = E#r_room_member.total_count,
            ispay = IsPay

        }
    end,
    MmberList).

send_room_update(PlayerId, RRoomInfo) ->
    #ets_room_info{
                id = RoomId,
                is_start = IsStart,
                is_march = IsMarch,
                members = MmberList,
                create_time = CreateTIme,
                % start_time = StartTime,
                long_time = LongTime,
                wait_long_time = WaitLongTime,
                record_id = RecordId,
                owner_id = OwnerId,
                type = Type,
                cost_list = CostList
    } = RRoomInfo,
    MmberList1 = 
        lists:filter(fun(E) ->
            (E#r_room_member.player_id == PlayerId) or (E#r_room_member.position > 0)
        end,
        MmberList),
    PbMembers = transfer_to_pb_room_member(MmberList1, CostList),
    EndTime = CreateTIme + WaitLongTime,
    Msg = 
        #sc_room_update{
            roomid = RoomId,
            members = PbMembers,
            isstart = ?CONDITIONAL_EXPRESSION(IsStart, 1, 0),
            ismarch = IsMarch,
            ownerid = OwnerId,
            type = Type,
            endtime = EndTime,
            longtime = LongTime,
            recordid = RecordId
        },
    GatePid = lib_player_procdict:get_dic_gate_pid(),
    lib_gate:send_data(GatePid, Msg).

send_all_room_update(RRoomInfo) ->
    #ets_room_info{
                id = RoomId,
                is_start = IsStart,
                is_march = IsMarch,
                members = MmberList,
                create_time = CreateTIme,
                % start_time = StartTime,
                long_time = LongTime,
                wait_long_time = WaitLongTime,
                record_id = RecordId,
                owner_id = OwnerId,
                type = Type,
                cost_list = CostList

    } = RRoomInfo,
    PbMembers = transfer_to_pb_room_member(MmberList,CostList),
    EndTime = CreateTIme + WaitLongTime,
    % LeftTime = max(0, RRoomInfo#ets_room_info.long_time - (mod_time:now_seconds() - RRoomInfo#ets_room_info.start_time)),
    Msg = 
        #sc_room_update{
            roomid = RoomId,
            members = PbMembers,
            isstart = ?CONDITIONAL_EXPRESSION(IsStart, 1, 0),
            ismarch = IsMarch,
            ownerid = OwnerId,
            type = Type,
            endtime = EndTime,
            longtime = LongTime,
            recordid = RecordId
        },
    lib_room_info:send_room_msg_to_member_list(MmberList, Msg).

%% 进入房间的初始化
%% 适用于：
%%  1、开房后进入
%%  2、通过房间id进入观战
%%  3、断线重新登录后返回房间继续游戏
send_room_init(PlayerInfo, NewRRoomInfo) ->
    PlayerId = PlayerInfo#player_info.id, 
    send_room_update(PlayerId, NewRRoomInfo),   % 房间信息更新
    %% 如果已经开局了，则进入观战，否则忽视
    lib_mahjong_fight:enter_battle(PlayerInfo, NewRRoomInfo),
    ok.

send_room_clean(RoomId) ->
    case ets:lookup(?ETS_ROOM_INFO, RoomId) of
        [RRoomInfo] ->
            Msg = #sc_room_clean{
                            roomid = RoomId
            },
            lib_room_info:send_room_msg_to_member_list(RRoomInfo#ets_room_info.members, Msg),
            ok;
        _ ->
            skip
    end,
    ok.

package_sc_room_find_response(Result, Reason) ->
    #sc_room_find_response{result = Result, reason = Reason}.

%% 能否切换房间
can_change_room(_PlayerInfo) ->
    true.
    % if (PlayerInfo#player_info.touch_room_id > 0) orelse (PlayerInfo#player_info.game_room_id > 0) ->
    %     false;
    % true ->
    %     true
    % end.

