%% @author zouv
%% @doc 房间信息管理

-module(lib_room_info).

-include("common.hrl").
-include_lib("db/include/record.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
    create_room/3,
    find_room/1,
    call_enter_room/2,
    enter_room/2,
    call_player_sit/3,
    pre_player_sit_check/3,
    start_mahjong/2,
    call_player_quit/2,
    player_quit/2,

    get_sit_member_num/1,
    update_room_info/1,
    delete_room_info/1,
    update_room_member/2,
    delete_room_member/2,

    change_room_state/2,
    change_room_march/2,
    cast_delete_room_info/1,
    send_room_msg_to_member_list/2
 ]).

%% 查找房间
%%  返回有空位的房间信息
create_room(PlayerInfo, Type, LongTime) ->
    AutoRoomId =  mod_increase_id:get_room_unique_id(),
    PlayerId = PlayerInfo#player_info.id,
    NowTime = mod_time:now_seconds(),
    RRoomInfo = #ets_room_info{
                            id = AutoRoomId, 
                            owner_id = PlayerId, 
                            type = Type, 
                            create_time = NowTime, 
                            start_time = NowTime, 
                            long_time = LongTime,   %% 这个是房间的时长 
                            wait_long_time = ?ROOM_WAIT_START_TIME
                            % record_id = RecordId
                            },
    %% 创建房间的时候，直接增加一个记录
    RecordId = lib_record_process:init(PlayerInfo, RRoomInfo),
    % RecordId = CommonRecord#common_record.id,
    RRoomInfo1 = RRoomInfo#ets_room_info{record_id = RecordId},
    gen_server:cast(mod_room_info:get_mod_pid(), {'create_room_info', RRoomInfo1}), % 创建房间，此处可不用call
    RRoomInfo1.

find_room(RoomId) ->
    case ets:lookup(?ETS_ROOM_INFO, RoomId) of
        [RRoomInfo] ->
            NowTime = mod_time:now_seconds(),
            #ets_room_info{
                        start_time = StartTime,
                        long_time = LongTime
            } = RRoomInfo,
            if
                NowTime - StartTime > LongTime ->
                    {false, "房间已经解散"};
                true ->
                    {true, RRoomInfo}
            end;
        _ ->
            {false, "房间不存在"}
    end.

call_enter_room(RRoomInfo, RRoomMember) ->
    if length(RRoomInfo#ets_room_info.members) >= ?ROOM_NUM_LIMIT ->
        {false, "房间人数已满"};
    true ->
        case gen_server:call(mod_room_info:get_mod_pid(), {'player_enter_room', RRoomInfo#ets_room_info.id, RRoomMember}) of
            {true, NewRRoomInfo} ->
                {true, NewRRoomInfo};
            _ ->
                {false, "进入失败"}
        end
    end.

enter_room(RoomId, RRoomMember) ->
    [RRoomInfo] = ets:lookup(?ETS_ROOM_INFO, RoomId),
    PlayerId = RRoomMember#r_room_member.player_id,
    case lists:keyfind(PlayerId, #r_room_member.player_id, RRoomInfo#ets_room_info.members) of
        false ->
            %%io:format("enter_room___1 ~p~n", [PlayerId]),
            NewRRoomMember = RRoomMember;
        TempRRoomMember ->
            %%io:format("enter_room___2 ~p~n", [PlayerId]),
            NewRRoomMember = TempRRoomMember#r_room_member{is_leave = false}
    end,
    NewRRoomInfo = lib_room_info:update_room_member(NewRRoomMember, RRoomInfo),
    lib_room_info:update_room_info(NewRRoomInfo),
    {true, NewRRoomInfo}.

%% 玩家坐下
call_player_sit(RoomId, Position, RRoomMember) ->
    case pre_player_sit_check(RoomId, Position,RRoomMember) of
        {true, _RRoomInfo} ->
            case gen_server:call(mod_room_info:get_mod_pid(), {'player_sit', RoomId, Position, RRoomMember}) of
                {true, NewRRoomInfo} ->
                    {true, NewRRoomInfo};
                R ->
                    R
            end;
        R ->
            R
    end.

pre_player_sit_check(RoomId, Position, RRoomMember) ->
    case ets:lookup(?ETS_ROOM_INFO, RoomId) of
        [] ->
            {false, "房间不存在"};
        [RRoomInfo] ->  %% 站起还是坐下
            if 
                1 =< Position andalso Position =< 4 ->
                    case lists:keyfind(Position, #r_room_member.position, RRoomInfo#ets_room_info.members) of
                        false ->
                            PlayerId = RRoomMember#r_room_member.player_id,
                            NewRRoomInfo = RRoomInfo#ets_room_info{cost_list = lists:umerge([PlayerId], RRoomInfo#ets_room_info.cost_list)},
                            {true, NewRRoomInfo};
                        _ ->
                            {false, "该位置已被占用！"}
                    end;
                true -> %% 站起
                    {true, RRoomInfo}
            end
    end.

%% 开局
%%  返回房间进程信息
start_mahjong(RoomId, PlayerId) ->
    case pre_start_mahjong(RoomId, PlayerId) of
        {true, DataDict} ->
            RRoomInfo = dict:fetch(room_info, DataDict),
            {Pid, UniqueFlag} = mod_room_manager:get_pid(RoomId),
            %% 改变march is_start 是说正在大牌中，is_march 意思是已经点过开局
            % gen_server:cast(mod_room_info:get_mod_pid(), {'change_room_march', RoomId, true}),
            change_room_march(RoomId, true),
            %%io:format("_____start_mahjong~p~n", [{Pid, UniqueFlag, RRoomInfo}]),
            %% 开启
            lib_mahjong_fight_process:open_mahjong_fight(RRoomInfo, UniqueFlag),
            {Pid, UniqueFlag};
        R ->
            R
    end.

pre_start_mahjong(RoomId, PlayerId) ->
    CheckFlagList = [
                     check_room,
                     check_room_state
                    ],
    DataDict = dict:store(room_id, RoomId, dict:new()),
    DataDict1 = dict:store('player_id', PlayerId, DataDict),
    pre_start_mahjong_check(DataDict1, CheckFlagList).

pre_start_mahjong_check(DataDict, []) ->
    {true, DataDict};
pre_start_mahjong_check(DataDict, [check_room | L]) ->
    RoomId = dict:fetch(room_id, DataDict),
    PlayerId = dict:fetch('player_id', DataDict),
    case ets:lookup(?ETS_ROOM_INFO, RoomId) of
        [] ->
            {false, "房间不存在"};
        % [RRoomInfo] when RRoomInfo#ets_room_info.owner_id =/= PlayerId ->
        %     {false, "只有房主能点开始游戏"};
        [RRoomInfo] when RRoomInfo#ets_room_info.sit_member_num == ?ROOM_SIT_NUM_LIMIT ->
            case lists:keyfind(PlayerId, #r_room_member.player_id, RRoomInfo#ets_room_info.members) of
                RRoomMember when RRoomMember#r_room_member.position == 1 ->
                    DataDict1 = dict:store(room_info, RRoomInfo, DataDict),
                    pre_start_mahjong_check(DataDict1, L);
                _ ->
                    {false, "由位置为东的玩家，点开始"}
            end;
        _ ->
            {false, "缺少牌友"}
    end;
pre_start_mahjong_check(DataDict, [check_room_state | L]) ->
    RRoomInfo = dict:fetch(room_info, DataDict),
    %% 用is_start不合理，因为每一局结束，is_start 会变成false;is_march 是这个房间点过一次开始，就是true，不能重复点，影响战绩记录
    #ets_room_info{
                is_start = IsStart,
                is_march = IsMarch,
                create_time = CreateTime,
                start_time = StartTime,
                long_time = LongTime,
                wait_long_time = WaitLongTime
    } = RRoomInfo,
    NowTime = mod_time:now_seconds(),
    DataDict1 = dict:store(is_march, IsMarch, DataDict),
    if
        IsStart ->
            {false, "房间已经开始游戏"};
        IsMarch ->
            {false, "房主已经点过开始"};
        NowTime - StartTime > LongTime ->
            {false, "房间开始过，房间战斗时间已结束"};
        %% 现在已经是时间已过，就会把房间删除 所以不需要特别判断
        % IsMarch -> %% 开始过，但战斗时间还有，可以重新点开始
        %     pre_start_mahjong_check(DataDict1, L);
        % NowTime - CreateTime > WaitLongTime -> %% 没开始过，同时房间等待时间已过
        %     {false, "房间已经超过等待时间"};
        true ->
            NewRRoomInfo = RRoomInfo#ets_room_info{
                                            % is_march = true,
                                            start_time = NowTime,
                                            wait_long_time = 0
            },
            DataDict2 = dict:store('room_info', NewRRoomInfo, DataDict1),
            pre_start_mahjong_check(DataDict2, L)
    end.
%% 离开房间
call_player_quit(RoomId, PlayerId) ->
    gen_server:call(mod_room_info:get_mod_pid(), {'player_quit', RoomId, PlayerId}).

player_quit(RoomId, PlayerId) ->
    case ets:lookup(?ETS_ROOM_INFO, RoomId) of
        [RRoomInfo] ->
            %%io:format("player_quit_mod___1 ~p~n", [{RoomId, PlayerId, RRoomInfo#ets_room_info.is_start}]),
            if RRoomInfo#ets_room_info.is_start == true ->
                NewRRoomInfo = player_quit1(RRoomInfo, PlayerId);
            true ->
                NewRRoomInfo = delete_room_member(PlayerId, RRoomInfo)
            end,
            %%io:format("player_quit_mod___2 ~p~n", [NewRRoomInfo]),
            lib_room_info:update_room_info(NewRRoomInfo),
            lib_room:send_all_room_update(NewRRoomInfo),
            {true, NewRRoomInfo};
        _ ->
            {false, "房间不存在"}
    end.

player_quit1(RRoomInfo, PlayerId) ->
    case lists:keyfind(PlayerId, #r_room_member.player_id, RRoomInfo#ets_room_info.members) of
        false ->
            logger:warning_msg("player quit1 ERROR! ~p~n", [{RRoomInfo, PlayerId}]),
            RRoomInfo;
        RRoomMember ->
            NewRRoomMember = RRoomMember#r_room_member{is_leave = true},
            update_room_member(NewRRoomMember, RRoomInfo)
    end.

%% 获取坐下的玩家数量
get_sit_member_num(Members) ->
    lists:foldl(fun(E, Acc) ->
        ?CONDITIONAL_EXPRESSION(E#r_room_member.position > 0, Acc + 1, Acc)
    end,
    0,
    Members).

update_room_info(NewRRoomInfo) ->
    %%io:format("update_room_info____ ~p~n", [NewRRoomInfo]),
    ets:insert(?ETS_ROOM_INFO, NewRRoomInfo).

delete_room_info(RoomId) ->
    %%io:format("delete_room_info____ ~p~n", [RoomId]),
    ets:delete(?ETS_ROOM_INFO, RoomId).

update_room_member(RRoomMember, RRoomInfo) ->
    Members = RRoomInfo#ets_room_info.members,
    NewMembers = lists:keystore(RRoomMember#r_room_member.player_id, #r_room_member.player_id, Members, RRoomMember),
    MemberNum = get_sit_member_num(NewMembers),
    RRoomInfo#ets_room_info{members = NewMembers, sit_member_num = MemberNum}.

delete_room_member(PlayerId, RRoomInfo) ->
    Members = RRoomInfo#ets_room_info.members,
    NewMembers = lists:keydelete(PlayerId, #r_room_member.player_id, Members),
    MemberNum = get_sit_member_num(NewMembers),
    RRoomInfo#ets_room_info{members = NewMembers, sit_member_num = MemberNum}.    
%% 清理房间
cast_delete_room_info(RoomId) ->
     gen_server:cast(mod_room_info:get_mod_pid(), {'clean_room', RoomId}).
%% 改变房间游戏状态
change_room_state(RoomId, IsStart) ->
    if IsStart ->
        GameRoomId = RoomId;
    true ->
        GameRoomId = 0
    end,
    gen_server:cast(mod_room_info:get_mod_pid(), {'change_room_state', RoomId, GameRoomId, IsStart}).
%% 只用在开局start_mahjong
change_room_march(RoomId, IsMarch) ->
    gen_server:cast(mod_room_info:get_mod_pid(), {'change_room_march', RoomId, IsMarch}).



%% 房间消息发送
send_room_msg_to_member_list([], _Msg) -> ok;
send_room_msg_to_member_list([Member | Left], Msg) when is_record(Member, r_room_member) ->
    lib_gate:send_data(Member#r_room_member.gate_pid, Msg),
    send_room_msg_to_member_list(Left, Msg);
send_room_msg_to_member_list([MemberId | Left], Msg) ->
    case ets:lookup(?ETS_ONLINE, MemberId) of
        [EtsOnline] ->
            tcp_client:send_data(EtsOnline#ets_online.gate_pid, Msg);
        _ ->
            ok
    end,
    send_room_msg_to_member_list(Left, Msg).

