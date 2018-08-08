%%麻将进入场景 与退出场景等
-module(lib_mahjong_fight).
-include("mahjong_fight.hrl").
-include_lib("db/include/record.hrl").
-include_lib("protob/include/p03_mahjong_fight_pb.hrl").


% -compile(export_all).
-export([
		enter_battle/2,
		enter_battle/3,
		enter_room_battle/5,

		quit_battle/0,
		quit_battle/1,
		quit_room_battle/1
		]).

%% 切换房间战斗 
%% 玩家进程
enter_battle(PlayerInfo, RRoomInfo) ->
	#ets_room_info{
				members = Members,
				id = RoomId,
				is_march = IsMarch
	} = RRoomInfo,
	PlayerId = PlayerInfo#player_info.id, 
	if
		IsMarch ->
			case lists:keyfind(PlayerId, #r_room_member.player_id, Members) of
				false ->
					skip;
				Member ->
					Position = Member#r_room_member.position,
					enter_battle(PlayerInfo, RoomId, Position)
			end;
		true ->
			skip
	end.
enter_battle(PlayerInfo, RoomId, Position) ->
    {RoomPid, UniqueFlag} = mod_room_manager:get_pid(RoomId),
    %% 更新玩家信息
    NewPlayerInfo = PlayerInfo#player_info{room_unique_flag = UniqueFlag},
    lib_player:save_player_info(NewPlayerInfo),
   
	%% 进入房间战斗
    GatePid = lib_player_procdict:get_dic_gate_pid(),
    gen_server:cast(RoomPid, {'handle', ?MODULE, enter_room_battle, [PlayerInfo, self(), GatePid, RoomId, Position]}),
    ok.

%% 进入战斗牌局
%% 房间进程 
enter_room_battle(PlayerInfo, PlayerPid, GatePid, RoomId, Position) ->
	%% 因为有上一局结束后新进来的
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	IsEnd = MahjongState#r_mahjong_fight_state.is_end,
	enter_room_battle(IsEnd, PlayerInfo,PlayerPid, GatePid, RoomId, Position).

%% 如果已经结束位置可以替换，否则拒绝 站起就是要观战
enter_room_battle(true, PlayerInfo,PlayerPid, GatePid, RoomId, Position) when not ?IS_POSITION(Position) ->
	#player_info{
				id = PlayerId,
				name = PlayerName,
				icon = PlayerIcon
	} = PlayerInfo,
	WatchPlayer = #mahjong_fight_info{
								player_id = PlayerId, 
								player_pid = PlayerPid,
								gate_pid = GatePid,
								room_id = RoomId,
								player_name = PlayerName,
								player_icon = PlayerIcon
					},
	add_watch_info(WatchPlayer);
enter_room_battle(true, PlayerInfo,PlayerPid, GatePid, RoomId, Position) ->
	#player_info{
				id = PlayerId,
				name = PlayerName,
				icon = PlayerIcon
	} = PlayerInfo,
	case lib_mahjong_fight_procdict:get_mahjong_by_player_id(PlayerId) of
		MahjongPlayer when is_record(MahjongPlayer, mahjong_fight_info) ->
			NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{
													player_id = PlayerId, 
													position = Position,
													player_pid = PlayerPid,
													gate_pid = GatePid,
													room_id = RoomId,
													player_name = PlayerName,
													player_icon = PlayerIcon,
													state = ?STATE_IN
			},
			add_battle_info(NewMahjongPlayer),
			%% 主动准备
			lib_mahjong_fight_process:mahjong_prepare(PlayerId);
		_ ->
			case lib_mahjong_fight_procdict:get_mahjong_by_position(Position) of
				false ->
					NewMahjongPlayer = #mahjong_fight_info{
													player_id = PlayerId, 
													player_pid = PlayerPid,
													gate_pid = GatePid,
													room_id = RoomId,
													position = Position,
													player_name = PlayerName,
													player_icon = PlayerIcon,
													state = ?STATE_IN
					},
					add_battle_info(NewMahjongPlayer),
					%% 主动准备
					lib_mahjong_fight_process:mahjong_prepare(PlayerId);
				_ ->
					WatchPlayer = #mahjong_fight_info{
													player_id = PlayerId, 
													player_pid = PlayerPid,
													gate_pid = GatePid,
													room_id = RoomId,
													player_name = PlayerName,
													player_icon = PlayerIcon
					},
					add_watch_info(WatchPlayer)
			end
	end;
%% 游戏进行中，只能是再桌的人update
enter_room_battle(false, PlayerInfo,PlayerPid, GatePid, RoomId, _Position) ->
	#player_info{
				id = PlayerId,
				name = PlayerName,
				icon = PlayerIcon
	} = PlayerInfo,
	case lib_mahjong_fight_procdict:get_mahjong_by_player_id(PlayerId) of
		MahjongPlayer when is_record(MahjongPlayer, mahjong_fight_info) ->
			NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{
													player_id = PlayerId, 
													player_pid = PlayerPid,
													gate_pid = GatePid,
													room_id = RoomId,
													player_name = PlayerName,
													player_icon = PlayerIcon,
													state = ?STATE_BEING
			},
			add_battle_info(NewMahjongPlayer);
			%% 先update 再取消定时
			% lib_mahjong_fight_process:cancel_mahjong_fight_ref(MahjongPlayer#mahjong_fight_info.re_ref);
		_ ->
			WatchPlayer = #mahjong_fight_info{
											player_id = PlayerId, 
											player_pid = PlayerPid,
											gate_pid = GatePid,
											room_id = RoomId,
											player_name = PlayerName,
											player_icon = PlayerIcon
			},
			add_watch_info(WatchPlayer)
	end.


%% 进入牌局
add_battle_info(MahjongPlayer) ->
	%% 重连之后，有可能gate_id 改变，之后有可能还有一种需求，如果玩家下线重新上之后，刚才牌局没结束，退出
	lib_mahjong_fight_procdict:delete_mahjong_fight_watch(MahjongPlayer#mahjong_fight_info.player_id),
	lib_mahjong_fight_procdict:update_mahjong_by_player_id(MahjongPlayer),
	% lib_mahjong_fight_msg:send_mahjong_fight_update(MahjongPlayer),
	lib_mahjong_fight_msg:send_mahjong_fight_state(MahjongPlayer#mahjong_fight_info.gate_pid),
	lib_mahjong_fight_msg:send_mahjong_fight_record(MahjongPlayer#mahjong_fight_info.gate_pid),
	lib_mahjong_fight_msg:send_mahjong_fight_left_time(),
	%% 推送重连玩家信息
	% lib_mahjong_fight_msg:send_mahjong_fight_info(MahjongPlayer),
	lib_mahjong_fight_process:cancel_mahjong_fight_ref(MahjongPlayer#mahjong_fight_info.re_ref),
	% %% 主动准备
	% lib_mahjong_fight_process:mahjong_prepare(PlayerId),
	ok.
%% 进入观战
add_watch_info(WatchPlayer) ->
	WatchPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_fight_watch_list(),
	if
		length(WatchPlayerList) >= 10 ->
			skip;
		true ->
			%% 假如是站起的要删除旧战斗信息
			lib_mahjong_fight_procdict:delete_mahjong_by_player_id(WatchPlayer#mahjong_fight_info.player_id),
			lib_mahjong_fight_procdict:update_mahjong_fight_watch(WatchPlayer),
			lib_mahjong_fight_msg:send_mahjong_fight_update(WatchPlayer),
			lib_mahjong_fight_msg:send_mahjong_fight_state(WatchPlayer#mahjong_fight_info.gate_pid),
			lib_mahjong_fight_msg:send_mahjong_fight_record(WatchPlayer#mahjong_fight_info.gate_pid),
			lib_mahjong_fight_msg:send_mahjong_fight_left_time()
	end.
	
%% 退出战斗
quit_battle() ->
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	quit_battle(PlayerInfo).

quit_battle(PlayerInfo) ->
	%% 退出玩家
	#player_info{
				id = PlayerId,
				room_unique_flag = UniqueFlag
			} = PlayerInfo,
	NewPlayerInfo = PlayerInfo#player_info{room_unique_flag = ""},
	lib_player:save_player_info(NewPlayerInfo),
	%% 检查如果还在其他房间，那做退出处理
    case mod_room_manager:get_pid_only(UniqueFlag) of
		RoomPid when is_pid(RoomPid) ->
			gen_server:cast(RoomPid, {'handle', ?MODULE, quit_room_battle, [PlayerId]}),
			ok;
		_ ->
			skip
	end,
	ok.

%% 房间进程
quit_room_battle(PlayerId) ->
	%% 这个只处理了属于战斗人员，因为牌局正在进行不能删除战斗人员的牌信息
	lib_mahjong_fight_process:mahjong_quit(PlayerId),
	%% 如果是观战人员，直接删除观战人员信息
	lib_mahjong_fight_procdict:delete_mahjong_fight_watch(PlayerId),
	%% 后续
	ok.
