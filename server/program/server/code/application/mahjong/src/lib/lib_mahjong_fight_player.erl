%% @icolun
%% 麻将战斗玩家进程处理
-module(lib_mahjong_fight_player).
-include("mahjong_fight.hrl").
-include_lib("db/include/record.hrl").
-include_lib("protob/include/p03_mahjong_fight_pb.hrl").

-export([
		get_dic_player_mahjong_fight_info/0,
		update_dic_player_mahjong_fight_info/1,

		init_player_mahjong_fight/1,
		save_player_mahjong_fight/0,

		update_open_record/1,                        %% 玩家身上存储记录id,可清，限制20，但实际记录会一直在

		notice_mahjong_fight_start/6,
		notice_mahjong_fight_end/4,

		cs_mahjong_fight_die/0,
		cs_mahjong_fight_play/1,
		cs_mahjong_fight_meld/1,
		cs_mahjong_fight_win/0,
		cs_mahjong_fight_cancel/0,
		cs_mahjong_fight_ready/1,
		cs_mahjong_fight_prepare/0,
		cs_mahjong_fight_update/0

		]).

-define(DIC_PLAYER_MAHJONG_FIGHT_INFO,     dic_player_mahjong_fight_info).
%% 进程字典相关
get_dic_player_mahjong_fight_info() ->
	case get(?DIC_PLAYER_MAHJONG_FIGHT_INFO) of
		?UNDEFINED ->
			#player_mahjong_fight{};
		Info ->
			Info
	end.
update_dic_player_mahjong_fight_info(Info) ->
	case Info#player_mahjong_fight.other of
		?DIRTY_STATE_NEW = NewDirtyState ->
			ok;
		_ ->
			NewDirtyState = ?DIRTY_STATE_UPDATE
	end,
	NewInfo = Info#player_mahjong_fight{other = NewDirtyState},
	put(?DIC_PLAYER_MAHJONG_FIGHT_INFO, NewInfo),
	save_player_mahjong_fight().

init_player_mahjong_fight(PlayerId) ->
	case db_agent_player_mahjong_fight:get(PlayerId) of
		[] ->
			Info = #player_mahjong_fight{
									player_id = PlayerId,
									other = ?DIRTY_STATE_NEW
			};
		[DbInfo] ->
			F = fun(E, Acc) ->
					case E of
						{_A, _B} ->
							[E|Acc];
						_ ->
							Acc
					end
				end,
			OpenList = lists:foldl(F, [], DbInfo#player_mahjong_fight.open_list),
			JoinList = lists:foldl(F, [], DbInfo#player_mahjong_fight.join_list),
			Info = DbInfo#player_mahjong_fight{
											open_list = init_player_mahjong_fight_check(OpenList),
											join_list = init_player_mahjong_fight_check(JoinList),
											other = ?DIRTY_STATE_NULL}
	end,
	put(?DIC_PLAYER_MAHJONG_FIGHT_INFO, Info).  

init_player_mahjong_fight_check(List) ->
	NowTime = mod_time:now_seconds(),
	F = fun({_, ETime} = E, Acc) ->
				DiffDay = util:get_diff_days(NowTime, ETime),
				if
					DiffDay < ?MAHJONG_RECORD_VAILD ->
						[E|Acc];
					true ->
						Acc
				end
			end,
	lists:foldl(F, [], List).
save_player_mahjong_fight() ->
	Info = get_dic_player_mahjong_fight_info(),
	case Info#player_mahjong_fight.other of
		?DIRTY_STATE_NULL ->
			skip;
		_ ->
			db_agent_player_mahjong_fight:update(Info)
	end,
	NewInfo = Info#player_mahjong_fight{other = ?DIRTY_STATE_NULL},
	put(?DIC_PLAYER_MAHJONG_FIGHT_INFO, NewInfo).
%% 增加开房的记录 主要是因为他们设计房间id,可能重开服后不唯一，所以发现等
%%需要记录唯一id,所以房主直接写入，所以也保存记录
update_open_record(RRoomInfo) ->
	PlayerMahjongFight = get_dic_player_mahjong_fight_info(),
	#ets_room_info{
				record_id = RecordId,
				owner_id = OwnerId,       % 房主Id
    			create_time = StartTime
	} = RRoomInfo,
	#player_mahjong_fight{
					player_id = PlayerId,
					open_list = OpenList
	} = PlayerMahjongFight,
	case OwnerId == PlayerId of
		true ->
			case lists:keyfind(RecordId, 1, OpenList) of
				false ->
					NewOpenList = [{RecordId, StartTime}|OpenList],
					NewPlayerMahjongFight = PlayerMahjongFight#player_mahjong_fight{open_list = NewOpenList},
					update_dic_player_mahjong_fight_info(NewPlayerMahjongFight);
				_ ->
					skip
			end;
		_ ->
			skip
	end.
%% 开打后 通知玩家保存房间唯一标识，用于中间收到请求通知房间进程
notice_mahjong_fight_start(RecordId, RoomId, OwnerId,Position,NowTime,_UniqueFlag) ->
	%%io:format("_____notice_mahjong_fight_start~p~n",[_UniqueFlag]),
	%% 暂时只做保存标志
	% update_dic_player_mahjong_fight_flag(UniqueFlag).
	PlayerMahjongFight = get_dic_player_mahjong_fight_info(),
	%% 改成存在playerinfo 里面
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	%% 统一单独进入，为后面重连进入做准备，同时获取更新的gate_pid, player_pid
	lib_mahjong_fight:enter_battle(PlayerInfo, RoomId, Position),
	%% 增加统计
	%% 记录我开的局或者参加的局
	Record = {RecordId, NowTime},
	if
		?IS_POSITION(Position) andalso PlayerInfo#player_info.id == OwnerId -> %% 开的局
			%% 一般会 最多记100条
			NewOpenList = lists:keystore(RecordId, 1, PlayerMahjongFight#player_mahjong_fight.open_list, Record),
			NewPlayerMahjongFight = PlayerMahjongFight#player_mahjong_fight{open_list = NewOpenList},
			update_dic_player_mahjong_fight_info(NewPlayerMahjongFight);
		?IS_POSITION(Position) ->
			%% 一般会 最多100条
			NewJoinList = lists:keystore(RecordId, 1, PlayerMahjongFight#player_mahjong_fight.join_list, Record),
			NewPlayerMahjongFight = PlayerMahjongFight#player_mahjong_fight{join_list = NewJoinList},
			update_dic_player_mahjong_fight_info(NewPlayerMahjongFight);
		true ->
			skip %% 观战人员
	end,
	ok.

%% 掷骰子请求
cs_mahjong_fight_die() ->
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	case mod_room_manager:get_pid_only(PlayerInfo#player_info.room_unique_flag) of
		RoomPid when is_pid(RoomPid) ->
			PlayerInfo = lib_player_procdict:get_dic_player_info(),
			gen_server:cast(RoomPid, {'handle', lib_mahjong_fight_process, mahjong_die, [PlayerInfo#player_info.id]}),
			ok;
		_ ->
			% skip
			ReplyMsg = #sc_mahjong_fight_die{ result = false, reason = "牌局没在进行中"},
			lib_mahjong_fight_msg:send_mahjong_fight_reply(ReplyMsg)
	end.
%% 出牌
cs_mahjong_fight_play(MahjongId) ->
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	case mod_room_manager:get_pid_only(PlayerInfo#player_info.room_unique_flag) of
		RoomPid when is_pid(RoomPid) ->
			PlayerInfo = lib_player_procdict:get_dic_player_info(),
			gen_server:cast(RoomPid, {'handle', lib_mahjong_fight_process, mahjong_play, [PlayerInfo#player_info.id, MahjongId]}),
			ok;
		_ ->
			% skip
			ReplyMsg = #sc_mahjong_fight_play{ result = false, reason = "牌局没在进行中"},
			lib_mahjong_fight_msg:send_mahjong_fight_reply(ReplyMsg)
			% lib_mahjong_fight_msg:send_mahjong_fight_reply(false)
	end.

cs_mahjong_fight_meld(PbMeld) -> 
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	case mod_room_manager:get_pid_only(PlayerInfo#player_info.room_unique_flag) of
		RoomPid when is_pid(RoomPid) ->
			PlayerInfo = lib_player_procdict:get_dic_player_info(),
			gen_server:cast(RoomPid, {'handle', lib_mahjong_fight_process, mahjong_meld, [PlayerInfo#player_info.id,PbMeld]}),
			ok;
		_ ->
			ReplyMsg = #sc_mahjong_fight_meld{ result = false, reason = "牌局没在进行中"},
			lib_mahjong_fight_msg:send_mahjong_fight_reply(ReplyMsg)
			% lib_mahjong_fight_msg:send_mahjong_fight_reply(false)
	end.
%% 胡牌请求
cs_mahjong_fight_win() ->
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	case mod_room_manager:get_pid_only(PlayerInfo#player_info.room_unique_flag) of
		RoomPid when is_pid(RoomPid) ->
			PlayerInfo = lib_player_procdict:get_dic_player_info(),
			gen_server:cast(RoomPid, {'handle', lib_mahjong_fight_process, mahjong_win, [PlayerInfo#player_info.id]}),
			ok;
		_ ->
			% skip
			ReplyMsg = #sc_mahjong_fight_win{ result = false, reason = "牌局没在进行中"},
			lib_mahjong_fight_msg:send_mahjong_fight_reply(ReplyMsg)
			% lib_mahjong_fight_msg:send_mahjong_fight_reply(false)
	end.
%% 
cs_mahjong_fight_cancel() ->
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	case mod_room_manager:get_pid_only(PlayerInfo#player_info.room_unique_flag) of
		RoomPid when is_pid(RoomPid) ->
			PlayerInfo = lib_player_procdict:get_dic_player_info(),
			gen_server:cast(RoomPid, {'handle', lib_mahjong_fight_process, mahjong_cancel, [PlayerInfo#player_info.id]}),
			ok;
		_ ->
			ReplyMsg = #sc_mahjong_fight_cancel{ result = false, reason = "牌局没在进行中"},
			lib_mahjong_fight_msg:send_mahjong_fight_reply(ReplyMsg)
			% lib_mahjong_fight_msg:send_mahjong_fight_reply(false)
	end.
%% 
cs_mahjong_fight_ready(MahjongId) ->
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	case mod_room_manager:get_pid_only(PlayerInfo#player_info.room_unique_flag) of
		RoomPid when is_pid(RoomPid) ->
			PlayerInfo = lib_player_procdict:get_dic_player_info(),
			gen_server:cast(RoomPid, {'handle', lib_mahjong_fight_process, mahjong_ready, [PlayerInfo#player_info.id, MahjongId]}),
			ok;
		_ ->
			ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_ready, false, "牌局没有在进行中"),
			lib_mahjong_fight_msg:send_mahjong_fight_reply(ReplyMsg)
			% lib_mahjong_fight_msg:send_mahjong_fight_reply(false)
	end.
cs_mahjong_fight_prepare() ->
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	case mod_room_manager:get_pid_only(PlayerInfo#player_info.room_unique_flag) of
		RoomPid when is_pid(RoomPid) ->
			PlayerInfo = lib_player_procdict:get_dic_player_info(),
			gen_server:cast(RoomPid, {'handle', lib_mahjong_fight_process, mahjong_prepare, [PlayerInfo#player_info.id]}),
			ok;
		_ ->
			ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_prepare, false, "牌局没有在进行中"),
			lib_mahjong_fight_msg:send_mahjong_fight_reply(ReplyMsg)
			% lib_mahjong_fight_msg:send_mahjong_fight_reply(false)
	end.

notice_mahjong_fight_end(RecordId, OwnerId, Flag, Score) ->
	%%io:format("_____notice_mahjong_fight_end~p~n",[{Flag}]),
	PlayerMahjongFight = get_dic_player_mahjong_fight_info(),
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	#player_info{
				count = WinCount,
				total_count = AllTotalCount,
				score = AllScore
	} = PlayerInfo,
	NowTime = mod_time:now_seconds(),
	#player_mahjong_fight{
					score = OldScore,
					count = OldCount,
					total_count = OldTotalCount,
					% recent_list = OldRecentList,
					open_list = OldOpenList,
					join_list = OldJoinList
	} = PlayerMahjongFight,
	NowTime = mod_time:now_seconds(),
	if
		Flag == ?FLAG_WINED -> %% 玩家赢了
			NewWinCount = WinCount + 1,
			NewCount = OldCount + 1;
		true ->
			NewWinCount = WinCount,
			NewCount = OldCount
	end,
	NewAllTotalCount = AllTotalCount + 1,
	NewTotalCount = OldTotalCount + 1,
	if
		OwnerId == PlayerInfo#player_info.id ->
			case lists:keyfind(RecordId, 1, OldOpenList) of
				false ->
					NewOpenList = [{RecordId, NowTime}|OldOpenList],
					NewJoinList = OldJoinList;
				_ ->
					NewOpenList = OldOpenList,
					NewJoinList = OldJoinList
			end;
		true ->
			case lists:keyfind(RecordId, 1, OldJoinList) of
				false ->
					NewOpenList =OldOpenList,
					NewJoinList = [{RecordId, NowTime}|OldJoinList];
				_ ->
					NewOpenList = OldOpenList,
					NewJoinList = OldJoinList
			end
	end,
	NewScore = max(0, OldScore + Score),
	NewAllScore = max(0, AllScore + Score),
	%% 如果是最近记录超过10条，那把最旧的数据删除
	
	NewPlayerMahjongFight = PlayerMahjongFight#player_mahjong_fight{
										score = NewScore,
										count = NewCount,
										total_count = NewTotalCount,
										% recent_list = NewRecentList
										open_list = NewOpenList,
										join_list = NewJoinList
	},
	update_dic_player_mahjong_fight_info(NewPlayerMahjongFight),
	NewPlayerInfo = PlayerInfo#player_info{
										score = NewAllScore,
										count = NewWinCount,
										total_count = NewAllTotalCount
	},
	lib_player_procdict:update_dic_player_info(NewPlayerInfo),
	ok.
%% 
cs_mahjong_fight_update() ->
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	case mod_room_manager:get_pid_only(PlayerInfo#player_info.room_unique_flag) of
		RoomPid when is_pid(RoomPid) ->
			PlayerInfo = lib_player_procdict:get_dic_player_info(),
			gen_server:cast(RoomPid, {'handle', lib_mahjong_fight_process, mahjong_update, [PlayerInfo#player_info.id]}),
			ok;
		_ ->
			skip
			% ReplyMsg = #sc_mahjong_fight_cancel{ result = false, reason = "牌局没在进行中"},
			% lib_mahjong_fight_msg:send_mahjong_fight_reply(ReplyMsg)
			% % lib_mahjong_fight_msg:send_mahjong_fight_reply(false)
	end.




% %% 客户端
% cs_mahjong_fight_record() ->
% 	ok.
	% PlayerMahjongFight = get_dic_player_mahjong_fight_info(),
	% #player_mahjong_fight{
	% 					score = Score,
	% 					count = Count,
	% 					total_count = TotalCount,
	% 					recent_list = RecentList
	% } = PlayerMahjongFight,

	% %% 记录
	% F = fun({EFlag, EScore, ETime}) ->
	% 		#pb_mahjong_fight_record{
	% 							flag = EFlag,
	% 							score = abs(EScore),
	% 							time = ETime
	% 		}
	% 	end,
	% PbRecent = lists:map(F, RecentList),
	% %% 
	% Msg = #sc_mahjong_fight_record{
	% 						score = Score,
	% 						count = Count,
	% 						totalcount = TotalCount,
	% 						list = PbRecent
	% },
	% GatePid = lib_player_procdict:get_dic_gate_pid(),
	% lib_gate:send_data(GatePid, Msg).	
