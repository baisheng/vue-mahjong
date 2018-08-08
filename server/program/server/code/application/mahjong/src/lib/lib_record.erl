%% @author icolun
%% @doc 记录相关

-module(lib_record).

-include("common.hrl").
-include_lib("db/include/record.hrl").
-include_lib("protob/include/p05_common_pb.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		cs_query_game_statistics/0,
		cs_query_game_record/1,
		cs_query_game_find/0,

		send/0
		]).

send() ->
	cs_query_game_statistics(),
	cs_query_game_record(0),
	cs_query_game_find(),
	ok.
% 查看记录
cs_query_game_statistics() ->
	% PlayerMahjongFight = lib_mahjong_fight_player:get_dic_player_mahjong_fight_info(),
	% #player_mahjong_fight{
	% 					score = Score,
	% 					count = Count,
	% 					total_count = TotalCount
	% 					% recent_list = RecentList
	% } = PlayerMahjongFight,
	% %%io:format("___cs_query_game_statistics~p~n",[PlayerMahjongFight]),
	%% 因为现在只有一种房间，先特殊了
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	#player_info{
				total_count = TotalCount,
				count = Count
	} = PlayerInfo,
	PList= make_pb_game_statistics_record(),

	Msg = #sc_query_game_statistics_response{
							alltimes = TotalCount,
							allwinprob = util_math:keep_point_round(Count/max(1,TotalCount), 2),
							list = PList
	},
	GatePid = lib_player_procdict:get_dic_gate_pid(),
	%%io:format("____-lib_record~p~n",[Msg]),
	lib_gate:send_data(GatePid, Msg).	

make_pb_game_statistics_record() ->
	make_pb_game_statistics_record(?ROOM_TYPE_LIST, []).

make_pb_game_statistics_record([], PList) ->
	PList;
make_pb_game_statistics_record([Type|List], PList) ->
	Pb = make_pb_game_statistics_record(Type),
	make_pb_game_statistics_record(List, [Pb|PList]).

make_pb_game_statistics_record(?ROOM_TYPE_1) ->
	PlayerMahjongFight = lib_mahjong_fight_player:get_dic_player_mahjong_fight_info(),
	#player_mahjong_fight{
						% score = Score,
						count = Count,
						total_count = TotalCount
	} = PlayerMahjongFight,
	#pb_game_statistics_record{
								type = ?ROOM_TYPE_1,
								totaltimes = TotalCount,
								winprob = util_math:keep_point_round(Count/max(1,TotalCount), 2)
	}.


	
cs_query_game_record(RecordId) when RecordId > 0 -> %% 指定玩家记录
	case lib_record_process:get_common_record(RecordId) of
		[] ->
			PList = [];
		Record ->
			GameRecord = pack_pb_game_record(Record),
			RecordMember = pack_pb_game_player_info_list(Record#common_record.info),
			Pb = #pb_game_record_info{
									info = GameRecord,
									members = RecordMember
			},
			PList = [Pb]
	end,
	Msg = #sc_query_game_record_response{
									recordid = RecordId,
									list = PList
	},
	%%io:format("____-lib_record~p~n",[Msg]),
	GatePid = lib_player_procdict:get_dic_gate_pid(),
	lib_gate:send_data(GatePid, Msg);	

cs_query_game_record(RecordId) ->
	PlayerMahjongFight = lib_mahjong_fight_player:get_dic_player_mahjong_fight_info(),
	#player_mahjong_fight{
		open_list = OpenList,
		join_list = JoinList
	} = PlayerMahjongFight,
	List = lists:ukeysort(2, OpenList ++ JoinList),
	F = fun({EId, _}, Acc) ->
			case lib_record_process:get_common_record(EId) of
				[] ->
					Acc;
				E ->
					GameRecord = pack_pb_game_record(E),
					RecordMember = pack_pb_game_player_info_list(E#common_record.info),
					Pb = #pb_game_record_info{
											info = GameRecord,
											members = RecordMember
					},
					[Pb|Acc]
			end
		end,
	PList = lists:foldl(F, [], List),
	Msg = #sc_query_game_record_response{
									recordid = RecordId,
									list = PList
	},
	%%io:format("____-lib_record~p~n",[Msg]),
	GatePid = lib_player_procdict:get_dic_gate_pid(),
	lib_gate:send_data(GatePid, Msg).	

%% 只要正在进行的，和正在参与的
cs_query_game_find() ->
	PlayerMahjongFight = lib_mahjong_fight_player:get_dic_player_mahjong_fight_info(),
	#player_mahjong_fight{
						open_list = OpenList,
						join_list = JoinList
	} = PlayerMahjongFight,
	NowTime = mod_time:now_seconds(),
	F = fun({EId, _}, Acc) ->
			case lib_record_process:get_common_record(EId) of
				[] ->
					Acc;
				E ->
					case ets:lookup(?ETS_ROOM_INFO, E#common_record.room_id) of %% 该房间还是之前房间
						[RRoomInfo] when RRoomInfo#ets_room_info.record_id == EId
						        andalso NowTime - RRoomInfo#ets_room_info.start_time =< RRoomInfo#ets_room_info.long_time ->
							GameRecord = pack_pb_game_record(E),
							[GameRecord|Acc];
						_ ->
							Acc
					end
			end
		end,
	My = lists:foldl(F, [], OpenList),
	Other = lists:foldl(F, [], JoinList),
	Msg = #sc_query_game_find_response{
								my = My,
								other = Other
	},
	%%io:format("____-lib_record~p~n",[Msg]),
	GatePid = lib_player_procdict:get_dic_gate_pid(),
	lib_gate:send_data(GatePid, Msg).	






pack_pb_game_record(CommonRecord) ->
	#common_record{
			id = RecordId,
			type = Type,
			owner_id = OwnerId,
			room_id = RoomId,
			total_time = TotalTime,
			start_time = StartTime,
		    end_time = EndTime
			% info = Info
	} = CommonRecord,
	#pb_game_record{
				id = RecordId,
				type = Type,
				ownerid = OwnerId,
				roomid = RoomId,
				totaltime = TotalTime,
				starttime = StartTime,
				endtime = EndTime
	}.
pack_pb_game_player_info_list(MemberList) ->
	pack_pb_game_player_info_list(MemberList, []).
pack_pb_game_player_info_list([], PbMemberList) ->
	PbMemberList;
pack_pb_game_player_info_list([E|MemberList], PbMemberList) ->
	Pb = pack_pb_game_player_info(E),
	pack_pb_game_player_info_list(MemberList, [Pb|PbMemberList]).

pack_pb_game_player_info(RecordMember) ->
	#r_record_member{
				player_id = PlayerId,
          		player_name = PlayerName,
          		player_icon = PlayerIcon,
          		win_flag = WinFlag,
          		score = Score
	} = RecordMember,
	#pb_game_player_info{
					playerid = PlayerId,
					name = PlayerName,
					icon = PlayerIcon,
					winflag = WinFlag,
					score = erlang:abs(Score) 
	}.