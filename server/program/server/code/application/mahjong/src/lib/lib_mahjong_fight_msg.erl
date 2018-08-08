%% @icolun
%% 麻将战斗消息
-module(lib_mahjong_fight_msg).
-include("mahjong_fight.hrl").
-include_lib("db/include/record.hrl").
-include_lib("protob/include/p03_mahjong_fight_pb.hrl").

-export([
		send_to_pid/2,
		send_to_list/1,
		send_to_list/2,

		send_mahjong_fight_update/1,        %% 强同步
		send_mahjong_fight_info_to_self/1,  %% 发送战斗信息给自己
		send_mahjong_fight_info/0,			%% 广播战斗信息
		send_mahjong_fight_info/1,
		send_mahjong_fight_state/0,			%% 广播牌局信息
		send_mahjong_fight_state/1,
		send_mahjong_fight_left_time/0,		%% 广播剩余时间
		send_mahjong_fight_left_time/3,
		send_mahjong_fight_record/0,		%% 广播战绩记录
		send_mahjong_fight_record/1,
		send_mahjong_fight_reply/1,			%% 返回信息
		send_mahjong_fight_reply/2,
		send_mahjong_fight_end/0,           %% 结算
		send_mahjong_fight_end/1,


		pack_pb_mahjong_fight_info_list/1,
		pack_pb_mahjong_fight_info_list/2,
		pack_pb_mahjong_fight_info/1,		%% 战斗信息
		pack_pb_mahjong_fight_state/1,		%% 牌局信息
		pack_pb_mahjong_fight_reply/3,	    %% 返回
		pack_sc_mahjong_fight_end/2         %% 结算


		]).
% -compile(export_all).

%% 观战人员函数区分开来，以防混淆 而且只会发送四个玩牌玩家的战斗信息，所以更应该区分开
%% 强同步 强同步的关键是，只把信息发给指定玩家
send_mahjong_fight_update(false) ->
	skip;
send_mahjong_fight_update(MahjongPlayer) when is_record(MahjongPlayer, mahjong_fight_info) ->
	send_mahjong_fight_update([MahjongPlayer]);
send_mahjong_fight_update(PlayerList) ->
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	PList = pack_pb_mahjong_fight_info_list(MahjongPlayerList), 
	Msg = #sc_mahjong_fight_info{
								list = PList
	},
	send_to_list(PlayerList, Msg).
	
%% 发送给指定玩家就可以的
send_mahjong_fight_info_to_self(false) ->
	skip;
send_mahjong_fight_info_to_self(MahjongPlayer) ->
	PList = pack_pb_mahjong_fight_info_list([MahjongPlayer]), 
	Msg = #sc_mahjong_fight_info{
								list = PList
	},
	send_to_pid(MahjongPlayer#mahjong_fight_info.gate_pid, Msg).

%% 广播接口
send_mahjong_fight_info() ->
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	send_mahjong_fight_info(MahjongPlayerList).
send_mahjong_fight_info(false) ->
	skip;
send_mahjong_fight_info(Player) when is_record(Player, mahjong_fight_info) ->
	send_mahjong_fight_info([Player]);
send_mahjong_fight_info(PlayerList) ->
	%% 战斗玩家
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	%% 观战玩家
	MahjongWatchList = lib_mahjong_fight_procdict:get_dic_mahjong_fight_watch_list(),
	PList = pack_pb_mahjong_fight_info_list(PlayerList), 
	Msg = #sc_mahjong_fight_info{
								list = PList
	},
	send_to_list(MahjongPlayerList ++ MahjongWatchList, Msg).

send_to_pid(GatePid, Msg) ->
	lib_gate:send_data(GatePid, Msg).

send_to_list(Msg) ->
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	%% 观战玩家
	MahjongWatchList = lib_mahjong_fight_procdict:get_dic_mahjong_fight_watch_list(),
	send_to_list(MahjongPlayerList ++ MahjongWatchList, Msg).

send_to_list([], _Msg) ->
	ok;
send_to_list([E|Z], Msg) ->
	lib_gate:send_data(E#mahjong_fight_info.gate_pid, Msg),
	send_to_list(Z, Msg).
% send_mahjong_fight_info(Player) ->
% 	ok.

pack_pb_mahjong_fight_info_list(PlayerList) ->
	pack_pb_mahjong_fight_info_list(PlayerList, []).

pack_pb_mahjong_fight_info_list([], PbMagjongList) ->
	PbMagjongList;
pack_pb_mahjong_fight_info_list([E|Z], PbMagjongList) ->
	PbMagjong = pack_pb_mahjong_fight_info(E),
	pack_pb_mahjong_fight_info_list(Z, [PbMagjong|PbMagjongList]).
pack_pb_mahjong_fight_info(MahjongPlayer) ->
	#mahjong_fight_info{
						player_id = PlayerId,      %% 玩家id
						% type = Type,
						player_icon = PlayerIcon,
						mahjong_id = MahjongId,
						circle = Circle,
						order = Order,
						% player_pid = PLayerPid,     %% 玩家pid 直接发消息
						position = Position,       %% 这个位置是唯一的，就像玩家id一样，好比座位
						basis = Basis,    		%% 手上的牌未出的牌 可以打的牌{11,1}
						meld = Meld,          %% 吃的牌[{?MAHJONG_MELD_TYPE_EAT，{1,2,3}}， {111，{4，4，4}}]吃也不在可以打 一组 
						out = Out,      		%% 记下玩家打出的牌
						flower = Flower,   		%% 玩家有的花牌 用来判断8张胡 等[1,2,3]
						state = State,           %% 1 进入，2 准备， 3进行中，4 已经离桌
						flag = Flag,
						canmeld = CanMeld,
						canwin = CanWin,
						re_time = ReTime,
						play_limit = PlayLimit
	} = MahjongPlayer,
	LeftTime = ?CONDITIONAL_EXPRESSION(State == ?STATE_LEAVEL, max(0, ?MAX_DROP_TIME - (mod_time:now_seconds() - ReTime)), 0),
	#pb_mahjong_fight_info{
						id = PlayerId,
						order = Order,
						circle = Circle,
						position = Position,
						basis = pack_pb_mahjong_basis_list(Basis),
						meld = pack_pb_mahjong_meld_info_list(Meld),
						out = Out,
						flower = pack_pb_mahjong_flower(Flower,Order),
						state = State,

						flag = Flag,
						canmeld = pack_pb_mahjong_meld_info_list(CanMeld),
						canwin = pack_pb_mahjong_ready_list(CanWin),
						iconurl = PlayerIcon,
						mahjongid = MahjongId,
						lefttime = LeftTime,
						
						playlimit = PlayLimit


	}.
%% 花的结构 案例可以在逻辑里面存成{Type, List}, 但之前已经完成 尽量不改逻辑，因为也不大影响效率
pack_pb_mahjong_flower(Flower, Position) ->
	pack_pb_mahjong_flower(Flower, Position, [], []).
pack_pb_mahjong_flower([], _Position, [], []) ->
	[];
pack_pb_mahjong_flower([], _Position, [], WildList) ->
	[#pb_mahjong_flower{type = false,list = WildList}];
pack_pb_mahjong_flower([], _Position, AreList, []) ->
	[#pb_mahjong_flower{type = true, list = AreList}];
pack_pb_mahjong_flower([], _Position, AreList, WildList) ->
	[#pb_mahjong_flower{ type = true,list = AreList},
	#pb_mahjong_flower{ type = false, list = WildList}];
pack_pb_mahjong_flower([E|FlowerList], Position, AreList, WildList) ->
	if
		Position == E orelse E == Position + 4 ->
			pack_pb_mahjong_flower(FlowerList, Position, [E|AreList], WildList);
		true ->
			pack_pb_mahjong_flower(FlowerList, Position, AreList, [E|WildList])
	end.
pack_pb_mahjong_basis_list(BasisList) ->
	NewBasisList = lists:ukeysort(1, BasisList),
	F = fun({EMahjongId, ENum}, Acc) ->
			EList = lists:duplicate(ENum, EMahjongId),
			Acc ++ EList
		end,
	lists:foldl(F, [], NewBasisList).

pack_pb_mahjong_meld_info_list(MeldList) ->
	pack_pb_mahjong_meld_info_list(MeldList, []).

pack_pb_mahjong_meld_info_list([], PbMeldList) ->
	PbMeldList;
pack_pb_mahjong_meld_info_list([E|Z], PbMeldList) ->
	PbMeld = pack_pb_mahjong_meld_info(E),
	pack_pb_mahjong_meld_info_list(Z, [PbMeld|PbMeldList]).
 
pack_pb_mahjong_meld_info({{Type, MahjongId}, PlayerId, E}) ->
	#pb_mahjong_meld_info{
					type = Type,
					playerid = PlayerId,
					mahjongid = MahjongId,
					info = tuple_to_list(E)
	}.

pack_pb_mahjong_ready_list(ReadyList) ->
	pack_pb_mahjong_ready_list(ReadyList, []).
pack_pb_mahjong_ready_list([], PbReadyList) ->
	PbReadyList;
pack_pb_mahjong_ready_list([E|Z], PbReadyList) ->
	PbReady = pack_pb_mahjong_ready(E),
	pack_pb_mahjong_ready_list(Z, [PbReady|PbReadyList]).
pack_pb_mahjong_ready({EId, ReadyList}) ->
	#pb_mahjong_ready{
						id = EId,
						list = ReadyList
	}.
%% 发送state 广播
send_mahjong_fight_state() ->
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	Msg = pack_pb_mahjong_fight_state(MahjongState),
	send_to_list(Msg).
%% 发送指定玩家
send_mahjong_fight_state(GatePid) ->
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	Msg = pack_pb_mahjong_fight_state(MahjongState),
	send_to_pid(GatePid, Msg),
	ok.

pack_pb_mahjong_fight_state(MahjongState) ->
	#r_mahjong_fight_state{
						banker = Banker,
						dice_postion = DicePostion,
						feng_circle = FengCircle, 
						basis_list = BasisList,
						% nowpostion = NowPostion,
						is_end = IsEnd,
						is_first = IsFirst,
						last_position = LastPosition,     %% 
						last_mahjongid = LastMahjongId, 
						start_time = StartTime,
						long_time = LongTime
						% meld = Meld
	} = MahjongState,
	#sc_mahjong_fight_state{
						banker = Banker,
						dicepostion = DicePostion,
						isend = IsEnd,
						isfirst = IsFirst,
						lastpostion = LastPosition,			%% 上一个出牌的方位
  						lastmahjongid = LastMahjongId,
  						endtime = StartTime + LongTime,
  						fengcircle = FengCircle,
  						num = length(BasisList)
	}.

send_mahjong_fight_reply(Msg) ->
	GatePid = lib_player_procdict:get_dic_gate_pid(),
	send_mahjong_fight_reply(GatePid, Msg).
send_mahjong_fight_reply(GatePid, Msg) when is_pid(GatePid)->
	lib_gate:send_data(GatePid, Msg);
send_mahjong_fight_reply(PlayerId, Msg) ->
	lib_player:send_msg_by_player_id(PlayerId, Msg).


pack_pb_mahjong_fight_reply(sc_mahjong_fight_die, Result, Reason) ->
	#sc_mahjong_fight_die{ result = Result, reason = Reason};
pack_pb_mahjong_fight_reply(sc_mahjong_fight_play, Result, Reason) ->
	#sc_mahjong_fight_play{ result = Result, reason = Reason};
pack_pb_mahjong_fight_reply(sc_mahjong_fight_win, Result, Reason) ->
	#sc_mahjong_fight_win{ result = Result, reason = Reason};
pack_pb_mahjong_fight_reply(sc_mahjong_fight_ready, Result, Reason) ->
	#sc_mahjong_fight_ready{ result = Result, reason = Reason};
pack_pb_mahjong_fight_reply(sc_mahjong_fight_prepare, Result, Reason) ->
	#sc_mahjong_fight_prepare{ result = Result, reason = Reason};
pack_pb_mahjong_fight_reply(sc_mahjong_fight_cancel, Result, Reason) ->
	#sc_mahjong_fight_cancel{ result = Result, reason = Reason};
pack_pb_mahjong_fight_reply(sc_mahjong_fight_meld, Result, Reason) ->
	#sc_mahjong_fight_meld{ result = Result, reason = Reason}.

send_mahjong_fight_left_time() ->
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	#r_mahjong_fight_state{
						now_postion = NowPosition,
						wait_start_time = WaitStartTime
						} = MahjongState,
	PlayerId = lib_mahjong_fight_procdict:get_mahjong_player_id_by_position(NowPosition),
	LeftTime = max(?MAX_WAIT_TIME - (mod_time:now_seconds() - WaitStartTime),0),
	send_mahjong_fight_left_time(LeftTime, PlayerId, NowPosition).
send_mahjong_fight_left_time(LeftTime, PlayerId, Position) ->
	Msg = #sc_mahjong_fight_left_time{
							lefttime = LeftTime,
							id = PlayerId,
							position = Position
	},
	send_to_list(Msg).


send_mahjong_fight_end() ->
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	MahjongWatchList = lib_mahjong_fight_procdict:get_dic_mahjong_fight_watch_list(),
	send_mahjong_fight_end(MahjongPlayerList ++ MahjongWatchList).
send_mahjong_fight_end(PlayerList) ->
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	Msg = pack_sc_mahjong_fight_end(MahjongState, MahjongPlayerList),
	send_to_list(PlayerList, Msg).

pack_sc_mahjong_fight_end(MahjongState, List) ->
	#r_mahjong_fight_state{
						is_win = IsWin,
						% meld = Meld,
						combination = Combination,
						type_list = TypeList,
						win_id = WinId,
						count = Count
	} = MahjongState,
	PbCombinationList = pack_pb_mahjong_combination(Combination),
	PbTypeList = pack_pb_mahjong_win_type(TypeList),
	PbList = pack_pb_mahjong_fight_end_list(List),
	#sc_mahjong_fight_end{
					iswin = IsWin,
					list = PbList,
					combinationlist = PbCombinationList,
					typelist = PbTypeList,
					winid = WinId,
					count = Count
	}.

%% 获取组合
pack_pb_mahjong_combination([]) ->
	#pb_mahjong_combination{
						pairs = 0,
						combination = []
	};
pack_pb_mahjong_combination(CombinationList) ->
	pack_pb_mahjong_value(CombinationList).

pack_pb_mahjong_value(CombinationList) ->
	pack_pb_mahjong_value(CombinationList, []).
pack_pb_mahjong_value([], PbCombinationList) ->
	PbCombinationList;
pack_pb_mahjong_value([E|CombinationList], PbCombinationList) ->
	Pb = #pb_mahjong_value{info = tuple_to_list(E)},
	pack_pb_mahjong_value(CombinationList, [Pb|PbCombinationList]).
%% 获取台数计算
pack_pb_mahjong_win_type(TypeList) ->
	pack_pb_mahjong_win_type(TypeList, []).
pack_pb_mahjong_win_type([], PbTypeList) ->
	PbTypeList;
pack_pb_mahjong_win_type([{Type, Score}|TypeList], PbTypeList) ->
	Pb = #pb_mahjong_win_type{
					type = Type,
					score = Score
	},
	pack_pb_mahjong_win_type(TypeList, [Pb|PbTypeList]).
%% 结算
pack_pb_mahjong_fight_end_list(List) ->
	pack_pb_mahjong_fight_end_list(List, []).
pack_pb_mahjong_fight_end_list([], PbList) ->
	PbList;
pack_pb_mahjong_fight_end_list([E|List], PbList) ->
	Pb = pack_pb_mahjong_fight_end(E),
	pack_pb_mahjong_fight_end_list(List, [Pb|PbList]).
	
pack_pb_mahjong_fight_end(E) ->
	#mahjong_fight_info{
				% flag = EFlag,
				end_type = EndType,
				player_id = EPlayerId,
				score = Score
	} = E,
	PbReward = #pb_reward_info{
							type = 1,
							num = abs(Score)
	},
	if
		Score == 0 ->  %% 不输不赢
			EFlag = 0;
		Score > 0 ->
			EFlag = 1;
		true ->
			EFlag = 2
	end,
	#pb_mahjong_fight_end{
						id = EPlayerId,
						type = EndType,
						flag = EFlag,
						reward = PbReward
	}.

send_mahjong_fight_record() ->
	Record = lib_mahjong_fight_procdict:get_dic_mahjong_common_record(),
	PList = pack_pb_mahjong_fight_record_list(Record#common_record.info),
	Msg = #sc_mahjong_fight_record{
							list = PList
	},
	send_to_list(Msg).
send_mahjong_fight_record(GatePid) ->
	Record = lib_mahjong_fight_procdict:get_dic_mahjong_common_record(),
	PList = pack_pb_mahjong_fight_record_list(Record#common_record.info),
	Msg = #sc_mahjong_fight_record{
							list = PList
	},
	send_to_pid(GatePid, Msg).

pack_pb_mahjong_fight_record_list(Members) ->
	pack_pb_mahjong_fight_record_list(Members, []).
pack_pb_mahjong_fight_record_list([], PbList) ->
	PbList;
pack_pb_mahjong_fight_record_list([E|Members], PbList) ->
	Pb = pack_pb_mahjong_fight_record(E),
	pack_pb_mahjong_fight_record_list(Members, [Pb|PbList]).

pack_pb_mahjong_fight_record(E) ->
	#r_record_member{
				player_id = PlayerId,
          		win_flag = WinFlag,
          		score = Score
	} = E,
	#pb_mahjong_fight_record{
						playerid = PlayerId,
						flag = WinFlag,
						score = abs(Score)
	}.




