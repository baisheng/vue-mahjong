 %% icolun
%% 麻将玩法进程
-module(lib_mahjong_fight_process).
-include("mahjong_fight.hrl").
-include("record.hrl").
-include("common.hrl").
-include_lib("db/include/record.hrl").
-include_lib("protob/include/p03_mahjong_fight_pb.hrl").

-export([
		open_mahjong_fight/2,						%% 第一次坐下后点开局
		init_battlefiled/2,							%% 初始化战斗

		mahjong_prepare/1,							%% 玩家准备
		mahjong_die/1,								%% 置色子
		mahjong_play/2,								%% 出牌
		mahjong_win/1,								%% 胡牌
		mahjong_meld/2,								%% 吃碰杠
		mahjong_flow/0,								%% 流局
		mahjong_fight_end/0,						%% 结算
		mahjong_cancel/0,
		mahjong_cancel/1,							%% 取消胡吃碰杠
		mahjong_quit/1,								%% 玩家离开
		mahjong_draw/1,								%% 抓牌
		mahjong_ready/2,                            %% 听牌
		mahjong_deal/0,                              %% 发牌
		mahjong_dropped/0,                          %% 检测玩家掉线
		mahjong_stop/0,                             %% 定时关闭地图进程
		mahjong_update/1,                           %% 牌局信息更新

		change_mahjong_wait_ref/0,
		change_mahjong_wait_ref/2,					%% 出牌定时改变
		change_mahjong_wait_ref/3,
		change_mahjong_wait_ref/4,

		make_mahjong_start_ref/2,
		make_mahjong_start_ref/3,
		make_mahjong_start_ref/4,

		%% 取消定时
		cancel_mahjong_fight_ref/1
		
		]).
% -compile(export_all).

%% 进行字典
%% 等待玩家操作的定时，如果玩家操作了，变更下一个
%% 担心之后还会有定时出牌，保留 只会更新定时
change_mahjong_wait_ref() ->
	change_mahjong_wait_ref(change_mahjong_wait_ref, []).
change_mahjong_wait_ref(Fun, Args) ->
	change_mahjong_wait_ref(?MODULE, Fun, Args).
change_mahjong_wait_ref(Module, Fun, Args) ->
	change_mahjong_wait_ref(?MAX_WAIT_TIME, Module, Fun, Args).
change_mahjong_wait_ref(Time, Module, Fun, Args) ->
	cancel_mahjong_wait_ref(),
	% %%io:format("______change_mahjong_wait_ref~n"),
	% %% 检测有人掉线么，掉线 掉线了，重复定时显示时间
	% %%io:format("___change_mahjong_wait_ref~p~n",[{Module, Fun, Args, mahjong_dropped()}]),
	% %% 之前做托管出牌的，该放定时的地方，还是需要定时了，直接改成不执行那个函数，就可以
	% case mahjong_dropped() of
	% 	true ->
			Ref = erlang:send_after(Time * 1000, self(), {'handle', ?MODULE, change_mahjong_wait_ref, [Module, Fun, Args]}),
	% 	_ ->
	% 		Ref = erlang:send_after(Time * 1000, self(), {'handle', Module, Fun, Args})
	% end,
	% update_dic_magjong_wait_ref(Ref),
	%% 更新新的等待开始时间
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	NewMahjongState = MahjongState#r_mahjong_fight_state{ wait_start_time = mod_time:now_seconds(), wait_ref = Ref},
	lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
	%% 发送剩余时间
	lib_mahjong_fight_msg:send_mahjong_fight_left_time(),
	ok.
%% 这个是区别开出牌登的定时，就是没有连续性的定时
make_mahjong_start_ref(Fun, Args) ->
	make_mahjong_start_ref(?MODULE, Fun, Args).
make_mahjong_start_ref(Module, Fun, Args) ->
	make_mahjong_start_ref(?MAX_START_TIME, Module, Fun, Args).
make_mahjong_start_ref(Time, Module, Fun, Args) ->
	cancel_mahjong_start_ref(),
	Ref = erlang:send_after(Time * 1000, self(), {'handle', Module, Fun, Args}),
	%%io:format("make_mahjong_start_ref~p~n",[{Time, Module, Fun, Args, Ref}]),
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	NewMahjongState = MahjongState#r_mahjong_fight_state{  start_ref = Ref},
	lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
	ok.
	%% 先取消之前的定时器
%% 处理定时
cancel_mahjong_start_ref() ->
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	cancel_mahjong_fight_ref(MahjongState#r_mahjong_fight_state.start_ref),
	NewMahjongState = MahjongState#r_mahjong_fight_state{start_ref = ?UNDEFINED},
	lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState).
%% 处理等候定时
cancel_mahjong_wait_ref() ->
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	cancel_mahjong_fight_ref(MahjongState#r_mahjong_fight_state.wait_ref),
	NewMahjongState = MahjongState#r_mahjong_fight_state{wait_ref = ?UNDEFINED},
	lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState).

%% 取消定时
cancel_mahjong_fight_ref(Ref) ->
	if
		erlang:is_reference(Ref) ->
			erlang:cancel_timer(Ref);
		true ->
			skip
	end.
%% 战斗进程直接用 还有一个检测是，玩家进入后一定时间不开战 自动解散
open_mahjong_fight(RRoomInfo, UniqueFlag) -> 
	case mod_room_manager:get_pid_only(UniqueFlag) of
		RoomPid when is_pid(RoomPid) ->
			%%io:format("room_pid~p~n",[RoomPid]),
			% lib_room_info:change_room_state(RoomId, true),
			gen_server:cast(RoomPid, {'handle', ?MODULE, init_battlefiled, [RRoomInfo,UniqueFlag]}),
			ok;
		_ ->
			skip
	end.

init_battlefiled(RRoomInfo, UniqueFlag) ->
	#ets_room_info{
				id = RoomId,
				record_id = RecordId,
				% type = Type,
				owner_id = OwnerId,
				members = MemberList,
				start_time = StartTime,
				long_time = LongTime
	} = RRoomInfo,
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	#r_mahjong_fight_state{
						banker = Banker,
						feng_circle = FengCircle,
						circle_banker = CircleBanker,
						banker_list = BankerList
	} = MahjongState,
	if
		?IS_POSITION(Banker) ->  %% 如果庄家在的话
			NewBanker = Banker,
			NewFengCircle = FengCircle,
			NewCircleBanker = CircleBanker;
		true ->
			NewBanker = util:random(?POSITION_ID_LIST),
			NewFengCircle = 1,
			NewCircleBanker = NewBanker  %% 圈位庄家
	end,
	%% 只有第一把是1，之后已经确定
	NewMahjongState = MahjongState#r_mahjong_fight_state{
									banker = NewBanker,
									feng_circle = NewFengCircle,
									circle_banker = NewCircleBanker,
									banker_list = lists:umerge([NewBanker],BankerList),
									room_id = RoomId,
									start_time = StartTime,
									long_time = LongTime,
									is_end = true

	},
	lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),


	%%io:format("init_battlefiled ~p~n",[MemberList]),
	% %% 更新战斗信息
	NowTime = mod_time:now_seconds(),
	%% 初始化牌局记录
	CommonRecord = lib_record_process:get_common_record(RecordId),
	lib_mahjong_fight_procdict:update_dic_mahjong_common_record(CommonRecord),
	%%  更新玩家信息
	F = fun(EPlayer, {AccMahjongfight, AccMahjongWatch}) ->
			#r_room_member{
						player_id = EPlayerId,
						player_pid =EPlayerPid,
						gate_pid = EGatePid,
						position = EPosition,
						player_name = PlayerName,
						player_icon = PlayerIcon

			} = EPlayer,
			%% 只是方便玩家进程在请求时，能拿到房间进程
			gen_server:cast(EPlayerPid, {'handle', lib_mahjong_fight_player, notice_mahjong_fight_start, [RecordId, RoomId, OwnerId, EPosition, NowTime,UniqueFlag]}),
			Pb = #mahjong_fight_info{
							player_id = EPlayerId, 
							player_pid = EPlayerPid,
							gate_pid = EGatePid,
							position = EPosition,

							room_id = RoomId,
							player_name = PlayerName,
							player_icon = PlayerIcon,

							% order = EOrder + 1,     %% 庄家是1
							basis = [],    		%% 手上的牌未出的牌 可以打的牌{11,1}
							meld = [],           %% 吃的牌[111,playerId, {1,2,3}， {4，4，4}]吃也不在可以打 一组 
							out = [],      		%% 记下玩家打出的牌
							flower = [],   		%% 玩家有的花牌 用来判断8张胡 等[1,2,3]
							state = ?STATE_IN,  %% 
							score = 0,

							flag = 0,           %% 0, 1 是已经选择听牌 2 可以胡牌 3已经选择赢牌 4 已经胡牌
							canmeld = [],    
							canwin = [],         %% 可以听的牌 {1, []}
							limit = [],
							play_limit = []
			},
			if
				?IS_POSITION(EPosition) -> %% 
					CirclePosition = ?GET_DISTANCE_POSITION(NewBanker, EPosition),
					{[Pb#mahjong_fight_info{circle = CirclePosition, order = CirclePosition}|AccMahjongfight], AccMahjongWatch};
				true ->
					{AccMahjongfight, [Pb|AccMahjongWatch]}
			end
		end,
	{MahjongPlayerList, MahjongWatchList} = lists:foldl(F, {[], []}, MemberList),
	%% 更新玩家信息
	lib_mahjong_fight_procdict:update_dic_mahjong_player_list(MahjongPlayerList),
	%% 更新旁观者信息
	lib_mahjong_fight_procdict:update_dic_mahjong_fight_watch_list(MahjongWatchList),

	% %% 初始化玩家信息
	% lib_mahjong_fight_msg:send_mahjong_fight_info(),
	%%io:format("init_battlefiled3 ~p~n",[MahjongPlayerList]),
	% %% 准备结束，洗
	ok.

%% 玩家准备，一轮结束后 要准备触发的，才能开启第二局
mahjong_prepare(PlayerId) ->
	case pre_mahjong_prepare(PlayerId) of
		{true, Dict} ->
			FunChange = dict:fetch('fun_change', Dict),
			FunChange(),
			ok;
		{false, Reason} ->
			ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_prepare, false, Reason),
			lib_mahjong_fight_msg:send_mahjong_fight_reply(PlayerId, ReplyMsg)
	end.
pre_mahjong_prepare(PlayerId) ->
	CheckList = [
				'player',
				'change'
				],
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	Dict = dict:from_list([
							{'mahjong_state', MahjongState},
							{'player_id', PlayerId}
						]),
	pre_mahjong_prepare_check(Dict, CheckList).

pre_mahjong_prepare_check(Dict, []) ->
	{true, Dict};
pre_mahjong_prepare_check(Dict, ['player'|Z]) ->
	PlayerId = dict:fetch('player_id', Dict),
	MahjongState = dict:fetch('mahjong_state', Dict),
	#r_mahjong_fight_state{
						is_end = IsEnd,
						start_time = StartTime,
						long_time = LongTime

	} = MahjongState,
	NowTime = mod_time:now_seconds(),
	if
		IsEnd == false ->
			{false, "牌局没有结束"};
		NowTime - StartTime > LongTime ->
			{false, "房间可打牌时间已经结束"};
		true ->
			case lib_mahjong_fight_procdict:get_mahjong_by_player_id(PlayerId) of
				false ->
					{false,"玩家不在牌局中"};
				MahjongPlayer when MahjongPlayer#mahjong_fight_info.state == ?STATE_PREPARE ->
					{false, "玩家已经准备"};
				MahjongPlayer ->
					NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{state =?STATE_PREPARE},
					Dict1 = dict:store('mahjong_player', NewMahjongPlayer, Dict),
					pre_mahjong_prepare_check(Dict1, Z)
			end
	end;
pre_mahjong_prepare_check(Dict, ['change'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	MahjongPlayer = dict:fetch('mahjong_player', Dict),
	EOrder = ?GET_DISTANCE_POSITION(MahjongState#r_mahjong_fight_state.banker, MahjongPlayer#mahjong_fight_info.position),
	NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{
							order = EOrder ,     %% 庄家是1 东风位
							basis = [],    		%% 手上的牌未出的牌 可以打的牌{11,1}
							meld = [],           %% 吃的牌[111,playerId, {1,2,3}， {4，4，4}]吃也不在可以打 一组 
							out = [],      		%% 记下玩家打出的牌
							flower = [],   		%% 玩家有的花牌 用来判断8张胡 等[1,2,3]
							state = ?STATE_PREPARE,
							score = 0,

							mahjong_id = 0,
							play_mahjong_id = 0,
							flag = 0,           %% 0, 1 是已经选择听牌 2 可以胡牌 3已经选择赢牌 4 已经胡牌
							canmeld = [],    
							canwin = [],         %% 可以听的牌 {1, []}

							limit = [],
							play_limit = [],
							is_kong = false,
							end_type = 0,

							re_time = 0,
							re_ref = undefined
	},
	FunChange = fun() ->
					lib_mahjong_fight_procdict:update_mahjong_by_player_id(NewMahjongPlayer),
					% lib_mahjong_fight_util:refresh_mahjong_fight_info(NewMahjongPlayer),
					lib_mahjong_fight_msg:send_mahjong_fight_info(NewMahjongPlayer),
					%%io:format("prepare~n"),
					%% 每一个玩家准备了，做一次检测洗牌
					mahjong_shuffle(),
					ok
				end,
	Dict1 = dict:store('fun_change', FunChange, Dict),
	pre_mahjong_prepare_check(Dict1, Z);
pre_mahjong_prepare_check(Dict, [_|Z]) ->
	pre_mahjong_prepare_check(Dict, Z).



%% 麻将洗牌
mahjong_shuffle() ->
	%%io:format("____shuffle~n"),
	case pre_mahjong_shuffle() of
		{true, Dict} ->
			FunChange = dict:fetch('fun_change', Dict),
			%%io:format("____shuffle1~n"),
			%% 执行洗牌
			FunChange();
		{false, _Reason} ->
			%%io:format("____shuffle~p~n",[Reason]),
			skip
	end.
pre_mahjong_shuffle() ->
	CheckList = [
				'ready',
				'banker',
				'change'  %% 麻将新的信息 包括 状态， 玩家列表，定时等信息 
				],
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	Dict = dict:from_list([
							{'mahjong_state', MahjongState},
							{'mahjong_player_list', MahjongPlayerList}
							]),
	pre_mahjong_shuffle_check(Dict, CheckList).

pre_mahjong_shuffle_check(Dict, []) ->
	{true, Dict};
pre_mahjong_shuffle_check(Dict, ['ready'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	MahjongPlayerList = dict:fetch('mahjong_player_list', Dict),
	if
		MahjongState#r_mahjong_fight_state.is_end == false ->
			{false, "游戏正在进行中"};
		length(MahjongPlayerList) < ?MAX_POSITION_NUM ->
			{false, "人员不足"};
		true ->
			F = fun(E, AccReady) ->
				if
					AccReady =/= true ->
						AccReady;
					E#mahjong_fight_info.state == ?STATE_LEAVEL  ->
						{false, "有玩家已经离开"};
					E#mahjong_fight_info.state =/= ?STATE_PREPARE ->
						{false, "有玩家没有准备"};
					true ->
						AccReady
				end
			end,
			case lists:foldl(F, true, MahjongPlayerList) of
				true ->
					pre_mahjong_shuffle_check(Dict, Z);
				R ->
					R
			end
	end;
pre_mahjong_shuffle_check(Dict, ['banker'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	#r_mahjong_fight_state{
						banker = Banker,
						feng_circle = OldFengBanker
	} = MahjongState,
	if
		?IS_POSITION(Banker) ->
			NewBanker = Banker,
			FengBanker = OldFengBanker;
		true ->
			NewBanker = util:random(?POSITION_ID_LIST),
			FengBanker = 1
	end,
	io:format("pre_mahjong_shuffle_check~p~n", [NewBanker]),
	Dict1 = dict:store('banker', NewBanker, Dict),
	Dict2 = dict:store('feng_circle', FengBanker, Dict1),
	pre_mahjong_shuffle_check(Dict2, Z);
pre_mahjong_shuffle_check(Dict, ['change'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	FengBanker = dict:fetch('feng_circle', Dict),
	Banker = dict:fetch('banker', Dict),
	%% 获取打乱的初始的基础麻将 即未拿的所有麻将的组合
	BasisList = util:random_order(?FLOWER_ID_LIST ++ lists:flatten(lists:duplicate(?MAX_POSITION_NUM, ?MAHJONG_ID_LIST))),
	%% 变更状态信息
	NewMahjongState = MahjongState#r_mahjong_fight_state{
										banker = Banker,
										feng_circle = FengBanker,
										now_postion = Banker,
										last_position = 0,
										last_mahjongid = 0,
										dice_postion = 0,

										basis_list = BasisList,
										out_list = [],
										flower_num = 0,
										kong_num = 0, 

										is_end = true,       	%% false 进行中   true 结束
										% is_first = false,
										is_win = false,      	%% 这一局是胜利结束的，因为胡牌有可能多家胡牌，不好确定下一家的庄家
										is_kong = false,

										is_first = true,
										circle_flag = 0,
										win_position = 0,       %% 如果本轮赢牌，从庄家下来，最近的先赢者 用来发牌
										combination = [],       %% 如果是胡牌，就是手上的牌的组合
										meld = [],              %% 胡的人手上的牌
										type_list = [],         %% 赢的时候胡牌类型{自摸，1台}
										win_id = 0,
										count = 0

	},

	%% 变化
	FunChange = fun() ->   
					%% 变更战斗状态
					lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
					%%io:format("____shuffle3~p~n", [NewMahjongState]),
					lib_mahjong_fight_msg:send_mahjong_fight_state(),
					lib_room_info:change_room_state(NewMahjongState#r_mahjong_fight_state.room_id, true),
					%% 主动置色子
					PlayerId = lib_mahjong_fight_procdict:get_mahjong_player_id_by_position(Banker),
					%%io:format("____shuffle~p~n",[{PlayerId, Banker}]),
					mahjong_die(PlayerId),
					ok
				end,
	Dict1 = dict:store('fun_change', FunChange, Dict),
	pre_mahjong_shuffle_check(Dict1, Z);
pre_mahjong_shuffle_check(Dict, [_|Z]) ->
	pre_mahjong_shuffle_check(Dict, Z).

%%
mahjong_die(PlayerId) ->
	case pre_mahjong_die(PlayerId) of
		{true, Dict} ->
			FunChange = dict:fetch('fun_change', Dict),
			%%io:format("——————————mahjong_die~n"),
			%% 执行
			FunChange();
		{false, Reason} ->
			% skip
			ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_die, false, Reason),
			lib_mahjong_fight_msg:send_mahjong_fight_reply(PlayerId, ReplyMsg)
	end.
pre_mahjong_die(PlayerId) -> %% 虽然只是一个随机，然后返回客户端，但是
	CheckList = [
				'player',
				'ready',
				'change'  %% 值完色子 会发牌
				],
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	Dict = dict:from_list([
							{'player_id', PlayerId},
							{'mahjong_state', MahjongState},
							{'mahjong_player_list', MahjongPlayerList}
							]),
	pre_mahjong_die_check(Dict, CheckList).
pre_mahjong_die_check(Dict, []) ->
	{true, Dict};
pre_mahjong_die_check(Dict, ['player'|Z]) ->
	PlayerId = dict:fetch('player_id', Dict),
	MahjongState = dict:fetch('mahjong_state', Dict),
	MahjongPlayerList = dict:fetch('mahjong_player_list', Dict),
	Length = length(?MAHJONG_ID_LIST) * 4 + length(?FLOWER_ID_LIST),
	StateLength = length(MahjongState#r_mahjong_fight_state.basis_list),
	case lib_mahjong_fight_procdict:get_mahjong_by_player_id(PlayerId) of
		false ->
			{false, "玩家没有参与牌局"};
		MahjongPlayer when MahjongPlayer#mahjong_fight_info.position =/= MahjongState#r_mahjong_fight_state.banker->
			{false, "玩家不是庄家"};
		_MahjongPlayer when StateLength < Length ->
			{false, "洗牌不成功"};
		_MahjongPlayer when length(MahjongPlayerList) < ?MAX_POSITION_NUM ->
			{false, "人员不足"};
		MahjongPlayer ->
			Dict1 = dict:store('mahjong_player', MahjongPlayer, Dict),
			pre_mahjong_die_check(Dict1, Z)
	end;
pre_mahjong_die_check(Dict, ['ready'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	MahjongPlayerList = dict:fetch('mahjong_player_list', Dict),
	if
		MahjongState#r_mahjong_fight_state.is_end == false ->
			{false, "游戏正在进行中"};
		true ->
			F = fun(E, AccReady) ->
				if
					AccReady =/= true ->
						AccReady;
					E#mahjong_fight_info.state =/= ?STATE_PREPARE ->
						{false, "有玩家没有准备"};
					true ->
						AccReady
				end
			end,
			case lists:foldl(F, true, MahjongPlayerList) of
				true ->
					pre_mahjong_die_check(Dict, Z);
				R ->
					R
			end
	end;
pre_mahjong_die_check(Dict, ['change'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	% MahjongPlayer = dict:fetch('mahjong_player', Dict),
	MahjongPlayerList = dict:fetch('mahjong_player_list', Dict),
	Position1 = util:random(?DIE_FACE_NUM),
	Position2 = util:random(?DIE_FACE_NUM),
	Position = Position1 + Position2,
	%%io:format("——————————mahjong_die2~p~n",[MahjongState]),
	%% 如果庄家为0，可以先掷骰子
	%% 把置色子后前面移至牌尾，并逆转
	{List1, List2} = lists:split(Position, MahjongState#r_mahjong_fight_state.basis_list),
	BasisList = List2 ++ lists:reverse(List1),
	NewMahjongState = MahjongState#r_mahjong_fight_state{
										basis_list = BasisList,
										dice_postion = Position,
										is_end = false, 
										is_win = false
										},
	%% 变更玩家信息
	F = fun(E) ->
			E#mahjong_fight_info{
							state = ?STATE_BEING
			}
		end,
	NewMahjongPlayerList = lists:map(F, MahjongPlayerList),
	FunChange = fun() ->
					%% 改变玩家状态
					lib_mahjong_fight_procdict:update_dic_mahjong_player_list(NewMahjongPlayerList),
					%% 变化最新的未取牌
					lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
					%% 制完色子 通知玩家色子
					% ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_die, true, ""),
					% lib_mahjong_fight_msg:send_mahjong_fight_reply(MahjongPlayer#mahjong_fight_info.gate_pid, ReplyMsg),
					%% 广播玩家色子数
					lib_mahjong_fight_msg:send_mahjong_fight_state(),
					%% 
					%%io:format("_____die~p~n",[{NewMahjongState}]),
					make_mahjong_start_ref(mahjong_deal, []),
					%% 完了直接发牌
					ok
				end,
	Dict1 = dict:store('fun_change', FunChange, Dict),
	pre_mahjong_die_check(Dict1, Z);
pre_mahjong_die_check(Dict, [_|Z]) ->
	pre_mahjong_die_check(Dict, Z).




%%  发牌
mahjong_deal() ->
	case pre_mahjong_deal() of
		{true, Dict} ->
			FunChange = dict:fetch('fun_change', Dict),

			%% 执行
			FunChange();
		{false, _Reason} ->
			skip
	end.
pre_mahjong_deal() ->
	CheckList = [
				'change'  %% 值完色子 会发牌
				],
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	Dict = dict:from_list([
							{'mahjong_state', MahjongState},
							{'mahjong_player_list', MahjongPlayerList}
							]),
	pre_mahjong_deal_check(Dict, CheckList).
pre_mahjong_deal_check(Dict, []) ->
	{true, Dict};
pre_mahjong_deal_check(Dict, ['change'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	MahjongPlayerList = dict:fetch('mahjong_player_list', Dict),

	NewMahjongPlayerList = lists:ukeysort(#mahjong_fight_info.order, MahjongPlayerList),

	%% 将获得的牌，由【1，2，3，2】——》【{1，1}，{2，2}，{3，1}
	FunChange = fun() ->
					%% 获取牌前13张牌
					%% 
					{NewMahjongState, NewMahjongPlayerList1} = deal_mahjong_init(NewMahjongPlayerList, MahjongState),

					lib_mahjong_fight_procdict:update_dic_mahjong_player_list(NewMahjongPlayerList1),
					%%io:format("_____mahjong_deal~p~n",[NewMahjongPlayerList1]),
					lib_mahjong_fight_msg:send_mahjong_fight_info(),
					%% 更新剩余基础牌
					lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
					%% 取消发牌定时
					cancel_mahjong_start_ref(),
					%% 合并排序
					combine_mahjong_list(NewMahjongState, NewMahjongPlayerList1),
					ok
				end,
	Dict1 = dict:store('fun_change', FunChange, Dict),
	pre_mahjong_deal_check(Dict1, Z);
pre_mahjong_deal_check(Dict, [_|Z]) ->
	pre_mahjong_deal_check(Dict, Z).

%% 给玩家发牌
deal_mahjong_init(MahjongPlayerList, MahjongState) ->
	deal_mahjong_init(MahjongPlayerList, 4, MahjongState, []).

deal_mahjong_init([], Num, MahjongState, NewMahjongPlayer) when Num >0 ->
	deal_mahjong_init(NewMahjongPlayer, Num -1, MahjongState, []);
deal_mahjong_init([], _Num, MahjongState, MahjongPlayerList) ->
	{MahjongState, MahjongPlayerList};
deal_mahjong_init([E|List], Num, MahjongState, MahjongPlayerList) ->
	% %%io:format("deal_mahjong_init~p~n",[{E, Num, MahjongPlayerList}]),
	#r_mahjong_fight_state{
						basis_list = BasisList,
						banker = Banker
	} = MahjongState,
	MaxNum = ?CONDITIONAL_EXPRESSION(Banker == E#mahjong_fight_info.position, ?MAX_MAHJONG_NUM + 1, ?MAX_MAHJONG_NUM),
	ENum = min(4, MaxNum - lib_mahjong_fight_util:get_mahjong_fight_basis_length(E#mahjong_fight_info.basis)),
	{EList, NewAccBasisList} = lists:split(ENum, BasisList),

	EList1 = lists:map(fun(E0) -> {E0,1} end, EList),
	% %%io:format("_____qqq~p~n",[{ENum, EList, EList1, BasisList}]),
	NewMahjongState = MahjongState#r_mahjong_fight_state{basis_list = NewAccBasisList},
	NewE = E#mahjong_fight_info{basis = E#mahjong_fight_info.basis ++ EList1},
	NewMahjongPlayerList = MahjongPlayerList ++ [NewE],

	% lib_mahjong_fight_msg:send_mahjong_fight_info(NewE),
	deal_mahjong_init(List, Num, NewMahjongState, NewMahjongPlayerList).


%% 替换花 同时将相同的牌合并成
combine_mahjong_list(MahjongState, MahjongPlayerList) ->
	NewMahjongPlayerList = lists:ukeysort(#mahjong_fight_info.order, MahjongPlayerList),
	#r_mahjong_fight_state{
						basis_list = BasisList,
						flower_num = FlowerNum,
						banker = Banker
	} = MahjongState,
	F = fun(E, {AccBasisList, AccFlowerNum, AccMahjongPlayerList}) ->
			#mahjong_fight_info{
							basis = PlayerBasisList,
							flower = Flower
			} = E,
			{NewAccBasisList, NewAccFlowerNum, NewFlower, NewPlayerBasisList} = 
				combine_mahjong_list1(AccBasisList, AccFlowerNum, PlayerBasisList, Flower, []),
			NewE = E#mahjong_fight_info{
									basis = NewPlayerBasisList,
									flower = NewFlower
			},
			NewAccMahjongPlayerList = [NewE|AccMahjongPlayerList],
			% lib_mahjong_fight_msg:send_mahjong_fight_info(NewE),
			{NewAccBasisList, NewAccFlowerNum,NewAccMahjongPlayerList}
		end,
	{NewBasisList, NewFlowerNum, NewMahjongPlayerList1} = lists:foldl(F, {BasisList, FlowerNum, []}, NewMahjongPlayerList),
	NewMahjongState = MahjongState#r_mahjong_fight_state{
													basis_list = NewBasisList,
													flower_num = NewFlowerNum,
													now_postion = Banker,
													is_first = false,
													is_end = false
	},

	%% update
	lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
	lib_mahjong_fight_procdict:update_dic_mahjong_player_list(NewMahjongPlayerList1),
	%%io:format("!!!~p~n",[{Banker, lib_mahjong_fight_procdict:get_mahjong_by_position(Banker)}]),
	lib_mahjong_fight_msg:send_mahjong_fight_state(),
	lib_mahjong_fight_msg:send_mahjong_fight_info(),

	MahjongPlayer = lib_mahjong_fight_procdict:get_mahjong_by_position(Banker),
	% MahjongPlayer = MahjongPlayer0#mahjong_fight_info{basis = [{12,1},{13,1},{16,4},{33,3},{26,3},{22,2}]},
	
	#mahjong_fight_info{
					player_id = PlayerId,
					basis = Basis,
					play_limit = Limit
	} = MahjongPlayer,
	
	%% 补完花 可能可以胡牌
	%% 检测是否可以胡牌
	case lib_mahjong_fight_util:mahjong_fight_win_check(Basis) of
		{true, _CombinationList} ->
			MahjongPlayer1 = MahjongPlayer#mahjong_fight_info{flag = ?FLAG_CANWIN};
		_ ->
			MahjongPlayer1 = MahjongPlayer
	end,
	%% 检查听牌
	case lib_mahjong_fight_util:mahjong_fight_ready_hand_check(Basis) of
		false ->
			MahjongPlayer2 = MahjongPlayer1;
		ReadyList ->
			MahjongPlayer2 = MahjongPlayer1#mahjong_fight_info{canwin = ReadyList}
	end,

	%% 检查暗杠
	case lib_mahjong_fight_util:mahjong_fight_concealed_kong_check(MahjongPlayer2) of
		false ->
			MahjongPlayer3 = MahjongPlayer2;
		CanMeld ->
			MahjongPlayer3 = MahjongPlayer2#mahjong_fight_info{canmeld = CanMeld}
	end,
	%% 提示自己
	lib_mahjong_fight_util:refresh_mahjong_fight_info(MahjongPlayer3),
	MahjongId = lib_mahjong_fight_util:get_mahjong_fight_random_play_mahjong_id(Limit, Basis),
	%% 等待玩家操作
	%% 一开始是以为有托管的，现在没有，直接在函数中
	change_mahjong_wait_ref(mahjong_play , [PlayerId, MahjongId]),
	ok.
	


combine_mahjong_list1([], FlowerNum, _, Flower, NewPlayerBasisList) ->
	%% 直接结算，流局
	{[], FlowerNum, Flower, NewPlayerBasisList};
combine_mahjong_list1(BasisList, FlowerNum, [], Flower, NewPlayerBasisList) ->
	{BasisList, FlowerNum, Flower, lists:ukeysort(1, NewPlayerBasisList)};
combine_mahjong_list1(BasisList, FlowerNum, [{_E, Num}|PlayerBasisList], Flower, NewPlayerBasisList) when Num =< 0 ->
	combine_mahjong_list1(BasisList, FlowerNum, PlayerBasisList, Flower, NewPlayerBasisList);
combine_mahjong_list1([E0|BasisList], FlowerNum, [{E1, Num}|PlayerBasisList], Flower, NewPlayerBasisList) ->
	if
		?IS_FLOWER(E1) andalso ?IS_FLOWER(E0) ->
			combine_mahjong_list1(BasisList, FlowerNum, [{E1,Num}|PlayerBasisList], [E0|Flower], NewPlayerBasisList);
		?IS_FLOWER(E1) ->
			case lists:keyfind(E0, 1, NewPlayerBasisList) of
				false ->
					NewPlayerBasisList1 = [{E0, 1}|NewPlayerBasisList];
				{E0, Value} ->
					NewPlayerBasisList1 = lists:keystore(E0, 1, NewPlayerBasisList, {E0, Value + 1})
			end,
			combine_mahjong_list1(BasisList, FlowerNum + 1, [{E1, Num-1}|PlayerBasisList], [E1|Flower], NewPlayerBasisList1);
		true ->
			case lists:keyfind(E1, 1, NewPlayerBasisList) of
				false ->
					NewPlayerBasisList1 = [{E1, Num}|NewPlayerBasisList];
				{E1, Value} ->
					NewPlayerBasisList1 = lists:keystore(E1, 1, NewPlayerBasisList, {E1, Value + Num})
			end,
			combine_mahjong_list1([E0|BasisList], FlowerNum, PlayerBasisList, Flower, NewPlayerBasisList1)
	end.



%% 抓牌 DrawType = true 杠后抓牌，用来自摸杠上花，DrawType = false  是的抓牌
mahjong_draw(DrawType) ->
	case pre_mahjong_draw(DrawType) of
		{true, Dict} ->
			FunChange = dict:fetch('fun_change', Dict),

			%% 执行
			FunChange();
		{false, _Reason} ->
			skip
	end.
pre_mahjong_draw(DrawType) ->
	CheckList = [
				'change'  %% 值完色子 会发牌
				],
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	Dict = dict:from_list([
							{'draw_type', DrawType},
							{'mahjong_state', MahjongState},
							{'mahjong_player_list', MahjongPlayerList}
							]),
	pre_mahjong_draw_check(Dict, CheckList).

pre_mahjong_draw_check(Dict, []) ->
	{true, Dict};
pre_mahjong_draw_check(Dict, ['change'|Z]) ->
	DrawType = dict:fetch('draw_type', Dict),
	MahjongState = dict:fetch('mahjong_state', Dict),
	#r_mahjong_fight_state{
						is_end = IsEnd,
						basis_list = BasisList,
						now_postion = NowPosition
	} = MahjongState,
	%% 检测流局 之前有限制，现在没有
	LimitCount = 0,%?FLOWER_WILD_NUM(FlowerNum) + 2 * KongNum,
	IsDrop = mahjong_dropped(),
	if
		IsEnd ->
			{false, "牌局已经结束"};
		IsDrop ->
			{false, "有人掉线中，不能抓牌"};
		length(BasisList) =< LimitCount -> %% 流局
			NewMahjongState = MahjongState#r_mahjong_fight_state{
															is_end = true

			},
			FunChange = fun() ->
							%% 直接结束
							lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
							%% 通关结束
							lib_mahjong_fight_msg:send_mahjong_fight_state(),
							%% 流局
							mahjong_flow()
						end,
			Dict1 = dict:store('fun_change', FunChange, Dict),
			pre_mahjong_draw_check(Dict1, Z);
		true ->
			case lib_mahjong_fight_procdict:get_mahjong_by_position(NowPosition) of
				false ->
					{false, "数据异常"};
				MahjongPlayer ->
					Length = lib_mahjong_fight_util:get_mahjong_fight_basis_length(MahjongPlayer#mahjong_fight_info.basis),
					if
						Length rem 3 =/= 1 ->
							{false, "这个时候不应该抓牌"};
						true ->
							{MahjongId, NewBasisList} = 
								if
									DrawType ->
										MahjongId1 = lists:last(BasisList),
										NewBasisList1 = lists:droplast(BasisList),
										{MahjongId1, NewBasisList1};
									true ->
										[MahjongId1|NewBasisList1] = BasisList,
										{MahjongId1, NewBasisList1}
								end,
							NewMahjongState = MahjongState#r_mahjong_fight_state{
																		basis_list = NewBasisList

							},
							case lists:keyfind(MahjongId, 1, MahjongPlayer#mahjong_fight_info.basis) of
								false ->
									NewBasis = lists:ukeysort(1, [{MahjongId, 1}|MahjongPlayer#mahjong_fight_info.basis]);
								{MahjongId, Num} ->
									NewBasis = lists:keystore(MahjongId, 1, MahjongPlayer#mahjong_fight_info.basis, {MahjongId, Num + 1})
							end,
							NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{
																			basis = NewBasis, 
																			is_kong = DrawType,
																			mahjong_id = MahjongId
																			},
							FunChange = fun() ->
											%% 更新状态
											lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
											%% 更新玩家信息
											lib_mahjong_fight_procdict:update_mahjong_by_player_id(NewMahjongPlayer),
											%% 通知玩家更新牌
											lib_mahjong_fight_msg:send_mahjong_fight_info(NewMahjongPlayer),
											%% 新抓的牌
											lib_mahjong_fight_msg:send_mahjong_fight_state(),
											%%io:format("_____draw~p~n", [{MahjongId, NewMahjongPlayer#mahjong_fight_info.player_id}]),
											%% 摸完牌判断是否补花，是否能胡，是否听牌 是否能杠 否则等待玩家出牌
											mahjong_draw_after_check(DrawType, MahjongId, NewMahjongPlayer),
											ok
										end,
							Dict1 = dict:store('fun_change', FunChange, Dict),
							pre_mahjong_draw_check(Dict1, Z)
					end
			end
	end;
pre_mahjong_draw_check(Dict, [_|Z]) ->
	pre_mahjong_draw_check(Dict, Z).
%% 抓完牌，检测
mahjong_draw_after_check(DrawType, MahjongId, MahjongPlayer) ->
	#mahjong_fight_info{
					flower = Flower,
					basis = Basis
					% flag = Flag,
					% meld = Meld
	} = MahjongPlayer,
	if
		?IS_FLOWER(MahjongId) ->
			NewBasis = lists:keydelete(MahjongId, 1, Basis),
			NewFlower = [MahjongId|Flower],
			NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{
														basis = NewBasis,
														flower = NewFlower
			},
			% lib_mahjong_fight_procdict:update_mahjong_by_player_id(NewMahjongPlayer),
			lib_mahjong_fight_util:refresh_mahjong_fight_info(NewMahjongPlayer),
			%%io:format("____is_flower~p~n",[MahjongId]),
			mahjong_draw(DrawType);
		true ->
			%% 检测是否可以胡牌
			case lib_mahjong_fight_util:mahjong_fight_win_check(Basis) of
				{true, _CombinationList} ->
					MahjongPlayer1 = MahjongPlayer#mahjong_fight_info{flag = ?FLAG_CANWIN};
				_ ->
					MahjongPlayer1 = MahjongPlayer
			end,
			%% 检查听牌
			case lib_mahjong_fight_util:mahjong_fight_ready_hand_check(Basis) of
				false ->
					MahjongPlayer2 = MahjongPlayer1;
				ReadyList ->
					MahjongPlayer2 = MahjongPlayer1#mahjong_fight_info{canwin = ReadyList}
			end,

			%% 检查暗杠
			case lib_mahjong_fight_util:mahjong_fight_concealed_kong_check(MahjongPlayer2) of
				false ->
					MahjongPlayer3 = MahjongPlayer2;
				CanMeldList ->
					MahjongPlayer3 = MahjongPlayer2#mahjong_fight_info{canmeld = CanMeldList}
			end,
			%% 检查明杠
			case lib_mahjong_fight_util:mahjong_fight_exposed_kong_check(MahjongPlayer3) of
				false ->	
					MahjongPlayer4 = MahjongPlayer3;
				CanMeldList1 ->
					MahjongPlayer4 = MahjongPlayer3#mahjong_fight_info{canmeld = MahjongPlayer3#mahjong_fight_info.canmeld ++ CanMeldList1}
			end,
			%% 提示自己
			lib_mahjong_fight_util:refresh_mahjong_fight_info(MahjongPlayer4),
			change_mahjong_wait_ref(mahjong_play, [MahjongPlayer#mahjong_fight_info.player_id, MahjongId]),
			ok
	end.

%% 出牌
mahjong_play(PlayerId, MahjongId) ->
	case pre_mahjong_play(PlayerId, MahjongId) of
		{true, Dict} ->
			FunChange = dict:fetch('fun_change', Dict),
			FunChange(),
			ok;
		{false, Reason} ->
			ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_play, false, Reason),
			lib_mahjong_fight_msg:send_mahjong_fight_reply(PlayerId, ReplyMsg)
	end.
%% 顺序是顺序检测其他玩家能不能胡牌，碰杠 之后优先级最低的是下家检测吃，不能吃下家抓牌
pre_mahjong_play(PlayerId, MahjongId) ->
	CheckList = [
				'state',
				'player',     %% 获取玩家，胡，碰，杠，吃的变化函数
				'change'        %% 下家吃 
				],
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	Dict = dict:from_list([
						{'player_id', PlayerId},
						{'mahjong_state', MahjongState},
						{'mahjong_player_list', MahjongPlayerList},
						{'mahjong_id', MahjongId}
						]),
	pre_mahjong_play_check(Dict, CheckList).
pre_mahjong_play_check(Dict, []) ->
	{true, Dict};
pre_mahjong_play_check(Dict, ['state'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	IsEnd = MahjongState#r_mahjong_fight_state.is_end,
	IsWin = MahjongState#r_mahjong_fight_state.is_win,
	IsDrop = mahjong_dropped(),
	if
		IsEnd ->
			{false, "牌局已经结束"};
		IsWin ->
			{false, "已经有玩家选择胡牌，不能再出牌"};
		IsDrop ->
			{false, "有玩家掉线,不能出牌"};
		true ->
			pre_mahjong_play_check(Dict, Z)
	end;
pre_mahjong_play_check(Dict, ['player'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	PlayerId = dict:fetch('player_id', Dict),
	% MahjongPlayerList = dict:fetch('mahjong_player_list', Dict),
	MahjongId = dict:fetch('mahjong_id', Dict),
	NowPosition = MahjongState#r_mahjong_fight_state.now_postion,
	OutList = MahjongState#r_mahjong_fight_state.out_list,
	IsEnd = MahjongState#r_mahjong_fight_state.is_end,
	%%io:format("!!!~p~n",[{NowPosition, PlayerId, lib_mahjong_fight_procdict:get_mahjong_by_position(NowPosition)}]),
	% NowPosition = ?GET_NEXT_POSITION(MahjongState#r_mahjong_fight_state.position),
	case lib_mahjong_fight_procdict:get_mahjong_by_position(NowPosition) of
		false ->
			{false, "没有该玩家"};
		MahjongPlayer when MahjongPlayer#mahjong_fight_info.player_id =/= PlayerId ->
			{false, io_lib:format("没轮到玩家~p出牌", [PlayerId])};
		_MahjongPlayer when IsEnd == true ->
			{false, "牌局已经结束"};
		MahjongPlayer ->
			IsVaid = lists:member(MahjongId, MahjongPlayer#mahjong_fight_info.play_limit),
			Length = lib_mahjong_fight_util:get_mahjong_fight_basis_length(MahjongPlayer#mahjong_fight_info.basis),
			if
				IsVaid ->
					{false, "吃碰牌没有过圈，不能吃碰什么出什么"};
				Length rem 3 =/= 2 ->
					{false, "手上牌不足或没轮到你出牌，不能出牌"};
				true ->
					case lists:keyfind(MahjongId, 1, MahjongPlayer#mahjong_fight_info.basis) of
						{MahjongId, Num} when Num > 0 ->	
							NewMahjongState = 
									MahjongState#r_mahjong_fight_state{
																last_position = NowPosition,
																last_mahjongid = MahjongId,
																now_postion = ?GET_NEXT_POSITION(NowPosition), %% 下一个玩家，有碰再改
																out_list = [MahjongId|OutList],
																is_kong = false
															},
							Basis = lists:keystore(MahjongId, 1, MahjongPlayer#mahjong_fight_info.basis, {MahjongId, Num -1}),
							Out = [MahjongId|MahjongPlayer#mahjong_fight_info.out],
							Flag = ?CONDITIONAL_EXPRESSION(MahjongPlayer#mahjong_fight_info.flag == ?FLAG_CANWIN, ?FLAG_NORMAL, MahjongPlayer#mahjong_fight_info.flag),
							NewMahjongPlayer = 
									MahjongPlayer#mahjong_fight_info{
															basis = Basis,
															out = Out,
															canmeld = [],
															canwin = [],
															limit = [],
															play_limit = [],
															play_mahjong_id = MahjongId,
															flag = Flag
														},
							Dict1 = dict:store('mahjong_state', NewMahjongState, Dict),
							Dict2 = dict:store('mahjong_player', NewMahjongPlayer, Dict1),
							pre_mahjong_play_check(Dict2, Z);
						_ ->
							{false, "玩家没有相应的牌"}
					end
			end
	end;
pre_mahjong_play_check(Dict, ['change'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	MahjongPlayer = dict:fetch('mahjong_player', Dict),
	MahjongId = dict:fetch('mahjong_id', Dict),
	FunChange = fun() ->
					%% 更新状态信息
					lib_mahjong_fight_procdict:update_dic_magjong_fight_state(MahjongState#r_mahjong_fight_state{circle_flag = 1}),
					%% 玩家出牌后信息变更
					lib_mahjong_fight_procdict:update_mahjong_by_player_id(MahjongPlayer),
					%% 执行通知客户端碰吃等 循环 检测其他玩家是否可以碰吃
					ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_play, true, ""),
					lib_mahjong_fight_msg:send_mahjong_fight_reply(MahjongPlayer#mahjong_fight_info.gate_pid, ReplyMsg),
					lib_mahjong_fight_msg:send_mahjong_fight_info(MahjongPlayer),
					lib_mahjong_fight_msg:send_mahjong_fight_state(),
					lib_chat:send_common_notice(util:erlang_list_to_string(["o", MahjongId, MahjongPlayer#mahjong_fight_info.player_id,MahjongPlayer#mahjong_fight_info.player_id])),
					%%io:format("____play~p~n",[MahjongId]),
					change_mahjong_wait_ref(mahjong_cancel, []),
					%% 然后其他玩家检测是否可吃，可胡，可碰，可杠等
					mahjong_play_after_check(),
					ok
				end,
	Dict1 = dict:store('fun_change', FunChange, Dict),
	pre_mahjong_play_check(Dict1, Z);
pre_mahjong_play_check(Dict, [_|Z]) ->
	pre_mahjong_play_check(Dict, Z).
	
%%检测其他玩家是否可以碰吃 会循环调用 现在改成所有吃碰杠都要显示
% 玩家出牌后，检测
mahjong_play_after_check() ->
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	F = fun(E, Acc) ->
			R = mahjong_play_after_check(E, MahjongState),
			if
				Acc == true ->
					true;
				R ->
					true;
				true ->
					Acc
			end
		end,
	case lists:foldl(F, false, MahjongPlayerList) of
		true ->
			skip;
		_ ->
			mahjong_draw(flase)
	end.
mahjong_play_after_check(MahjongPlayer, MahjongState)  ->
	#r_mahjong_fight_state{
						last_mahjongid = LastMahjongId,
						last_position = LastPosition
	} = MahjongState,
	#mahjong_fight_info{
					basis = EBasis,
					position = EPosition,
					flag = EFlag,
					limit = Limit,
					meld = Meld
	} = MahjongPlayer,

	IsVaid = lists:member(LastMahjongId, Limit), %% 如果上一把没胡，这一把也不能胡 过自己算一圈
	case LastPosition =/= EPosition andalso IsVaid == false of
		true ->
			LastPlayerId = lib_mahjong_fight_procdict:get_mahjong_player_id_by_position(LastPosition),
			NextPosition = ?GET_NEXT_POSITION(LastPosition),
			%% 检测胡牌 自摸肯定有台数，所以只有出牌后，放炮是没有台数的
			case lib_mahjong_fight_util:mahjong_fight_win_check(LastMahjongId, EBasis) of
				{false, _Reason} ->
					MahjongPlayer1 = MahjongPlayer;
				{true, CombinationList} ->
					Combination = lib_mahjong_fight_util:get_mahjong_fight_combination(CombinationList, Meld),
					case lib_mahjong_fight_util:get_mahjong_fight_mahjong_count(Combination, false,false, LastMahjongId, MahjongPlayer, MahjongState) of
						{Count, _} when Count =< 0 ->
							MahjongPlayer1 = MahjongPlayer;
						_ ->
							MahjongPlayer1 = MahjongPlayer#mahjong_fight_info{flag = ?FLAG_CANWIN}
					end
			end,
			%% 检测杠
			case lib_mahjong_fight_util:mahjong_fight_exposed_kong_check(LastMahjongId, LastPlayerId, EBasis) of
				false  ->
					MahjongPlayer2 = MahjongPlayer1;
				{true, CanMeld} ->
					MahjongPlayer2 = MahjongPlayer1#mahjong_fight_info{canmeld = [CanMeld|MahjongPlayer1#mahjong_fight_info.canmeld]}
			end,
			%% 检查碰
			case lib_mahjong_fight_util:mahjong_fight_pong_check(LastMahjongId, EBasis) andalso EFlag == ?FLAG_NORMAL of
				false ->
					MahjongPlayer3 = MahjongPlayer2;
				true ->
					EPong = {{?MAHJONG_MELD_TYPE_PONG, LastMahjongId},LastPlayerId, {LastMahjongId, LastMahjongId, LastMahjongId}},
					MahjongPlayer3 = MahjongPlayer2#mahjong_fight_info{canmeld = [EPong|MahjongPlayer2#mahjong_fight_info.canmeld]}
			end,
			%% 检测吃牌
			case lib_mahjong_fight_util:get_mahjong_fight_eat(LastMahjongId, LastPlayerId, EBasis) of
				EatMeld when EatMeld =/= [] andalso NextPosition == EPosition andalso EFlag == ?FLAG_NORMAL  ->

					MahjongPlayer4 = MahjongPlayer3#mahjong_fight_info{canmeld = EatMeld ++ MahjongPlayer3#mahjong_fight_info.canmeld};
				_ ->
					MahjongPlayer4 = MahjongPlayer3
			end,
			%% 执行通知玩家
			lib_mahjong_fight_util:refresh_mahjong_fight_info(MahjongPlayer4),
			MahjongPlayer4 =/= MahjongPlayer;
		_ ->
			false
	end.


%% 胡牌
mahjong_win(PlayerId) ->
	%%io:format("___mahjong_win~n"),
	case pre_mahjong_win(PlayerId) of
		{true, Dict} ->
			FunChange = dict:fetch('fun_change', Dict),
			FunChange(),
			ok;
		{false, Reason} ->
			ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_win, false, Reason),
			lib_mahjong_fight_msg:send_mahjong_fight_reply(PlayerId, ReplyMsg)
	end,
	ok.
pre_mahjong_win(PlayerId) ->
	CheckList = [
				'player',	  %% 会检测直接自摸， 如果不是，检测放炮 
				'shoot',      %% 放炮
				'score',     %% 计算胡牌类型 与计算积分
				'change'
				],
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	Dict = dict:from_list([
						{'mahjong_state', MahjongState},
						{'mahjong_player_list', MahjongPlayerList},
						{'player_id', PlayerId}
						]),
	pre_mahjong_win_check(Dict, CheckList).
pre_mahjong_win_check(Dict, []) ->
	{true, Dict};
pre_mahjong_win_check(Dict, ['player'|Z]) ->
	PlayerId  = dict:fetch('player_id', Dict),
	MahjongState = dict:fetch('mahjong_state', Dict),
	case lib_mahjong_fight_procdict:get_mahjong_by_player_id(PlayerId) of
		false ->
			{false, "玩家没有参与牌局"};
		_MahjongPlayer when MahjongState#r_mahjong_fight_state.is_end == true ->
			{false, "上一个玩家胡牌后，你有5秒钟选择胡牌，否则认为你放弃，牌局结束"}; %% 如果你在第一个结算之后仍没点胡牌， 那么算你放弃
		MahjongPlayer when MahjongPlayer#mahjong_fight_info.flag == ?FLAG_WINED ->
			{false, "已经选择胡牌，等待结算中"};
		MahjongPlayer ->
			#mahjong_fight_info{
							basis = Basis,
							mahjong_id = WinId,
							meld = Meld,
							% flower = Flower,
							is_kong = IsKong
			} = MahjongPlayer,
			case lib_mahjong_fight_util:mahjong_fight_win_check(Basis) of
				{true, CombinationList} ->
					Combination = lib_mahjong_fight_util:get_mahjong_fight_combination(CombinationList, Meld),
					{MahjongCount, WinTypeList} = lib_mahjong_fight_util:get_mahjong_fight_mahjong_count(Combination, true,IsKong, WinId, MahjongPlayer, MahjongState),
					IsTouch = true;
				_ ->
					MahjongCount = 0,
					IsTouch = false,
					Combination = [],
					WinTypeList = []
			end,
			Dict1 = dict:store('mahjong_player', MahjongPlayer#mahjong_fight_info{flag = ?FLAG_WINED}, Dict),
			Dict2 = dict:store('is_touch', IsTouch, Dict1),
			Dict3 = dict:store('mahjong_count', MahjongCount, Dict2),
			Dict4 =dict:store('combination', Combination, Dict3),
			Dict5 = dict:store('win_type_list', WinTypeList, Dict4),
			pre_mahjong_win_check(Dict5, Z)
	end;
pre_mahjong_win_check(Dict, ['shoot'|Z]) ->
	IsTouch  = dict:fetch('is_touch', Dict),
	MahjongPlayer = dict:fetch('mahjong_player', Dict),
	MahjongState = dict:fetch('mahjong_state', Dict),
	MahjongPlayerList = dict:fetch('mahjong_player_list', Dict),
	TouchCombination = dict:fetch('combination', Dict),
	TouchWinTypeList = dict:fetch('win_type_list', Dict),
	MahjongCount = dict:fetch('mahjong_count', Dict),
	#mahjong_fight_info{
						player_id = PlayerId,
						basis = Basis,
						position = Position,
						limit = Limit,
						meld = Meld,
						mahjong_id = MahjongId
	} = MahjongPlayer,
	#r_mahjong_fight_state{
					banker = Banker,
					win_position = WinPosition,
					combination = OldCombination,
					meld = OldMeld,
					type_list = OldWinTypeList,
					win_id = OldWinId,
					count = OldMahjongCount,
					last_mahjongid = LastMahjongId,
					last_position = LastPosition
	} = MahjongState,

	if
		IsTouch -> %% 如果是四模
			F = fun(E, {AccFail, AccPlayer}) ->
					if
						E#mahjong_fight_info.player_id == PlayerId ->
							{AccFail, [E#mahjong_fight_info{end_type = 2, flag = ?FLAG_WINED}|AccPlayer]};
						true ->
							NewE = E#mahjong_fight_info{flag = ?FLAG_FAIL, end_type = 4},
							{[NewE|AccFail], [NewE|AccPlayer]}
					end
				end,
			{FailPlayerList, MahjongPlayerList1} = lists:foldl(F, {[], []}, MahjongPlayerList),
			NewMahjongState = MahjongState#r_mahjong_fight_state{
															is_win = true,
															win_position = Position, 
															combination = TouchCombination,
															meld = Meld, 
															type_list = TouchWinTypeList, 
															win_id = MahjongId, 
															count = MahjongCount
															},
			Content = util:erlang_list_to_string(["w", MahjongId, PlayerId, PlayerId]),
			Dict1 = dict:store('mahjong_state', NewMahjongState, Dict),
			Dict2 = dict:store('fail_mahjong_player', FailPlayerList, Dict1),
			Dict3 = dict:store('is_kong', MahjongPlayer#mahjong_fight_info.is_kong, Dict2),
			Dict4 = dict:store('content', Content, Dict3),
			Dict5 = dict:store('mahjong_player_list', MahjongPlayerList1, Dict4),
			pre_mahjong_win_check(Dict5, Z);
		true ->
			%% 检查拉杠胡
			IsVaid = lists:member(LastMahjongId, Limit),%% 拉杠
			if
				Position == LastPosition ->
					{false, "不能胡自己出的牌"};
				IsVaid ->
					{false, "放弃胡这张牌没过圈，不能胡"};
				true ->
					case lib_mahjong_fight_util:mahjong_fight_win_check(LastMahjongId, Basis) of
						{true, CombinationList} ->
							LastPositionPlayer = lib_mahjong_fight_procdict:get_mahjong_by_position(LastPosition),
							IsKong = LastPositionPlayer#mahjong_fight_info.is_kong,

							Combination = lib_mahjong_fight_util:get_mahjong_fight_combination(CombinationList, Meld),
							case lib_mahjong_fight_util:get_mahjong_fight_mahjong_count(Combination, IsTouch,IsKong, LastMahjongId, MahjongPlayer, MahjongState) of
								{MahjongCount1, _WinTypeList} when MahjongCount1 =< 0 ->
									{false, "台数至少达到1台才可以胡牌"};
								{MahjongCount1, WinTypeList} ->
									FailPlayer = LastPositionPlayer#mahjong_fight_info{flag = ?FLAG_FAIL, end_type = 1},
									FailPlayerList = [FailPlayer],
									MahjongPlayerList1 = lists:keystore(FailPlayer#mahjong_fight_info.player_id, #mahjong_fight_info.player_id, MahjongPlayerList, FailPlayer),
									%% 如果是一炮多响，那理被胡牌最近的赢牌的玩家做下一个庄家
									if
										?IS_POSITION(WinPosition) ->
											NewWinPosition = ?CONDITIONAL_EXPRESSION(?GET_DISTANCE_POSITION(Banker, WinPosition) > ?GET_DISTANCE_POSITION(Banker, Position), Position, WinPosition),
											NewCombination = ?CONDITIONAL_EXPRESSION(NewWinPosition =/= WinPosition, Combination, OldCombination),
											NewMeld = ?CONDITIONAL_EXPRESSION(NewWinPosition =/= WinPosition, Meld, OldMeld),
											NewTypeList = ?CONDITIONAL_EXPRESSION(NewWinPosition =/= WinPosition, WinTypeList, OldWinTypeList),
											NewWinId = ?CONDITIONAL_EXPRESSION(NewWinPosition =/= WinPosition, LastMahjongId, OldWinId),
											NewMahjongCount = ?CONDITIONAL_EXPRESSION(NewWinPosition =/= WinPosition, MahjongCount1, OldMahjongCount);
										true ->
											NewWinPosition = Position,
											NewCombination = Combination,
											NewMeld = Meld,
											NewTypeList = WinTypeList,
											NewWinId = LastMahjongId,
											NewMahjongCount = MahjongCount1
									end,
									NewMahjongState = MahjongState#r_mahjong_fight_state{
																						is_win = true, 
																						banker = NewWinPosition,
																						win_position = NewWinPosition, 
																						combination = NewCombination,
																						meld = NewMeld, 
																						type_list = NewTypeList, 
																						win_id = NewWinId,
																						count = NewMahjongCount
																						},
									Content = util:erlang_list_to_string(["s", LastMahjongId, LastPositionPlayer#mahjong_fight_info.player_id, LastPositionPlayer#mahjong_fight_info.player_id]),
									Dict1 = dict:store('mahjong_state', NewMahjongState, Dict),
									Dict2 = dict:store('fail_mahjong_player', FailPlayerList, Dict1),
									Dict3 = dict:store('is_kong', IsKong, Dict2),
									Dict4 = dict:store('mahjong_count', MahjongCount1, Dict3),
									Dict5 = dict:store('content', Content, Dict4),
									Dict6 = dict:store('mahjong_player_list', MahjongPlayerList1, Dict5),
									pre_mahjong_win_check(Dict6, Z)
							end;
						{false, _R} ->
							{false, "不能胡牌"}
					end
			end
	end;
pre_mahjong_win_check(Dict, ['change'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	MahjongPlayer = dict:fetch('mahjong_player', Dict),
	IsKong = dict:fetch('is_kong', Dict),
	IsTouch = dict:fetch('is_touch', Dict),
	FailPlayerList = dict:fetch('fail_mahjong_player', Dict),
	MahjongCount = dict:fetch('mahjong_count', Dict),
	MahjongPlayerList = dict:fetch('mahjong_player_list', Dict),
	Content = dict:fetch('content', Dict),
	% NewMahjongState = MahjongState#r_mahjong_fight_state{is_end = true},
	%% 因为输家也有可能一炮双响，只需要把玩家
	NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{
													flag = ?FLAG_WINED,
													canmeld = [],
													canwin = [],
													end_type = 2
	},
	MahjongPlayerList1 = lists:keystore(MahjongPlayer#mahjong_fight_info.player_id, #mahjong_fight_info.player_id, MahjongPlayerList, NewMahjongPlayer),
	%% 计算每个玩家获得与减少的积分 单独计算 比如AB 互包 c放炮 赢6*M	输4*M	输2*M，然后A先胡，接着b胡，最后AB直赢C
	NewMahjongPlayerList = lib_mahjong_fight_util:get_mahjong_fight_mahjong_score(IsTouch, IsKong, MahjongCount, MahjongPlayer, FailPlayerList, MahjongPlayerList1, MahjongState),

	FunChange = fun() ->
					lib_mahjong_fight_util:refresh_mahjong_fight_info(NewMahjongPlayerList),
					lib_mahjong_fight_procdict:update_dic_magjong_fight_state(MahjongState),

					ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_win, true, ""),
					lib_mahjong_fight_msg:send_mahjong_fight_reply(MahjongPlayer#mahjong_fight_info.gate_pid, ReplyMsg),
					
					%% 取消发牌的定时
					cancel_mahjong_wait_ref(),
					%% 取消其他玩家的碰吃杠
					mahjong_canmeld(),
					%% 发送定时时间为0
					lib_mahjong_fight_msg:send_mahjong_fight_left_time(0, MahjongPlayer#mahjong_fight_info.player_id, MahjongPlayer#mahjong_fight_info.position),
					%% 提示胡牌
					lib_chat:send_common_notice(Content),
					%% 检测如果还有其他玩家可以胡牌，不结算
					mahjong_canwin(),
					% make_mahjong_start_ref(mahjong_fight_end, []),
					ok
				end,
	Dict1 = dict:store('fun_change', FunChange,Dict),
	pre_mahjong_win_check(Dict1, Z);
pre_mahjong_win_check(Dict, [_|Z]) ->
	pre_mahjong_win_check(Dict, Z).

%% 流局
mahjong_flow() ->
%%流局时，如果庄家有明杠或暗杠，则庄家轮到下一家。反之则继续坐庄。
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	#r_mahjong_fight_state{
						now_postion = NowPosition,
						banker = Banker,
						feng_circle = FengCircle,
						banker_list = BankerList
	} = MahjongState,
	if
		MahjongState#r_mahjong_fight_state.is_end ->
			skip;
		true ->
			NowPlayerId = lib_mahjong_fight_procdict:get_mahjong_player_id_by_position(NowPosition),
			BankerPlayer = lib_mahjong_fight_procdict:get_mahjong_by_position(Banker),
			if
				is_record(BankerPlayer, mahjong_fight_info) ->
					F = fun({{EType, _MahjongId} , _, _}, Acc) ->
							if
								Acc == true ->
									true;
								EType == ?MAHJONG_MELD_TYPE_EXPOSED_KONG ->
									true;
								EType == ?MAHJONG_MELD_TYPE_CONCEALED_KONG ->
									true;
								true ->
									Acc
							end
						end,
					%% 庄家有杠下一家坐庄
					case lists:foldl(F, false, BankerPlayer#mahjong_fight_info.meld) of
						false ->
							NewBanker = Banker,
							NewFengCircle = FengCircle;
						true ->
							NewBanker = ?GET_NEXT_POSITION(Banker),
							NewFengCircle = ?CONDITIONAL_EXPRESSION(length(BankerList) >= ?MAX_POSITION_NUM, ?GET_NEXT_POSITION(FengCircle), FengCircle)
					end;
				true ->
					NewBanker = ?GET_NEXT_POSITION(Banker),
					NewFengCircle = ?CONDITIONAL_EXPRESSION(length(BankerList) >= ?MAX_POSITION_NUM, ?GET_NEXT_POSITION(FengCircle), FengCircle)
			end,
			io:format("mahjong_flow~p~n",[{Banker, NewBanker}]),
			%% 流局可以直接确定新庄家
			NewMahjongState = MahjongState#r_mahjong_fight_state{
																banker = NewBanker,
																feng_circle = NewFengCircle
																},
			lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
			%% 去掉提示
			mahjong_canmeld(),
			%% 取消玩牌的定时
			cancel_mahjong_wait_ref(),
			%%
			lib_mahjong_fight_msg:send_mahjong_fight_left_time(0, NowPlayerId, NowPosition),
			%% 结算
			mahjong_fight_end()
	end,
	ok.
%% 结算
mahjong_fight_end() ->
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	CommonRecord = lib_mahjong_fight_procdict:get_dic_mahjong_common_record(),
	F = fun(E, {AccRecordList, AccMahjongPlayerList}) ->
			#mahjong_fight_info{
							player_id = EPlayerId,
							player_name = EPlayerName,
							player_icon = EPlayerIcon,
							player_pid = EPlayerPid,
							flag = EFlag,
							score = EScore,
							state = EState,
							re_ref = Ref
			} = E,
			%% 去掉玩家身上的定时
			cancel_mahjong_fight_ref(Ref),
			gen_server:cast(EPlayerPid, {'handle', lib_mahjong_fight_player, notice_mahjong_fight_end, [CommonRecord#common_record.id, CommonRecord#common_record.owner_id,  EFlag, EScore]}),
			case lists:keyfind(EPlayerId, #r_record_member.player_id, AccRecordList) of
				false ->
					R = #r_record_member{
									player_id = EPlayerId,
									player_name = EPlayerName,
									player_icon = EPlayerIcon,
									score = EScore
					};
				Member ->
					R = Member#r_record_member{score = Member#r_record_member.score + EScore}
			end,
			if
				EState =/= ?STATE_LEAVEL ->
					NewAccMahjongPlayerList = [E#mahjong_fight_info{state = ?STATE_IN}|AccMahjongPlayerList];
				true ->
					NewAccMahjongPlayerList = AccMahjongPlayerList
			end,
			if
				R#r_record_member.score >= 0 ->
					NewRecord = R#r_record_member{win_flag = true};
				true ->
					NewRecord = R#r_record_member{win_flag = false}
			end,
			NewAccRecordList = lists:keystore(EPlayerId, #r_record_member.player_id, AccRecordList, NewRecord),
			{NewAccRecordList, NewAccMahjongPlayerList}
		end,
	{NewRecordList, NewMahjongPlayerList} = lists:foldl(F, {CommonRecord#common_record.info, []}, MahjongPlayerList),
	%% 如果是有玩家赢，也就是怕有多家胡牌，所以还没确定新庄家
	% Banker = MahjongState#r_mahjong_fight_state.banker,
	#r_mahjong_fight_state{
						is_win = IsWin,
						% win_position = WinPosition,
						banker = Banker,
						banker_list = BankerList,
						feng_circle = FengCircle
	} = MahjongState,
	%% 拿出庄家的下一位置
	BankerPlayer = lib_mahjong_fight_procdict:get_mahjong_by_position(Banker),
	BankerWinFlag = BankerPlayer#mahjong_fight_info.flag,
	if
		IsWin andalso BankerWinFlag == ?FLAG_WINED -> %% 牌局没有流局，庄家赢，继续坐庄
			NewBanker = Banker,
			NewFengCircle = FengCircle;
		IsWin -> %% 赢牌，但庄家没赢，下一家
			NewBanker = ?GET_NEXT_POSITION(Banker),
			NewFengCircle = ?CONDITIONAL_EXPRESSION(length(BankerList) >= ?MAX_POSITION_NUM, ?GET_NEXT_POSITION(FengCircle), FengCircle);
		true -> %% 流局因为有特殊处理，流局时已经给了新庄家，以及圈
			NewBanker = Banker,
			NewFengCircle = FengCircle
	end,
	io:format("mahjong_fight_end~p~n",[{Banker, IsWin, BankerWinFlag, NewBanker, NewFengCircle}]),
	%% 结算改变状态， 要删除
	lib_room_info:change_room_state(MahjongState#r_mahjong_fight_state.room_id, false),
	%% 通知结束了
	NewMahjongState = MahjongState#r_mahjong_fight_state{
														banker = NewBanker, 
														feng_circle = NewFengCircle,
														last_position = 0,
														last_mahjongid = 0,
														is_end = true
														},
	NewCommonRecord = CommonRecord#common_record{info = NewRecordList},
	%% 结算协议
	lib_mahjong_fight_msg:send_mahjong_fight_end(),
	lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
	lib_mahjong_fight_msg:send_mahjong_fight_state(),
	lib_mahjong_fight_procdict:update_dic_mahjong_player_list(NewMahjongPlayerList),
	lib_mahjong_fight_msg:send_mahjong_fight_info(),
	% 进程里保存一份方便一些
	lib_mahjong_fight_procdict:update_dic_mahjong_common_record(NewCommonRecord),
	lib_mahjong_fight_msg:send_mahjong_fight_record(),
	%% 检测如果没有玩家了，关闭进程
	mahjong_stop(),
	ok.
%% 吃牌 
%% 暗杠
mahjong_meld(PlayerId, PbMeld) ->
	{Result, Reason} = 
	case lib_mahjong_fight_procdict:get_mahjong_by_player_id(PlayerId) of
		false ->
			{false, "未知玩家"};
		MahjongPlayer when MahjongPlayer#mahjong_fight_info.flag == ?FLAG_READY ->
			{false, "已经选择听牌"};
		MahjongPlayer ->
			%% 先去掉可以其他玩家可以胡牌
			%% 检查掉线
			IsDrop = mahjong_dropped(),
			MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
			F = fun(E, Acc) ->
					if
						Acc =/= true  ->
							Acc;
						E#mahjong_fight_info.player_id =/= PlayerId andalso E#mahjong_fight_info.flag == ?FLAG_CANWIN ->
							{false, "有其他玩家可以胡牌"};
						true ->
							Acc
					end
				end,
			case lists:foldl(F, true, MahjongPlayerList) of
				_R when IsDrop ->
					{false, "有玩家掉线， 不能吃碰杠"};
				true ->	
					MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
					#pb_mahjong_meld_info{
									type = Type,
									info = ValueList
					} = PbMeld,
					mahjong_meld1(Type, ValueList, MahjongPlayer, MahjongState);
				R ->
					R
			end
	end,
	%%io:format("mahjong_meld~p~n",[{Result, Reason}]),
	case Result of
		false ->
			ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_meld, Result, Reason),
			lib_mahjong_fight_msg:send_mahjong_fight_reply(PlayerId, ReplyMsg);
		_ ->
			skip
	end,
	ok.
%% 暗杠
mahjong_meld1(?MAHJONG_MELD_TYPE_CONCEALED_KONG, [MahjongId|_], MahjongPlayer, MahjongState) ->
	#mahjong_fight_info{
					player_id = PlayerId,
					gate_pid = GatePid,
					position = Position,
					meld = Meld,
					flag = Flag,
					basis = Basis
	} = MahjongPlayer,
	#r_mahjong_fight_state{
						now_postion = NowPosition
	} = MahjongState,
	if
		Position =/= NowPosition ->
			{false, "没有轮到你操作"};
		true ->
			case lists:keyfind(MahjongId, 1, Basis) of
				{MahjongId, Num} when Num >=4 ->
					NewMeld = [{{?MAHJONG_MELD_TYPE_CONCEALED_KONG,MahjongId}, PlayerId, {MahjongId,MahjongId, MahjongId, MahjongId}}|Meld],
					NewBasis = lists:keydelete(MahjongId, 1, Basis),
					NewFlag = ?CONDITIONAL_EXPRESSION(Flag == ?FLAG_CANWIN, ?FLAG_NORMAL, Flag),
					NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{
																	basis = NewBasis,
																	meld = NewMeld,
																	flag = NewFlag,
																	mahjong_id = 0,
																	canmeld = []
					},
					lib_mahjong_fight_util:refresh_mahjong_fight_info(NewMahjongPlayer),
					NewMahjongState = MahjongState#r_mahjong_fight_state{			% now_postion = Position, 
																		kong_num = MahjongState#r_mahjong_fight_state.kong_num + 1
																				},
					lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
					%% 先清理所有玩家可以碰的信息
					mahjong_canmeld(),
					%% 提示
					lib_chat:send_common_notice(util:erlang_list_to_string(["k", MahjongId, PlayerId, PlayerId])),
					%% 先返回成功，再抓牌
					ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_meld, true, ""),
					lib_mahjong_fight_msg:send_mahjong_fight_reply(GatePid, ReplyMsg),
					%% 杠抓牌
					mahjong_draw(true),
					{true, ""};
				_ ->   
					{false, io_lib:format("~p没有牌足够的牌可以暗杠",[MahjongId])}
			end
	end;
%% 明杠
mahjong_meld1(?MAHJONG_MELD_TYPE_EXPOSED_KONG, [MahjongId|_], MahjongPlayer, MahjongState) ->
	#mahjong_fight_info{
					player_id = PlayerId,
					gate_pid = GatePid,
					meld = Meld,
					position = Position,
					basis = Basis,
					flag = Flag
	} = MahjongPlayer,
	#r_mahjong_fight_state{
					last_mahjongid = LastMahjongId,
					last_position = LastPosition,
					now_postion = NowPosition,
					is_end = IsEnd
	} = MahjongState,
	Length = lib_mahjong_fight_util:get_mahjong_fight_basis_length(Basis),
	if
		IsEnd ->
			{false, "牌局已经结束了"};
		Position == NowPosition andalso Length rem 3 == 2 -> %% 新抓起来的牌的明杠
			case lists:keyfind({?MAHJONG_MELD_TYPE_PONG, MahjongId}, 1, Meld) of
				false ->
					{false, "没有碰有过相应的牌，新抓牌明杠失败"};
				{_, PlayerId1, _} ->
					case lists:keyfind(MahjongId, 1, Basis) of
						{MahjongId, Num} when Num >= 1 ->
							NewMeld = lists:keydelete({?MAHJONG_MELD_TYPE_PONG, MahjongId}, 1, Meld),
							NewMeld1 = [{{?MAHJONG_MELD_TYPE_EXPOSED_KONG,MahjongId}, PlayerId1, {MahjongId,MahjongId, MahjongId, MahjongId}}|NewMeld],
							NewBasis = lists:keystore(MahjongId, 1, Basis, {MahjongId, Num -1}),
							NewFlag = ?CONDITIONAL_EXPRESSION(Flag == ?FLAG_CANWIN, ?FLAG_NORMAL, Flag),	
							NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{
																	basis = NewBasis,
																	meld = NewMeld1,
																	flag = NewFlag,
																	mahjong_id = 0,
																	canmeld = []
							},
							lib_mahjong_fight_util:refresh_mahjong_fight_info(NewMahjongPlayer),
							NewMahjongState = MahjongState#r_mahjong_fight_state{
																		last_mahjongid = MahjongId,
																		last_position = Position,
																		now_postion = Position, 
																		is_kong = true
																		},
							lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
							%% 清理所有玩家可碰杠
							mahjong_canmeld(),
							%% 提醒杠
							lib_chat:send_common_notice(util:erlang_list_to_string(["k", MahjongId, PlayerId, PlayerId1])),
							%% 提醒
							mahjong_notice(NewMahjongPlayer, PlayerId1),
							%% 先返回成功，再抓牌
							ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_meld, true, ""),
							lib_mahjong_fight_msg:send_mahjong_fight_reply(GatePid, ReplyMsg),
							
							%% 检测是否有玩家抢杠胡，没有杠后抓牌，暂时直接抓牌
							MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
							F = fun(E, Acc) ->
									if
										E#mahjong_fight_info.player_id =/= PlayerId ->
											Acc;
										true ->
											case lib_mahjong_fight_util:mahjong_fight_win_check(MahjongId, E#mahjong_fight_info.basis) of
												{true, _} ->
													[E#mahjong_fight_info{flag = ?FLAG_CANWIN}|Acc];
												_ ->
													Acc
											end 
									end
								end,
							case lists:foldl(F, [], MahjongPlayerList) of
								[] ->
									mahjong_draw(true);
								ChangeList ->	
									lib_mahjong_fight_util:refresh_mahjong_fight_info(ChangeList)
							end,
							{true, ""};
						_ ->
							{false, "明杠，杠一碰的牌，但手上没有第四张牌"}
					end
			end;
		true ->
			if
				MahjongId =/=LastMahjongId ->
					{false, "只能杠新出的牌"};
				Position == LastPosition ->
					{false, "不能杠自己出的牌"};
				true ->
					LastPlayerId = lib_mahjong_fight_procdict:get_mahjong_player_id_by_position(LastPosition),
					case lib_mahjong_fight_util:mahjong_fight_exposed_kong_check(MahjongId, LastPlayerId, Basis) of
						{true, Kong} ->
							NewMeld = [Kong|Meld],
							NewBasis = lists:keydelete(MahjongId, 1, Basis),
							NewFlag = ?CONDITIONAL_EXPRESSION(Flag == ?FLAG_CANWIN, ?FLAG_NORMAL, Flag),
							NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{
																			basis = NewBasis,
																			meld = NewMeld,
																			flag = NewFlag,
																			mahjong_id = 0,
																			canmeld = []
							},
							lib_mahjong_fight_util:refresh_mahjong_fight_info(NewMahjongPlayer),
							NewMahjongState = MahjongState#r_mahjong_fight_state{
																				now_postion = Position, 
																				kong_num = MahjongState#r_mahjong_fight_state.kong_num + 1,
																				is_kong = true
																				},
							lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
							%% 清理提醒碰杠
							mahjong_canmeld(),
							%% 清理某个玩家出的牌，因为被碰了
							mahjong_out(LastPlayerId),
							%% 提醒
							lib_chat:send_common_notice(util:erlang_list_to_string(["k", MahjongId, PlayerId, LastPlayerId])),
							%% 提醒
							mahjong_notice(NewMahjongPlayer, LastPlayerId),
							%% 先返回成功，再抓牌
							ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_meld, true, ""),
							lib_mahjong_fight_msg:send_mahjong_fight_reply(GatePid, ReplyMsg),
							%% 抓牌
							mahjong_draw(true),
							{true, ""};
						_ ->
							{false, io_lib:format("~p没有牌足够的牌可以明杠",[MahjongId])}
					end
			end
	end;
%% 碰
mahjong_meld1(?MAHJONG_MELD_TYPE_PONG, [MahjongId|_], MahjongPlayer, MahjongState) ->
	IsVaid = lists:member(MahjongId, MahjongPlayer#mahjong_fight_info.limit),
	if
		MahjongId =/= MahjongState#r_mahjong_fight_state.last_mahjongid ->
			{false, "只能碰新出的牌"};
		MahjongPlayer#mahjong_fight_info.position == MahjongState#r_mahjong_fight_state.last_position ->
			{false, "不能碰自己出的牌"};
		IsVaid ->
			%%io:format("is_vaid~p~n",[MahjongPlayer#mahjong_fight_info.limit]),
			{false, "放弃碰牌，没过圈不能碰相同的牌"};
		true ->
			#mahjong_fight_info{
					player_id = PlayerId,
					gate_pid = GatePid,
					flag = Flag,
					meld = Meld,
					position = Position,
					basis = Basis,
					play_limit = PlayLimit
			} = MahjongPlayer,
			case lists:keyfind(MahjongId, 1, Basis) of
				{MahjongId, Num} when Num >=2 ->
					LastPlayerId = lib_mahjong_fight_procdict:get_mahjong_player_id_by_position(MahjongState#r_mahjong_fight_state.last_position),
					NewMeld = [{{?MAHJONG_MELD_TYPE_PONG,MahjongId}, LastPlayerId, {MahjongId,MahjongId, MahjongId}}|Meld],
					NewBasis = lists:keystore(MahjongId, 1, Basis, {MahjongId, Num -2}),
					NewFlag = ?CONDITIONAL_EXPRESSION(Flag == ?FLAG_CANWIN, ?FLAG_NORMAL, Flag),
					NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{
																	basis = NewBasis,
																	meld = NewMeld,
																	canmeld = [],
																	flag = NewFlag,
																	mahjong_id = 0,
																	play_limit = [MahjongId|PlayLimit]
					},
					lib_mahjong_fight_util:refresh_mahjong_fight_info(NewMahjongPlayer),
					NewMahjongState = MahjongState#r_mahjong_fight_state{now_postion = Position},
					lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
					%% 清理提醒
					mahjong_canmeld(),
					%% 清理某个玩家出的牌，因为被碰了
					mahjong_out(LastPlayerId),
					%% 提醒碰牌了
					lib_chat:send_common_notice(util:erlang_list_to_string(["p", MahjongId, PlayerId, LastPlayerId])),
					%% 提醒
					mahjong_notice(NewMahjongPlayer, LastPlayerId),
					%% 先返回成功，再抓牌
					ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_meld, true, ""),
					lib_mahjong_fight_msg:send_mahjong_fight_reply(GatePid, ReplyMsg),
					%% 
					PlayMahjongId = lib_mahjong_fight_util:get_mahjong_fight_random_play_mahjong_id([MahjongId|PlayLimit], NewBasis),
					change_mahjong_wait_ref(mahjong_play, [NewMahjongPlayer#mahjong_fight_info.player_id, PlayMahjongId]),
					{true,""};
				_ ->
					{false, io_lib:format("~p没有牌足够的牌可以明杠",[MahjongId])}
			end
	end;
%% 吃牌
mahjong_meld1(?MAHJONG_MELD_TYPE_EAT, ValueList, MahjongPlayer, MahjongState) ->
	#mahjong_fight_info{
					player_id = MyId,
					gate_pid = GatePid,
					basis = Basis,
					flag = Flag,
					meld = Meld,
					position = Position,
					play_limit = PlayLimit
	} = MahjongPlayer,
	#r_mahjong_fight_state{
						last_position = LastPosition,
						last_mahjongid = MahjongId
	} = MahjongState,
	NextPosition = ?GET_NEXT_POSITION(LastPosition),
	[MahjongId1,MahjongId2|_] = lists:delete(MahjongId, ValueList),

	IsVaid = lists:member(MahjongId, MahjongPlayer#mahjong_fight_info.limit),
	%% 检查其他玩家可不可以碰
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	F = fun(E, Acc) ->
			if
				Acc == true ->
					true;
				E#mahjong_fight_info.position =/= NextPosition andalso E#mahjong_fight_info.position =/= LastPosition 
					andalso E#mahjong_fight_info.canmeld =/= [] ->
						true;
				true ->
					Acc
			end
		end,
	R = lists:foldl(F, false, MahjongPlayerList),
	if
		R  ->
			{false, "有玩家可以碰牌"};
		Position =/= NextPosition ->
			{false, "只能吃上家出的牌"};
		IsVaid ->
			%%io:format("is_vaid~p~n",[MahjongPlayer#mahjong_fight_info.limit]),
			{false, "放弃吃牌没过圈，不能再吃相同组合的牌"};  %% 45 放弃吃3 那没过圈不能吃3，7 ，同一组合
		true ->
			%%io:format("mahjong_fight_eat_check~p~n",[{MahjongId, MahjongId1, MahjongId2, Basis}]),
			case lib_mahjong_fight_util:mahjong_fight_eat_check(MahjongId, MahjongId1, MahjongId2, Basis) of
				{true, NewBasis} ->	
					%%io:format("__mahjong_fight_eat_check~p~n",[NewBasis]),
					PlayerId = lib_mahjong_fight_procdict:get_mahjong_player_id_by_position(LastPosition),
					NewMeld = [{{?MAHJONG_MELD_TYPE_EAT,MahjongId}, PlayerId, {MahjongId, MahjongId1, MahjongId2}}|Meld],
					NewFlag = ?CONDITIONAL_EXPRESSION(Flag == ?FLAG_CANWIN, ?FLAG_NORMAL, Flag),
					if
						length(Meld) >2 -> %% 碰杠超过两次 可以吃什么碰什么
							NewPlayLimit1 = PlayLimit;
						true ->
							NewPlayLimit = lib_mahjong_fight_util:get_mahjong_fight_common_meld_mahjong_id(MahjongId1, MahjongId2),
							NewPlayLimit1 = PlayLimit ++ NewPlayLimit
					end,
					NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{
																	basis = NewBasis,
																	meld = NewMeld,
																	canmeld = [],
																	flag = NewFlag,
																	mahjong_id = 0,
																	play_limit = NewPlayLimit1
					},
					lib_mahjong_fight_util:refresh_mahjong_fight_info(NewMahjongPlayer),
					NewMahjongState = MahjongState#r_mahjong_fight_state{now_postion = Position},
					lib_mahjong_fight_procdict:update_dic_magjong_fight_state(NewMahjongState),
					%% 清理提醒
					mahjong_canmeld(),
					%% 清理某个玩家出的牌，因为被碰了
					mahjong_out(PlayerId),
					%% 提醒吃谁的牌
					lib_chat:send_common_notice(util:erlang_list_to_string(["e", MahjongId, MyId, PlayerId])),
					mahjong_notice(NewMahjongPlayer, PlayerId),
					%% 先返回成功，再抓牌
					ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_meld, true, ""),
					lib_mahjong_fight_msg:send_mahjong_fight_reply(GatePid, ReplyMsg),

					PlayMahjongId = lib_mahjong_fight_util:get_mahjong_fight_random_play_mahjong_id(NewPlayLimit1, NewBasis),
					change_mahjong_wait_ref(mahjong_play, [NewMahjongPlayer#mahjong_fight_info.player_id, PlayMahjongId]),
					%% 等待玩家出牌
					{true,""};
				_ ->
					{false, "不能组成顺子"}
			end
	end;
mahjong_meld1(Type, _, _M, _) ->
	{false, io_lib:format("未知请求组合类型~p", [Type])}.

%% 服务端取消
mahjong_cancel() ->
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	F = fun(E) ->
			mahjong_cancel(E#mahjong_fight_info.player_id)
		end,
	lists:foreach(F, MahjongPlayerList).
			
%% 放弃碰听杠等等
mahjong_cancel(PlayerId) -> 
	case pre_mahjong_cancel(PlayerId) of
		{true, Dict} ->
			FunChange = dict:fetch('fun_change', Dict),
			FunChange(),
			ok;
		{false, Reason} ->
			ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_cancel, false, Reason),
			lib_mahjong_fight_msg:send_mahjong_fight_reply(PlayerId, ReplyMsg)
	end.
	
pre_mahjong_cancel(PlayerId) ->
	CheckList = [
				'player',
				'change'
				],
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	Dict = dict:from_list([
						{'player_id', PlayerId},
						{'mahjong_state', MahjongState}
						]),
	pre_mahjong_cancel_check(Dict, CheckList).

pre_mahjong_cancel_check(Dict, []) ->
	{true, Dict};
pre_mahjong_cancel_check(Dict, ['player'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	PlayerId = dict:fetch('player_id', Dict),
	LastMahjongId = MahjongState#r_mahjong_fight_state.last_mahjongid,
	case lib_mahjong_fight_procdict:get_mahjong_by_player_id(PlayerId) of
		false ->
			{false, "玩家没有参与牌局"};
		MahjongPlayer ->
			Flag = ?CONDITIONAL_EXPRESSION(MahjongPlayer#mahjong_fight_info.flag ==?FLAG_CANWIN, 0, MahjongPlayer#mahjong_fight_info.flag),
			Length = lib_mahjong_fight_util:get_mahjong_fight_basis_length(MahjongPlayer#mahjong_fight_info.basis),
			if
				Length rem 3 == 2 -> %% 自己抓牌放弃，不影响后面，而且直接抓牌放弃，下一个自己出牌限制就清除了,这里只是为了，确认玩家放弃的不是lastmahjongId
					Limit = MahjongPlayer#mahjong_fight_info.limit;
				true ->
					Limit = [LastMahjongId|MahjongPlayer#mahjong_fight_info.limit]
			end,
			NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{
														canmeld = [],
														canwin = [],
														flag = Flag,
														limit = Limit

			},
			Dict1 = dict:store('mahjong_player', NewMahjongPlayer, Dict),
			pre_mahjong_cancel_check(Dict1, Z)
	end;
pre_mahjong_cancel_check(Dict, ['change'|Z]) ->
	MahjongPlayer = dict:fetch('mahjong_player', Dict),
	% FunCircle = dict:fetch('fun_circle', Dict),
	FunChange = fun() ->
					lib_mahjong_fight_util:refresh_mahjong_fight_info(MahjongPlayer),
					ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_cancel, true, ""),
					lib_mahjong_fight_msg:send_mahjong_fight_reply(MahjongPlayer#mahjong_fight_info.gate_pid, ReplyMsg),
					%% 如果取消的是别人出牌可以碰，需要循环，如果是增加抓牌，那就增加出牌
					mahjong_cancel_after_check()
				end,
	Dict1 = dict:store('fun_change', FunChange, Dict),
	pre_mahjong_cancel_check(Dict1, Z);
pre_mahjong_cancel_check(Dict, [_|Z]) ->
	pre_mahjong_cancel_check(Dict, Z).
%% 每一个玩家取消后，检测是否开始循环检测 因为有可能其他玩家同时可以操作
mahjong_cancel_after_check() ->
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	#r_mahjong_fight_state{
						is_end = IsEnd,
						is_kong = IsKong,
						now_postion = NowPosition
	} = MahjongState,
	F = fun(E, Acc) ->
			#mahjong_fight_info{
					flag = Flag,
					canmeld = CanMeld,
					canwin = CanWin
			} = E,
			if 
				Acc == true ->
					true;
				Flag == ?FLAG_CANWIN orelse CanMeld =/= [] orelse CanWin =/= [] ->
					true;
				true ->
					Acc
			end
		end,
	Result = lists:foldl(F, false, MahjongPlayerList),
	if
		IsEnd ->
			{false, "游戏已经结束"};
		Result  ->
			{false, "还有人没取消"};
		true ->
			case lib_mahjong_fight_procdict:get_mahjong_by_position(NowPosition) of
				[] ->
					{false, "数据异常"};
				MahjongPlayer ->
					Length = lib_mahjong_fight_util:get_mahjong_fight_basis_length(MahjongPlayer#mahjong_fight_info.basis) ,
					if
						Length rem 3 == 2 ->
							%% 应该是出牌 玩家主动
							skip;
						true ->
							%% 取消后就是抓牌
							mahjong_draw(IsKong)
					end
			end
	end.

%% 听牌
mahjong_ready(PlayerId, MahjongId) -> 
	case pre_mahjong_ready(PlayerId, MahjongId) of
		{true, Dict} ->
			FunChange = dict:fetch('fun_change', Dict),
			FunChange(),
			ok;
		{false, Reason} ->
			ReplyMsg = lib_mahjong_fight_msg:pack_pb_mahjong_fight_reply(sc_mahjong_fight_ready, false, Reason),
			lib_mahjong_fight_msg:send_mahjong_fight_reply(PlayerId, ReplyMsg)
	end.

pre_mahjong_ready(PlayerId, MahjongId) ->
	CheckList = [
				'state',
				'player', 
				'change'
				],
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	Dict = dict:from_list([
						{'mahjong_state', MahjongState},
						{'player_id', PlayerId},
						{'mahjong_id', MahjongId}
						]),
	pre_mahjong_ready_check(Dict, CheckList).
pre_mahjong_ready_check(Dict, []) ->
	{true, Dict};
pre_mahjong_ready_check(Dict, ['state'|Z]) ->
	MahjongState = dict:fetch('mahjong_state', Dict),
	case MahjongState#r_mahjong_fight_state.is_end of
		true ->
			{false, "牌局已经结束"};
		_ ->
			pre_mahjong_ready_check(Dict, Z)
	end;
pre_mahjong_ready_check(Dict, ['player'|Z]) ->
	PlayerId = dict:fetch('player_id', Dict),
	MahjongId = dict:fetch('mahjong_id', Dict),
	case lib_mahjong_fight_procdict:get_mahjong_by_player_id(PlayerId) of
		false ->
			{false, "玩家没有参与牌局"};
		MahjongPlayer when MahjongPlayer#mahjong_fight_info.state =/= ?STATE_BEING ->
			{false, "玩家不在参与牌局状态"};
		MahjongPlayer when MahjongPlayer#mahjong_fight_info.canwin == [] ->
			{false, "不可听牌"};
		MahjongPlayer ->
			case lib_mahjong_fight_util:mahjong_fight_ready_hand_check(MahjongPlayer#mahjong_fight_info.basis) of
				false ->
					{false, "当前状态不可听牌"};
				ReadyList ->
					case lists:keyfind(MahjongId, 1, ReadyList) of
						false ->
							{false, "可以听牌，但选择出牌错误"};
						_ ->
							Dict1 = dict:store('mahjong_player', MahjongPlayer, Dict),
							pre_mahjong_ready_check(Dict1, Z)
					end
			end
	end;
pre_mahjong_ready_check(Dict, ['change'|Z]) ->
	MahjongPlayer = dict:fetch('mahjong_player', Dict),
	MahjongId = dict:fetch('mahjong_id', Dict),
	% Out = MahjongPlayer#mahjong_fight_info.out,
	case lists:keyfind(MahjongId, 1, MahjongPlayer#mahjong_fight_info.basis) of
		{MahjongId, Num} when Num > 0 ->
			% Basis = lists:keystore(MahjongId, 1, MahjongPlayer#mahjong_fight_info.basis, {MahjongId, Num -1}),
			NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{flag = ?FLAG_READY, canmeld = [], canwin = []},
			FunChange = fun() ->
							%% 更新听牌信息
							lib_mahjong_fight_util:refresh_mahjong_fight_info(NewMahjongPlayer),
							%% 选择听牌，要出一张牌
							mahjong_play(NewMahjongPlayer#mahjong_fight_info.player_id, MahjongId),
							ok
						end,
			Dict1 = dict:store('fun_change', FunChange, Dict),
			pre_mahjong_ready_check(Dict1, Z);
		_ ->
			{false, "手上没有出的牌"}
	end;
pre_mahjong_ready_check(Dict, [_|Z]) ->
	pre_mahjong_ready_check(Dict, Z).

mahjong_quit(PlayerId) ->
	%%io:format("mahjong_quit~p~n",[PlayerId]),
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	case lib_mahjong_fight_procdict:get_mahjong_by_player_id(PlayerId) of
		MahjongPlayer when MahjongPlayer#mahjong_fight_info.state =/= ?STATE_LEAVEL ->
			case MahjongState#r_mahjong_fight_state.is_end of
				true ->
					Ref = ?UNDEFINED;
				_ ->
					Ref = erlang:send_after(?MAX_DROP_TIME * 1000, self(), {'handle', ?MODULE, mahjong_flow, []})
			end,
			MahjongPlayer1 = MahjongPlayer#mahjong_fight_info{
															flag = 4,
															state = ?STATE_LEAVEL, 
															gate_pid = ?UNDEFINED,
															re_time = mod_time:now_seconds(),
															re_ref = Ref
														},
			lib_mahjong_fight_procdict:update_mahjong_by_player_id(MahjongPlayer1),
			lib_mahjong_fight_msg:send_mahjong_fight_info(MahjongPlayer1),
			%% 检测玩家是否有人在线，没有直接结算
			case mahjong_online() of
				false ->  %% 四个人都走后流局
					mahjong_flow();
				_ ->
					skip
			end;
		_ ->
			skip
	end.
%% 检测有没有人掉线 true 是没有人掉线 false有人掉线了
mahjong_dropped() ->
	% 做一个玩家定时
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	F = fun(E, Acc) ->
			if
				Acc == true ->
					true;
				E#mahjong_fight_info.state == ?STATE_LEAVEL ->
					true;
				true ->
					Acc
			end
		end,
	lists:foldl(F, false, MahjongPlayerList).
%% 检测有人在线
mahjong_online() ->
	% 做一个玩家定时
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	F = fun(E, Acc) ->
			if
				Acc == true ->
					true;
				E#mahjong_fight_info.state =/= ?STATE_LEAVEL ->
					true;
				true ->
					Acc
			end
		end,
	lists:foldl(F, false, MahjongPlayerList).
%% 关闭进程
mahjong_stop() ->
	MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
	% MahjongWatchList = lib_mahjong_fight_procdict:get_dic_mahjong_fight_watch_list(),
	% MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	#r_mahjong_fight_state{
						is_end = IsEnd,
						room_id = RoomId,
						start_time = StartTime,
						long_time = LongTime
	} = MahjongState,
	NowTime = mod_time:now_seconds(),
	% Online = mahjong_online(),
	% %%io:format("mahjong_stop~p~n",[{Online, NowTime, StartTime, LongTime}]),

	if
		IsEnd andalso NowTime - StartTime > LongTime ->
			%%io:format("IsEnd~p~n",[{IsEnd,NowTime, StartTime, LongTime}]),
			lib_room_info:cast_delete_room_info(RoomId),
			erlang:send(self(), 'stop');
		%% 进程等关闭时间吧
		% Online == false andalso MahjongWatchList == [] andalso MahjongPlayerList == [] ->
		% 	% lib_room_info:change_room_march(RoomId, false),
		% 	erlang:send(self(), 'stop');
		true ->
			make_mahjong_start_ref(60, ?MODULE, mahjong_stop, [])
	end.
%%牌局更新信息
mahjong_update(PlayerId) ->
	MahjongPlayer = lib_mahjong_fight_procdict:get_mahjong_by_player_id(PlayerId),
	MahjongWatch = lib_mahjong_fight_procdict:get_mahjong_watch_by_player_id(PlayerId),
	lib_mahjong_fight_msg:send_mahjong_fight_update(MahjongPlayer),
	lib_mahjong_fight_msg:send_mahjong_fight_update(MahjongWatch),
	%% 客户端要求
	lib_mahjong_fight_msg:send_mahjong_fight_info(MahjongPlayer),
	lib_mahjong_fight_msg:send_mahjong_fight_info(MahjongWatch),
	% lib_mahjong_fight_msg:send_mahjong_fight_info_to_self(MahjongPlayer),
	%% 有人离线不能发牌，重新连上后，要检查发牌
	mahjong_reconnection(),
	ok.

%% 牌局碰后，清空提示
mahjong_canmeld() ->
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	F = fun(E) ->
			E#mahjong_fight_info{canmeld = []}
		end,
	MahjongPlayerList1 = lists:map(F, MahjongPlayerList),
	%% 取消碰杠提示
	lib_mahjong_fight_util:refresh_mahjong_fight_info(MahjongPlayerList1),
	ok.
%% 检查是不是还有其他玩家可以胡牌，而且不是自己
mahjong_canwin() ->
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	F = fun(E, Acc) ->
			if
				Acc == true ->
					true;
				E#mahjong_fight_info.flag == ?FLAG_CANWIN ->
					true;
				true ->
					Acc
			end
		end,
	case lists:foldl(F, false, MahjongPlayerList)of
		true -> %% 还有其他玩家可以胡牌，等待
			skip;
		_ -> %% 没有其他玩家可以胡牌
			mahjong_fight_end()
	end.
%% 清理玩家已出的牌，因为被吃了
mahjong_out(PlayerId) ->
	case lib_mahjong_fight_procdict:get_mahjong_by_player_id(PlayerId) of
		MahjongPlayer when is_record(MahjongPlayer, mahjong_fight_info) ->
			case MahjongPlayer#mahjong_fight_info.out of
				[] ->
					skip;
				[_MahjongId|OutList] ->
					NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{out = OutList},
					lib_mahjong_fight_procdict:update_mahjong_by_player_id(NewMahjongPlayer),
					%% 广播出的牌
					lib_mahjong_fight_msg:send_mahjong_fight_info(NewMahjongPlayer)
			end;
		_ ->
			skip
	end.

%% 提醒杠数量
mahjong_notice(MahjongPlayer, DumyId) ->
	#mahjong_fight_info{
					% player_id = PlayerId,
					player_name = PlayerName,
					meld = Meld
	} = MahjongPlayer,
	F = fun({_, EPlayerId, _}, Acc) ->
			if
				EPlayerId == DumyId ->
					Acc + 1;
				true ->
					Acc
			end
		end,
	Num = lists:foldl(F, 0, Meld),
	if
		Num >= 2 ->
			case lib_mahjong_fight_procdict:get_mahjong_by_player_id(DumyId) of
				DumyInfo when is_record(DumyInfo, mahjong_fight_info) ->
					DumyName = DumyInfo#mahjong_fight_info.player_name,
					lib_chat:send_common_notice(1, lists:concat([DumyName, "已经被", PlayerName, "上了", Num, "次牌"]));
				_ ->
					skip
			end;
		true ->
			skip
	end.

%% 玩家新连上上后发牌
mahjong_reconnection() ->
	%% 先检查有没有人可碰可胡之类的
	MahjongPlayerList = lib_mahjong_fight_procdict:get_dic_mahjong_player_list(),
	F = fun(E, Acc) ->
			#mahjong_fight_info{
							canmeld = CanMeld,
							flag = Flag
			} = E,
			if
				Acc == true ->
					true;
				CanMeld =/= [] orelse Flag == ?FLAG_CANWIN ->
					true; %% 有人可以胡可以碰
				true ->
					Acc
			end
		end,
	case lists:foldl(F, false, MahjongPlayerList) of
		true -> %% 有人可以碰，等待别人操作
			skip;
		_ ->
			MahjongState = lib_mahjong_fight_procdict:get_dic_mahjong_fight_state(),
			#r_mahjong_fight_state{
								now_postion = NowPosition,
								is_kong = IsKong
			} = MahjongState,
			case lib_mahjong_fight_procdict:get_mahjong_by_position(NowPosition) of
				NowPlayer when is_record(NowPlayer, mahjong_fight_info) ->
					mahjong_draw(IsKong);
				_ ->
					skip
			end
	end.







