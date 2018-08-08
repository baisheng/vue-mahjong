%% 公共判断
-module(lib_mahjong_fight_util).
-include("mahjong_fight.hrl").

-export([

		refresh_mahjong_fight_info/1,       %% 更新玩家信息，并且通知客户端变化

		mahjong_fight_win_check/1,
		mahjong_fight_win_check/2,          %% 检查手上的牌，是否符合胡牌
		mahjong_fight_ready_hand_check/1,   %% 听牌检查
		mahjong_fight_eat_check/4,
		mahjong_fight_concealed_kong_check/1,			%% 暗杠
		mahjong_fight_exposed_kong_check/1,          %% 明杠 抓牌后
		mahjong_fight_exposed_kong_check/3,          %% 明杠 出牌后
		mahjong_fight_pong_check/2,



		get_mahjong_fight_ready_hand/1,      %% 获取听牌组合
		get_mahjong_fight_eat/3,
		get_mahjong_fight_last_mahjong_id/1,  %% 获取最后一个id
		get_mahjong_fight_random_mahjong_id/1,
		get_mahjong_fight_random_play_mahjong_id/2,  %% 获取一张随机的，能够出的牌
		get_mahjong_fight_common_meld_mahjong_id/2,  %% 获取一张吃碰杠的组合id 比如33，34，那就可能和32，35组成顺子
		get_mahjong_fight_mahjong_count/6,            %% 这次胡牌台数
		get_mahjong_fight_mahjong_score/7,            %% 根据台数算积分，包括有谁负
		get_mahjong_fight_basis_length/1,              %% 获得长度
		get_mahjong_fight_combination/2               %% 结算结构，用最合适的，当然其实除了暗自，与顺子的	区别，就是结算显示，用暗自对的组合

		]).


refresh_mahjong_fight_info(NewMahjongPlayer) when is_list(NewMahjongPlayer) ->
	F = fun(E) ->
			refresh_mahjong_fight_info(E)
		end,
	lists:foreach(F, NewMahjongPlayer);
refresh_mahjong_fight_info(NewMahjongPlayer) ->
	case lib_mahjong_fight_procdict:get_mahjong_by_player_id(NewMahjongPlayer#mahjong_fight_info.player_id) of
		MahjongPlayer when is_record(NewMahjongPlayer, mahjong_fight_info) andalso MahjongPlayer =/= NewMahjongPlayer ->
			lib_mahjong_fight_procdict:update_mahjong_by_player_id(NewMahjongPlayer),
			lib_mahjong_fight_msg:send_mahjong_fight_info_to_self(NewMahjongPlayer),
			ok;
		_ ->
			skip
	end.



%% 直接指定一张牌返回了一个组合
mahjong_fight_win_check(MahjongId, List) ->
	case lists:keyfind(MahjongId, 1, List) of
		{MahjongId, Num} when Num >=4 ->
			{false, "传入不合法的麻将，出现了5张"};
		{MahjongId, Num} ->
			NewList = lists:keystore(MahjongId, 1, List, {MahjongId, Num + 1}),
			mahjong_fight_win_check(NewList);
		_ ->
			NewList = lists:keystore(MahjongId, 1, List, {MahjongId, 1}),
			mahjong_fight_win_check(NewList)
	end.
%% 一个组合看看是否赢 2 5 8 11 14 张牌 [{11,1}......]
mahjong_fight_win_check(List) ->
	%% 排序，并且防止相同的key值存在，所以在逻辑里处理就要注意了
	NewList = lists:ukeysort(1, List),
	F = fun({MahjongId, ENum}, {AccNum, AccCheckList}) ->
			if
				ENum >= 2 ->
					{ENum + AccNum, [MahjongId|AccCheckList]};
				true ->
					{ENum + AccNum, AccCheckList}
			end
		end,
	{TotalNum, CheckPairsList} = lists:foldl(F, {0, []}, NewList),
	mahjong_fight_win_check(TotalNum, CheckPairsList, NewList).
mahjong_fight_win_check(TotalNum, _CheckPairsList, _NewList) when TotalNum rem 3 =/= 2 ->
	{false, "大哥相公了"};
mahjong_fight_win_check(_TotalNum, CheckPairsList, _NewList) when CheckPairsList == [] ->
	{false, "没有将眼"};
mahjong_fight_win_check(_TotalNum, CheckPairsList, NewList) ->
	get_mahjong_fight_win_combination(CheckPairsList, NewList).
	
	
%% 根据将眼 拿出所有组合
get_mahjong_fight_win_combination(CheckPairsList, List) ->
	get_mahjong_fight_win_combination(CheckPairsList, List, []).

get_mahjong_fight_win_combination([], _List, CombinationList) when CombinationList == [] ->
	{false, "并没有可以赢牌的组合"};
get_mahjong_fight_win_combination([], _List, CombinationList) ->
	{true, CombinationList};
get_mahjong_fight_win_combination([MahjongId|CheckPairsList], List, CombinationList) ->
	case lists:keyfind(MahjongId, 1, List) of
		{MahjongId, Num} when Num >= 2 ->
			NewList = lists:keystore(MahjongId, 1, List, {MahjongId, Num -2}), %% 位置没变
			CombinationList1 = get_mahjong_fight_win_combination(NewList, MahjongId, [], CombinationList);
		_ ->
			CombinationList1 = CombinationList
	end,
	get_mahjong_fight_win_combination(CheckPairsList, List, CombinationList1).


%% 这个接口测试的是手上除开将眼的 3n个组合 也就是总数只可能是 0，3，6，9，12
%% 获取赢牌的3n 组合
%% 0个参数
% get_mahjong_fight_win_combination([], _PairsId, [], CombinationList) ->
% 	CombinationList;
get_mahjong_fight_win_combination([], PairsId, TempList, CombinationList) ->
	Combination = #r_mahjong_fight_win_combination{
												pairs = PairsId,
												combination = TempList
	},
	[Combination|CombinationList];
%% 一个参数
get_mahjong_fight_win_combination([{MahjongId, Num}], PairsId, TempList, CombinationList) when Num == 3 ->
	NewTempList = [{MahjongId, MahjongId, MahjongId}|TempList],
	get_mahjong_fight_win_combination([], PairsId, NewTempList, CombinationList);
get_mahjong_fight_win_combination([{_MahjongId, Num}], PairsId, TempList, CombinationList) when Num =< 0 ->
	get_mahjong_fight_win_combination([], PairsId, TempList, CombinationList);
get_mahjong_fight_win_combination([{_MahjongId, _Num}], _PairsId, _TempList, CombinationList)  ->
	CombinationList;
%% 两个参数
get_mahjong_fight_win_combination([{MahjongId1, Num1},{MahjongId2, Num2}], PairsId, TempList, CombinationList) when Num1 == 3 ->
	NewTempList = [{MahjongId1, MahjongId1, MahjongId1}|TempList],
	get_mahjong_fight_win_combination([{MahjongId2, Num2}], PairsId, NewTempList, CombinationList);
get_mahjong_fight_win_combination([{MahjongId1, Num1},{MahjongId2, Num2}], PairsId, TempList, CombinationList) when Num1 >3 ->
	NewTempList = [{MahjongId1, MahjongId1, MahjongId1}|TempList],
	get_mahjong_fight_win_combination([{MahjongId1, Num1 - 3},{MahjongId2, Num2}], PairsId, NewTempList, CombinationList);
get_mahjong_fight_win_combination([{_MahjongId1, Num1},{MahjongId2, Num2}], PairsId, TempList, CombinationList) when Num1 =< 0->
	get_mahjong_fight_win_combination([{MahjongId2, Num2}], PairsId, TempList, CombinationList);
get_mahjong_fight_win_combination([{_MahjongId1, _Num1},{_MahjongId2, _Num2}], _PairsId, _TempList, CombinationList) -> %% Num1 == 1or 2 但是MahjongId1 =/= MahjongId2
	CombinationList;
%% 3个参数
get_mahjong_fight_win_combination([{_MahjongId1, Num1},{MahjongId2, Num2},{MahjongId3, Num3}|L], PairsId, TempList, CombinationList) when Num1 =< 0 ->
	get_mahjong_fight_win_combination([{MahjongId2, Num2},{MahjongId3, Num3}|L], PairsId, TempList, CombinationList);
get_mahjong_fight_win_combination([{MahjongId1, Num1},{_MahjongId2, Num2},{MahjongId3, Num3}|L], PairsId, TempList, CombinationList) when Num2 =< 0 ->
	get_mahjong_fight_win_combination([{MahjongId1, Num1},{MahjongId3, Num3}|L], PairsId, TempList, CombinationList);
get_mahjong_fight_win_combination([{MahjongId1, Num1},{MahjongId2, Num2},{_MahjongId3, Num3}|L], PairsId, TempList, CombinationList) when Num3 =< 0 ->
	get_mahjong_fight_win_combination([{MahjongId1, Num1},{MahjongId2, Num2}|L], PairsId, TempList, CombinationList);
% 如果num1=1 or 2    Mahjong 1 2 3成顺子
get_mahjong_fight_win_combination([{MahjongId1, Num1},{MahjongId2, Num2},{MahjongId3, Num3}|L], PairsId, TempList, CombinationList) when Num1 =< 2 ->
	if
		MahjongId1 + 1 == MahjongId2 andalso MahjongId2 + 1 == MahjongId3 andalso ?IS_CAN_EAT(MahjongId1) -> %% 所以检测时要合并排序
			NewTempList = [{MahjongId1, MahjongId2, MahjongId3}|TempList],
			get_mahjong_fight_win_combination([{MahjongId1, Num1 - 1},{MahjongId2, Num2 - 1},{MahjongId3, Num3 -1}|L], PairsId, NewTempList, CombinationList);
		true ->
			CombinationList
	end;
%% num1 == 3 有两种可能，1是MahjongId1 做暗子 2 mahjong 1 2 3 成顺子
get_mahjong_fight_win_combination([{MahjongId1, Num1},{MahjongId2, Num2},{MahjongId3, Num3}|L], PairsId, TempList, CombinationList) when Num1 =< 3 ->
	%% 要么num1 做暗子，要么与后两成顺子[{11,12,13}] []
	TempList1 = [{MahjongId1, MahjongId1, MahjongId1}|TempList],
	CombinationList1 = get_mahjong_fight_win_combination([{MahjongId2, Num2},{MahjongId3, Num3}|L], PairsId, TempList1, CombinationList),
	%% 顺子
	if
		?IS_CAN_EAT(MahjongId1) ->
			TempList2 = [{MahjongId1, MahjongId2, MahjongId3}|TempList],
			get_mahjong_fight_win_combination([{MahjongId1, Num1 - 1},{MahjongId2, Num2 - 1},{MahjongId3, Num3 -1}|L], PairsId, TempList2, CombinationList1);
		true ->
			CombinationList1
	end;
% num1 ==4 最大4张  先一张与majong1 2 3 成顺子；接着num1=3,成了上面一种情况
get_mahjong_fight_win_combination([{MahjongId1, Num1},{MahjongId2, Num2},{MahjongId3, Num3}|L], PairsId, TempList, CombinationList) ->
	if
		MahjongId1 + 1 == MahjongId2 andalso MahjongId2 + 1 == MahjongId3 andalso ?IS_CAN_EAT(MahjongId1) ->
			NewTempList = [{MahjongId1, MahjongId2, MahjongId3}|TempList],
			get_mahjong_fight_win_combination([{MahjongId1, Num1 - 1},{MahjongId2, Num2 - 1},{MahjongId3, Num3 -1}|L], PairsId, NewTempList, CombinationList);
		true ->
			CombinationList
	end.
	% [{11,4},{12,4},{13,4}] []
	% [{11,3},{12,3},{13,4}] [{11,12,13}]

	% [{11,11,11},{12,12,12},{13,13,13},{11,12,13}]
	% [{11,12,13},{11,12,13},{11,12,13},{11,12,13}]


mahjong_fight_ready_hand_check(List) ->
	Length = get_mahjong_fight_basis_length(List),
	if
		Length rem 3 =/= 2 ->
			false;
		true ->
			mahjong_fight_ready_hand_check1(List, List, [])
	end.

%% baseList 是检测的原始牌 检测听牌 是说玩家手上一共3n + 2 张牌，去掉一张后，可以登哪张牌来胡
% mahjong_fight_ready_hand_check1(_, BaseList, _ReadyList) when get_mahjong_fight_basis_length(BaseList) rem 3 =/= 2 ->
% 	false;
mahjong_fight_ready_hand_check1([], _BaseList, ReadyList) when ReadyList == [] ->
	false;
mahjong_fight_ready_hand_check1([], _BaseList, ReadyList) ->
	ReadyList;
mahjong_fight_ready_hand_check1([{_EId, Num}|List], BaseList, ReadyList) when Num =< 0 ->
	mahjong_fight_ready_hand_check1(List, BaseList, ReadyList);
mahjong_fight_ready_hand_check1([{EId, Num}|List], BaseList, ReadyList) when Num =< 1 ->
	BaseList1 = lists:keydelete(EId, 1, BaseList),
	case get_mahjong_fight_ready_hand(BaseList1) of
		[] ->
			NewReadyList = ReadyList;
		Ready ->
			NewReadyList = [{EId, Ready}|ReadyList]
	end,
	mahjong_fight_ready_hand_check1(List, BaseList, NewReadyList);
mahjong_fight_ready_hand_check1([{EId, Num}|List], BaseList, ReadyList) ->
	BaseList1 = lists:keystore(EId, 1, BaseList, {EId, Num - 1}),
	case get_mahjong_fight_ready_hand(BaseList1) of
		[] ->
			NewReadyList = ReadyList;
		Ready ->
			NewReadyList = [{EId, Ready}|ReadyList]
	end,
	mahjong_fight_ready_hand_check1(List, BaseList, NewReadyList).

%% 检查听牌
get_mahjong_fight_ready_hand(List) -> %% 拿出34张牌， 一张一张去判断可否赢牌
	case get_mahjong_fight_ready_hand(?MAHJONG_ID_LIST, List) of
		false ->
			[];
		{true, ReadyList} ->
			ReadyList
	end.

get_mahjong_fight_ready_hand(MahjongIdList, List) when is_list(MahjongIdList) ->
	get_mahjong_fight_ready_hand(MahjongIdList, List, []);
get_mahjong_fight_ready_hand(MahjongIdList, List) ->
	get_mahjong_fight_ready_hand([MahjongIdList], List, []).

get_mahjong_fight_ready_hand([], _List, []) ->
	false;
get_mahjong_fight_ready_hand([], _List, ReadyList) ->
	{true, ReadyList};
get_mahjong_fight_ready_hand([MahjongId|MahjongIdList], List, ReadyList) ->
	case mahjong_fight_win_check(MahjongId, List) of
		{false, _Reason} ->
			get_mahjong_fight_ready_hand(MahjongIdList, List, ReadyList);
		{true, _CombinationList} ->
			get_mahjong_fight_ready_hand(MahjongIdList, List, [MahjongId|ReadyList])
	end.
%% 检测吃牌成顺子 1 2 是list牌，吃mahjong1
get_mahjong_fight_eat(MahjongId, PlayerId, List) ->
	get_mahjong_fight_eat(List, MahjongId, PlayerId, []).

get_mahjong_fight_eat(List, _MahjongId, _PlayerId, MeldList) when length(List) <2 ->
	MeldList;
get_mahjong_fight_eat([{_MahjongId1, Num1}, {MahjongId2, Num2}|L], MahjongId, PlayerId, MeldList) when Num1 =< 0  ->
	get_mahjong_fight_eat([{MahjongId2, Num2}|L], MahjongId, PlayerId, MeldList);
get_mahjong_fight_eat([{MahjongId1, Num1}, {_MahjongId2, Num2}|L], MahjongId, PlayerId, MeldList) when Num2 =< 0  ->
	get_mahjong_fight_eat([{MahjongId1, Num1}|L], MahjongId, PlayerId, MeldList);
get_mahjong_fight_eat([{MahjongId1, _Num1}, {MahjongId2, Num2}|L], MahjongId, PlayerId, MeldList) ->
	[NewMahjongId1, NewMahjongId2, NewMahjongId3|_] = lists:sort([MahjongId1, MahjongId2,MahjongId]),
	if
		NewMahjongId1+ 1 == NewMahjongId2 andalso NewMahjongId2 + 1 == NewMahjongId3 andalso ?IS_CAN_EAT(NewMahjongId1) ->
			NewMeldList = [{{?MAHJONG_MELD_TYPE_EAT,MahjongId}, PlayerId, {NewMahjongId1, NewMahjongId2, NewMahjongId3}}|MeldList];
		true ->
			NewMeldList = MeldList
	end,
	get_mahjong_fight_eat([{MahjongId2, Num2}|L], MahjongId, PlayerId, NewMeldList).

mahjong_fight_eat_check(MahjongId1, MahjongId2, MahjongId3, List) ->
	[NewMahjongId1, NewMahjongId2, NewMahjongId3|_] = lists:sort([MahjongId1, MahjongId2,MahjongId3]),
	if
		NewMahjongId1+ 1 == NewMahjongId2 andalso NewMahjongId2 + 1 == NewMahjongId3 andalso ?IS_CAN_EAT(NewMahjongId1) ->
			mahjong_fight_eat_check1(MahjongId2, MahjongId3, List);
		true ->
			{false, "不能组成顺子"}
	end.
mahjong_fight_eat_check1(MahjongId1, MahjongId2, List) ->
	case lists:keyfind(MahjongId1, 1, List) of
		false ->
			Num1 = 0;
		{_, Num1} ->
			ok
	end,
	case lists:keyfind(MahjongId2, 1, List) of
		false ->
			Num2 = 0;
		{_, Num2} ->
			ok
	end,	
	if
		Num1 > 0 andalso Num2 > 0 ->
			List1 = lists:keystore(MahjongId1, 1, List, {MahjongId1, Num1 - 1}),
			List2 = lists:keystore(MahjongId2, 1, List1,{MahjongId2, Num2 - 1}),
			{true, List2};
		true ->
			{false, "没有足够的牌组件成顺子"}
	end.

%% 检测明杠 就是玩家出牌后的 或者抓牌后 别人出牌后的明杠
mahjong_fight_exposed_kong_check(MahjongId, PlayerId, List) ->
	case lists:keyfind(MahjongId, 1, List) of
		{MahjongId, Num} when Num >= 3 ->
			{true, {{?MAHJONG_MELD_TYPE_EXPOSED_KONG, MahjongId}, PlayerId, {MahjongId, MahjongId, MahjongId, MahjongId}}};
		_ ->
			false
	end.
%% 抓牌后的明杠
mahjong_fight_exposed_kong_check(MahjongPlayer) ->
	#mahjong_fight_info{
					meld = Meld,
					basis = BasisList
	}= MahjongPlayer,
	mahjong_fight_exposed_kong_check1(Meld, BasisList, []).

mahjong_fight_exposed_kong_check1([], _BasisList, KongList) when KongList == [] ->
	false;
mahjong_fight_exposed_kong_check1([], _BasisList, KongList) ->
	KongList;
mahjong_fight_exposed_kong_check1([{{?MAHJONG_MELD_TYPE_PONG, MahjongId},PlayerId, _}|Meld], BasisList, KongList) ->
	case lists:keyfind(MahjongId, 1, BasisList) of
		{MahjongId, Num} when Num >= 1 ->
			Kong = {{?MAHJONG_MELD_TYPE_EXPOSED_KONG, MahjongId}, PlayerId, {MahjongId, MahjongId, MahjongId, MahjongId}},
			NewKongList = [Kong|KongList];
		_ ->
			NewKongList = KongList
	end,
	mahjong_fight_exposed_kong_check1(Meld, BasisList, NewKongList);
mahjong_fight_exposed_kong_check1([_E|Meld], BasisList, KongList) ->
	mahjong_fight_exposed_kong_check1(Meld, BasisList, KongList).	
%% 检测暗杠    
mahjong_fight_concealed_kong_check(MahjongPlayer) ->
	#mahjong_fight_info{
					basis = List,
					player_id = PLayerId
	} = MahjongPlayer,
	NewList = lists:ukeysort(1, List),
	mahjong_fight_concealed_kong_check1(NewList, PLayerId, []).

mahjong_fight_concealed_kong_check1([], _PlayerId, KongList) when KongList == [] ->
	false;
mahjong_fight_concealed_kong_check1([], _PlayerId, KongList) ->
	KongList;
mahjong_fight_concealed_kong_check1([{_EId, Num}|List], PlayerId, KongList) when Num < 4->
	mahjong_fight_concealed_kong_check1(List, PlayerId, KongList);
mahjong_fight_concealed_kong_check1([{EId, _Num}|List], PlayerId, KongList) ->
	Kong = {{?MAHJONG_MELD_TYPE_CONCEALED_KONG, EId}, PlayerId, {EId, EId, EId, EId}}, %% 暗杠的标志
	mahjong_fight_concealed_kong_check1(List, PlayerId, [Kong|KongList]).
%% 

%% 检测碰
mahjong_fight_pong_check(MahjongId, List) ->
	case lists:keyfind(MahjongId, 1, List) of
		{MahjongId, Num} when Num >= 2 ->
			true;
		_ ->
			false
	end.

%% 获取最后一个元素 因为有可能{EId, 0}
get_mahjong_fight_last_mahjong_id([]) ->
	0;
get_mahjong_fight_last_mahjong_id(List) ->
	{MahjongId, Num} = lists:last(List),
	if
		Num > 0 ->
			MahjongId;
		true ->
			get_mahjong_fight_last_mahjong_id(lists:droplast(List))
	end.
get_mahjong_fight_basis_length(List) ->
	F = fun({_EId, ENum}, AccLength) ->
			ENum + AccLength
		end,
	lists:foldl(F, 0, List).
%% 获取一个随机的，但不能是数量是0的
get_mahjong_fight_random_mahjong_id([]) ->
	0;
get_mahjong_fight_random_mahjong_id(List) ->
	{MahjongId, Num} =  util:random(List),
	if
		Num > 0 ->
			MahjongId;
		true ->
			NewList = lists:keydelete(MahjongId, 1, List),
			get_mahjong_fight_random_mahjong_id(NewList)
	end.
%% 获取一个随机的，同时能够出的牌
get_mahjong_fight_random_play_mahjong_id(Limit, List) ->
	MahjongId = get_mahjong_fight_random_mahjong_id(List),
	case lists:member(MahjongId, Limit) of
		true ->
			NewList = lists:keydelete(MahjongId, 1, List),
			get_mahjong_fight_random_play_mahjong_id(Limit, NewList);
		_ ->
			MahjongId
	end.
%% 45 找出3 7
get_mahjong_fight_common_meld_mahjong_id(MahjongId1, MahjongId2) when MahjongId1 == MahjongId2 ->
	[MahjongId1];
get_mahjong_fight_common_meld_mahjong_id(MahjongId1, MahjongId2) ->
	get_mahjong_fight_common_meld_mahjong_id(?MAHJONG_ID_LIST, MahjongId1,MahjongId2, []).

get_mahjong_fight_common_meld_mahjong_id([], _MahjongId1, _MahjongId2, MahjongIdList) ->
	MahjongIdList;
get_mahjong_fight_common_meld_mahjong_id([MahjongId|L], MahjongId1, MahjongId2, MahjongIdList) ->
	[NewMahjongId1, NewMahjongId2, NewMahjongId3] = lists:sort([MahjongId, MahjongId1, MahjongId2]),
	if
		?IS_CAN_EAT(MahjongId) andalso NewMahjongId1 + 1 == NewMahjongId2 andalso NewMahjongId2 + 1 == NewMahjongId3 ->
			NewMahjongIdList = [MahjongId|MahjongIdList];
		true ->
			NewMahjongIdList = MahjongIdList
	end,
	get_mahjong_fight_common_meld_mahjong_id(L, MahjongId1, MahjongId2, NewMahjongIdList).


%% 找出胡牌的台数5
get_mahjong_fight_mahjong_count(Combination, IsTouch,IsKong, WinId, MahjongPlayer,MahjongState) ->
	%%io:format("get_mahjong_fight_mahjong_count~p~n",[{Combination, IsTouch,IsKong, WinId, MahjongPlayer,MahjongState}]),
	Count = get_mahjong_fight_mahjong_count(?MAHJONG_TYPE_HU_LIST, Combination, IsTouch, IsKong, WinId, MahjongPlayer, MahjongState, 0, []),
	Count.
%% 7
get_mahjong_fight_mahjong_count([], _Combination, _IsTouch, _IsKong, _WinId, _MahjongPlayer, _MahjongState, Count, WinTypeList) ->
	{Count, WinTypeList};
get_mahjong_fight_mahjong_count([E|L], Combination, IsTouch, IsKong, WinId, MahjongPlayer, MahjongState, Count,WinTypeList) ->
	case get_mahjong_fight_mahjong_count(E, Combination, IsTouch, IsKong, WinId, MahjongPlayer, MahjongState) of
		{Type, AddNum} ->
			NewCount = Count + AddNum,
			NewWinTypeList = [{Type, AddNum}|WinTypeList];
		AddNum when AddNum > 0 ->
			NewCount = Count + AddNum,
			NewWinTypeList = [{E, AddNum}|WinTypeList];
		_ ->
			NewCount = Count,
			NewWinTypeList = WinTypeList
	end,
	get_mahjong_fight_mahjong_count(L, Combination, IsTouch, IsKong, WinId, MahjongPlayer, MahjongState, NewCount, NewWinTypeList).
%% 互斥部分
get_mahjong_fight_mahjong_mutex_count([], _Combination, _IsTouch, _IsKong, _WinId, _MahjongPlayer, _MahjongState) ->
	0;
get_mahjong_fight_mahjong_mutex_count([E|L], Combination, IsTouch, IsKong, WinId, MahjongPlayer, MahjongState) ->
	NewCount = get_mahjong_fight_mahjong_count(E, Combination, IsTouch, IsKong, WinId, MahjongPlayer, MahjongState),
	if
		NewCount > 0 ->
			{E, NewCount};
		true ->
			get_mahjong_fight_mahjong_mutex_count(L, Combination, IsTouch, IsKong, WinId, MahjongPlayer, MahjongState)
	end.

%% 6
%% 天胡13 
get_mahjong_fight_mahjong_count(?MAHJONG_TIAN_HU, _Combination, true, false, _WinId, MahjongPlayer, MahjongState) ->
	#mahjong_fight_info{
					position = Position,
					meld = Meld,
					out = Out
	} = MahjongPlayer,
	if
		Meld == [] andalso Out == [] andalso MahjongState#r_mahjong_fight_state.banker == Position ->
			13;
		true ->
			0
	end;
%% 地胡13
get_mahjong_fight_mahjong_count(?MAHJONG_DI_HU, _Combination, false, false, _WinId, MahjongPlayer, MahjongState) ->
	#r_mahjong_fight_state{ banker = Banker, out_list = OutList, last_position = LastPosition} = MahjongState,
	if
		LastPosition == Banker andalso length(OutList) == 1 andalso MahjongPlayer#mahjong_fight_info.position =/= Banker ->
			13;
		true ->
			0
	end;
%% 8花 13
get_mahjong_fight_mahjong_count(?MAHJONG_EIGHT_FLOWER, _Combination, _IsTouch, _IsKong, _WinId, MahjongPlayer, _MahjongState) ->
	if
		length(MahjongPlayer#mahjong_fight_info.flower) >= length(?FLOWER_ID_LIST) ->
			13;
		true ->
			0
	end;
%%清一色10
get_mahjong_fight_mahjong_count(?MAHJONG_SAME_SUIT, Combination, _IsTouch, _IsKong, WinId, _MahjongPlayer, _MahjongState) ->
	MahjongType = ?MAHJONG_ID_TO_TYPE(WinId), %% 如果是8花胡牌，那没有清一色之说 
	F = fun(E, Acc) ->
			[EId|_] = tuple_to_list(E),
			if
				Acc == false ->
					false;
				?MAHJONG_ID_TO_TYPE(EId) =/= MahjongType ->
					false;
				true ->
					Acc
			end
		end,
	case lists:foldl(F, true, Combination) of
		true ->
			10;
		_ ->
			0
	end;
%% 字一色
get_mahjong_fight_mahjong_count(?MAHJONG_HONOUR_SUIT, Combination, _IsTouch, _IsKong, WinId, _MahjongPlayer, _MahjongState) ->
	MahjongType = ?MAHJONG_ID_TO_TYPE(WinId),
	if
		MahjongType == ?MAHJONG_HONOUR ->
			F = fun(E, Acc) ->
				[EId|_] = tuple_to_list(E),
				if
					Acc == false ->
						false;
					?MAHJONG_ID_TO_TYPE(EId) =/= MahjongType ->
						false;
					true ->
						Acc
				end
			end,
			case lists:foldl(F, true, Combination) of
				true ->
					10;
				_ ->
					0
			end;
		true ->
			0
	end;
	
% 四花 8
get_mahjong_fight_mahjong_count(?MAHJONG_FOUR_FLOWER, _Combination, _IsTouch, _IsKong, _WinId, MahjongPlayer, _MahjongState) ->
	F = fun(E, {Acc1, Acc2}) ->
			if
				E > 4 ->
					{Acc1, [E|Acc2]};
				true ->
					{[E|Acc1], Acc2}
			end
		end,
	{FourList1, FourList2} = lists:foldl(F, {[], []}, MahjongPlayer#mahjong_fight_info.flower),
	if
		length(FourList1) >= 4 orelse length(FourList2)->
			8;
		true ->
			0
	end;
%% 大吊 6
get_mahjong_fight_mahjong_count(?MAHJONG_LARGE_CRANE,  _, _IsTouch, _IsKong, _WinId, MahjongPlayer, _MahjongState) ->
	Length = get_mahjong_fight_basis_length(MahjongPlayer#mahjong_fight_info.basis),
	if
		Length == 1 -> %% 手上只有一张牌
			6;
		true ->
			0
	end;
%% 对对胡 6
get_mahjong_fight_mahjong_count(?MAHJONG_PONG_HU, Combination, _IsTouch, _IsKong, _WinId, _MahjongPlayer, _MahjongState) ->
	F = fun(E, Acc) ->
			[EId1, EId2|_] = tuple_to_list(E),
			if
				Acc == false ->
					false;
				EId1 =/= EId2 ->
					false;
				true ->
					Acc
			end
		end,
	case lists:foldl(F, true, Combination) of
		true ->
			6;
		_ ->
			0
	end;
%% 混一色6
get_mahjong_fight_mahjong_count(?MAHJONG_HUN_SUIT, Combination, _IsTouch, _IsKong, _WinId, _MahjongPlayer, _MahjongState) ->
	F = fun(E, {Acc1, Acc2} = Acc) ->
			[EId|_] = tuple_to_list(E),
			if
				Acc1 == false ->
					Acc;
				?MAHJONG_ID_TO_TYPE(EId) == ?MAHJONG_HONOUR orelse ?MAHJONG_ID_TO_TYPE(EId) == ?MAHJONG_WIND ->
					Acc;
				Acc2 == 0 ->
					{Acc1, EId};
				?MAHJONG_ID_TO_TYPE(EId) =/= ?MAHJONG_ID_TO_TYPE(Acc2) ->
					{false, Acc2};
				true ->
					Acc
			end
		end,
	case lists:foldl(F, {true, 0}, Combination) of
		{true, _} ->
			6;
		_ ->
			0
	end;
%% 海底老月MAHJONG_HAY
get_mahjong_fight_mahjong_count(?MAHJONG_HAY, _Combination, true, _IsKong, _WinId, _MahjongPlayer, MahjongState) ->
	#r_mahjong_fight_state{
						basis_list = BasisList
	} = MahjongState,
	if
		BasisList == [] -> %% 没有牌了而且还是自摸
			4;
		true ->
			0
	end;
%% 正风 2
get_mahjong_fight_mahjong_count(?MAHJONG_MAIN_WIND, _Combination, _IsTouch, _IsKong, _WinId, MahjongPlayer, _MahjongState) ->
	#mahjong_fight_info{
						meld = Meld,
						order = Order
	} = MahjongPlayer,
	%% 东风位 那就是4*10 + 1
	MahjongId = 10 * ?MAHJONG_WIND + Order,
	%%io:format("order~p~n",[{MahjongId, Order,Meld}]),
	F = fun({{_, EId}, _, _}, Acc)  ->
			if
				Acc == true  ->
					true;
				EId == MahjongId ->
					true;
				true ->
					Acc
			end
		end,
	case lists:foldl(F, false, Meld) of
		true ->
			2;
		_ ->
			0
	end;
%% 正花 2
get_mahjong_fight_mahjong_count(?MAHJONG_MAIN_FLOWER, _Combination, _IsTouch, _IsKong, _WinId, MahjongPlayer, _MahjongState) ->
	#mahjong_fight_info{
						flower = Flower,
						order = Order
	} = MahjongPlayer,
	F = fun(E, Acc) ->
			if
				E == Order orelse E == Order + 4 -> % 位置是2 那么只要有2和6一张花就可以
					Acc + 2;
				true ->
					Acc
			end
		end,
	lists:foldl(F, 0, Flower);
%% 平胡 1  combinationList == [{r_mahjong_fight_win_combination, 1, [{1,2,3}]}.....]
get_mahjong_fight_mahjong_count(?MAHJONG_PLAIN_HU, Combination,_IsTouch, _IsKong, _WinId, _MahjongPlayer, _MahjongState) ->
	%% 与平湖做互斥处理
	F = fun(E, Acc) ->
			EList = tuple_to_list(E),
			if
				Acc == false ->
					false;
				true ->
					case EList of
						[EId|_] when ?MAHJONG_ID_TO_TYPE(EId) == ?MAHJONG_WIND orelse ?MAHJONG_ID_TO_TYPE(EId) == ?MAHJONG_HONOUR ->
							false;
						[_EId1,_EId2] ->
							Acc;
						[EId1, EId2|_] when EId2 == EId1 ->
							false;
						_ ->
							Acc
					end
			end
		end,
	case lists:foldl(F, true, Combination) of
		true ->
			1;
		_ ->
			0
	end;
%% 边倒1 
get_mahjong_fight_mahjong_count(?MAHJONG_EDGE_HU, _Combination, _IsTouch, _IsKong, WinId, MahjongPlayer, _MahjongState) ->
	if
		?IS_CAN_EAT(WinId) andalso WinId rem 10 == 3 ->
			case lists:keyfind(WinId - 2 , 1, MahjongPlayer#mahjong_fight_info.basis) of
				{_Id1, Num1} when Num1 > 0 ->
					R1 = true;
				_ ->
					R1 = false
			end,
			case lists:keyfind(WinId - 1 , 1, MahjongPlayer#mahjong_fight_info.basis) of
				{_Id2, Num2} when Num2 > 0 ->
					R2 = true;
				_ ->
					R2 = false
			end,
			R = R1 andalso R2;
		?IS_CAN_EAT(WinId) andalso WinId rem 10 == 7 ->
			case lists:keyfind(WinId +1 , 1, MahjongPlayer#mahjong_fight_info.basis) of
				{_Id1, Num1} when Num1 > 0 ->
					R1 = true;
				_ ->
					R1 = false
			end,
			case lists:keyfind(WinId + 2 , 1, MahjongPlayer#mahjong_fight_info.basis) of
				{_Id2, Num2} when Num2 > 0 ->
					R2 = true;
				_ ->
					R2 = false
			end,
			R = R1 andalso R2;
		true ->
			R = false
	end,
	case R of
		true ->
			1;
		_ ->
			0
	end;
%%TODO		
%% 对倒1 可以把对倒，边倒放到一起，但是类型分开更好，代码多，不一定效率差， 最多14张牌 4{1，1，1}
get_mahjong_fight_mahjong_count(?MAHJONG_ORDER_HU, Combination, _IsTouch, _IsKong, WinId, MahjongPlayer, _MahjongState) ->
	F = fun(E, Acc) ->
			case E of
				{EId1, EId2,_EId3} when EId1 == EId2 andalso EId1 == WinId ->
					true;
				_ ->
					Acc
			end
		end,
	case lists:foldl(F, false, Combination) of
		true ->
			case lists:keyfind(WinId, 1, MahjongPlayer#mahjong_fight_info.basis) of
				{WinId, Num} when Num >= 2 ->
					1;
				_ ->
					0
			end;
		_ ->
			0
	end;
% 嵌套
get_mahjong_fight_mahjong_count(?MAHJONG_SET_HU, Combination, _IsTouch, _IsKong, WinId, MahjongPlayer,_MahjongState) ->
	EId1 = WinId -1,
	EId2 = WinId + 1,
	F = fun(E, Acc) ->
			case E of
				{EId1, WinId, EId2} ->
					true;
				_ ->
					Acc
			end
		end,
	case lists:foldl(F, false, Combination) of
		true ->
			case lists:keyfind(EId1, 1, MahjongPlayer#mahjong_fight_info.basis) of
				{EId1, Num1} when Num1 >= 1 ->
					R1 = true;
				_ ->
					R1 = false
			end,
			case lists:keyfind(EId2, 1, MahjongPlayer#mahjong_fight_info.basis) of
				{EId2, Num2} when Num2 >= 1 ->
					R2 = true;
				_  ->
					R2 = false
			end,
			if
				R1 andalso R2 ->
					1;
				true ->
					0
			end;
		_ ->
			0
	end;
%% 单吊 1
get_mahjong_fight_mahjong_count(?MAHJONG_ONE_CRANE,  Combination, IsTouch, IsKong, WinId, MahjongPlayer, MahjongState) ->
	F = fun(E, Acc) ->
			case E of
				{PairsId, PairsId} when PairsId == WinId ->
					true;
				_ ->
					Acc
			end
		end,
	case lists:foldl(F, false, Combination) of
		true ->
			1;
		_ ->
			get_mahjong_fight_mahjong_mutex_count(?MAHJONG_TYPE_MUTEX_HU_LIST, Combination, IsTouch, IsKong, WinId, MahjongPlayer, MahjongState)
	end;
%% 自摸 1
get_mahjong_fight_mahjong_count(?MAHJONG_MAIN_HU, _Combination, true, _IsKong, _WinId, _MahjongPlayer, _MahjongState) ->
	1;
%% 杠开
get_mahjong_fight_mahjong_count(?MAHJONG_KONG_HU, _Combination, true, true, _WinId, _MahjongPlayer, _MahjongState) ->
	1;
%% 中发白
get_mahjong_fight_mahjong_count(?MAHJONG_PONG_HONOUR, Combination, _IsTouch, _IsKong, _WinId, _MahjongPlayer, _MahjongState) ->
	F = fun(E, Acc)->
			ValueList = tuple_to_list(E),
			[EId|_] = ValueList,
			if
				?MAHJONG_ID_TO_TYPE(EId) == ?MAHJONG_HONOUR andalso length(ValueList) >= 3 ->
					Acc + 1;
				true ->
					Acc
			end
		end,
	lists:foldl(F, 0, Combination);
%% 圈风1
get_mahjong_fight_mahjong_count(?MAHJONG_CIRCLE_HONOUR, _CombinationList, _IsTouch, _IsKong, _WinId, MahjongPlayer, MahjongState) ->
	#r_mahjong_fight_state{
					feng_circle = FengCircle
	} = MahjongState,
	#mahjong_fight_info{
					order = Order,
					meld = Meld
	} = MahjongPlayer,
	MainHonourId = 10 * ?MAHJONG_HONOUR + FengCircle,
	F = fun({{_, EId}, _, _}, Acc) ->
			if
				Acc == true ->
					Acc;
				EId == MainHonourId ->
					true;
				true ->
					Acc
			end
		end,
	case lists:foldl(F, false, Meld) of
		true when Order == FengCircle ->
			2;
		true ->
			1;
		_ ->
			0
	end;
%% 门清
get_mahjong_fight_mahjong_count(?MAHJONG_MAIN_HAND, _Combination, true, _IsKong, _WinId, MahjongPlayer, _MahjongState) ->
	Length = get_mahjong_fight_basis_length(MahjongPlayer#mahjong_fight_info.basis),
	if
		Length >= 14 ->
			1;
		true ->
			0
	end;
%%野花
get_mahjong_fight_mahjong_count(?MAHJONG_OTHER_FLOWER, _Combination, _IsTouch, _IsKong, _WinId, MahjongPlayer, _MahjongState) ->
	#mahjong_fight_info{
						flower = Flower,
						order = Order
	} = MahjongPlayer,
	%%io:format("flower~p~n",[Flower]),
	F = fun(E, Acc) ->
			if
				E =/= Order andalso E =/= Order + 4 ->
					Acc + 1;
				true ->
					Acc
			end
		end,
	lists:foldl(F, 0, Flower);
get_mahjong_fight_mahjong_count(_Type, _CombinationList, _IsTouch, _IsKong, _WinId, _MahjongPlayer, _MahjongState) ->
	0.
% %%io:format("~ts~n",[unicode:characters_to_binary([25151,38388,21487,25171,29260,26102,38388,24050, 32463,32467,26463])]).
                         
%% 计算积分先算特殊的，没有特殊的再按正常的
%% 自摸 互包 A 10M B -10M
get_mahjong_fight_mahjong_score(IsTouch, IsKong, MahjongCount, WinPlayer, FailPlayerList, MahjongPlayerList, MahjongState) ->
	MahjongCount1 = MahjongCount * ?M,
	%% 先计算是不是送杠，是不是拉杆
	ScoreList1 = get_mahjong_fight_kong_score(IsTouch, IsKong, MahjongCount1, WinPlayer, MahjongState, []),
	%% 计算是不是胜者包别人
	ScoreList2 = get_mahjong_fight_pack_score_by_myself(IsTouch, MahjongCount1, WinPlayer, FailPlayerList, MahjongPlayerList, ScoreList1),
	%% 计算是不是别人包胜者
	ScoreList3 = get_mahjong_fight_pack_score_by_other(IsTouch, MahjongCount1, WinPlayer, FailPlayerList, MahjongPlayerList, ScoreList2),
	%% 计算正常的，如果已经有特殊，胡自动忽略
	ScoreList4 = get_mahjong_fight_normal_score(IsTouch, MahjongCount1, WinPlayer,FailPlayerList, ScoreList3),
	%%io:format("get_mahjong_fight_mahjong_score~p~n",[{ScoreList1, ScoreList2, ScoreList3, ScoreList4}]),
	%% 将{playerId, score} 放到MahjongPlayerList
	F = fun({EId, EScore}, AccMahjongPlayerList) ->
			% %%io:format("_____get_mahjong_fight_mahjong_score~p~n",[{EId, lists:keyfind(EId, 1, AccMahjongPlayerList)}]),
			case lists:keyfind(EId, #mahjong_fight_info.player_id, AccMahjongPlayerList) of
				false ->
					AccMahjongPlayerList;
				MahjongPlayer ->
					OldScore = MahjongPlayer#mahjong_fight_info.score,
					NewMahjongPlayer = MahjongPlayer#mahjong_fight_info{score = OldScore + EScore},
					%%io:format("_____get_mahjong_fight_mahjong_score~p~n",[{EId, OldScore, EScore}]),
					lists:keystore(EId, #mahjong_fight_info.player_id, AccMahjongPlayerList, NewMahjongPlayer)
			end
		end,
	NewMahjongPlayerList = lists:foldl(F, MahjongPlayerList, ScoreList4),
	% %%io:format("___~p~n",[NewMahjongPlayerList]),
	NewMahjongPlayerList.
%% 计算正常的，自摸没特殊，没人1M,放炮2M
%% 已经有特殊的，不用再计算正常的
get_mahjong_fight_normal_score(true, _MahjongCount, _WinPlayer, _FailPlayerList, ScoreList) when ScoreList =/= [] ->
	ScoreList;
%% 如果放炮有人三包了，那在这里算放炮者的
get_mahjong_fight_normal_score(false, MahjongCount, WinPlayer, [Fail|_FailPlayerList], ScoreList) when ScoreList =/= [] ->
	MyPack = check_mahjong_fight_pack(WinPlayer, Fail),
	OtherPack = check_mahjong_fight_pack(Fail, WinPlayer),
	if
		MyPack andalso OtherPack -> %% 互保
			AddNum = 8 * MahjongCount;
		MyPack -> %% 主包
			AddNum = 4 * MahjongCount;
		OtherPack ->%% 被包
			AddNum = 4 * MahjongCount;
		true -> %% 正常，但有非点报者被包
			AddNum = 2 * MahjongCount
	end,
	MyId = WinPlayer#mahjong_fight_info.player_id,
	FailId = Fail#mahjong_fight_info.player_id,
	[{MyId, AddNum}, {FailId, -AddNum}|ScoreList];
get_mahjong_fight_normal_score(true, MahjongCount, WinPlayer, FailPlayerList, ScoreList) ->
	MyId = WinPlayer#mahjong_fight_info.player_id,
	ScoreList1 = [{MyId, 3 * MahjongCount}|ScoreList],
	F = fun(E, AccScoreList) ->
			EId = E#mahjong_fight_info.player_id,
			[{EId, -MahjongCount}|AccScoreList]
		end,
	lists:foldl(F, ScoreList1, FailPlayerList);
get_mahjong_fight_normal_score(false, MahjongCount, WinPlayer, FailPlayerList, ScoreList) ->
	MyId = WinPlayer#mahjong_fight_info.player_id,
	ScoreList1 = [{MyId, 2 * MahjongCount}|ScoreList],
	F = fun(E, AccScoreList) ->
			EId = E#mahjong_fight_info.player_id,
			[{EId, -2 *MahjongCount}|AccScoreList]
		end,
	lists:foldl(F, ScoreList1, FailPlayerList).
	
%% 是送杠还是拉杠
get_mahjong_fight_kong_score(true, true, MahjongCount, WinPlayer, MahjongState, ScoreList) ->
	#r_mahjong_fight_state{
						last_position = LastPosition,
						last_mahjongid = LastMahjongId
	} = MahjongState,
	LastPlayerId = lib_mahjong_fight_procdict:get_mahjong_player_id_by_position(LastPosition),
	case lists:keyfind({?MAHJONG_MELD_TYPE_EXPOSED_KONG, LastMahjongId}, 1, WinPlayer#mahjong_fight_info.meld) of
		{_, PlayerId, _} when PlayerId == LastPlayerId -> %% 送杠5*M 不是送杠,应该是每家负责2M,但一台算到台数去了
			AddNum = 5 * MahjongCount,
			MyId = WinPlayer#mahjong_fight_info.player_id,
			[{MyId, AddNum}, {LastPlayerId, -AddNum}|ScoreList];
		_ ->
			ScoreList
	end;
%% 是不是拉杠胡
get_mahjong_fight_kong_score(false, false, MahjongCount, WinPlayer, MahjongState, ScoreList) ->
	#r_mahjong_fight_state{
						last_position = LastPosition,
						now_postion = NowPostion
	} = MahjongState,
	if
		LastPosition == NowPostion andalso LastPosition =/= 0 -> %% 如果胡牌的和操作的都是一样，那是送杠胡
			AddNum = 5 * MahjongCount,
			FailId = lib_mahjong_fight_procdict:get_mahjong_player_id_by_position(LastPosition),
			MyId = WinPlayer#mahjong_fight_info.player_id,
			[{MyId, AddNum},{FailId, -AddNum}|ScoreList];
		true ->
			ScoreList
	end;
%% 杠胡，已经算到台数离去了
get_mahjong_fight_kong_score(_, _, _, _, _, ScoreList) ->
	ScoreList.
%我包别人，别人包我
%% 先计算我包别人 先保存{player, score} 会报处理一些
get_mahjong_fight_pack_score_by_myself(true, MahjongCount, WinPlayer, FailPlayerList, _MahjongPlayerList, ScoreList) ->
	MyPackId = get_mahjong_fight_pack(WinPlayer),
	F = fun(E, AccScoreList) ->
			EId = E#mahjong_fight_info.player_id,
			MyId = WinPlayer#mahjong_fight_info.player_id,
			case check_mahjong_fight_pack(MyPackId, E) of
				true ->
					AddNum = 5 * MahjongCount,
					[{MyId, AddNum}, {EId, -AddNum}|AccScoreList];
				_ ->
					AccScoreList
			end
		end,
	NewScoreList = lists:foldl(F, ScoreList, FailPlayerList),
	NewScoreList;
%% 只算三包，先不算输家必须出的2M，因为可能互包，会重复计算
%% 放炮的话，担心三包重复，放炮者先不算
get_mahjong_fight_pack_score_by_myself(false, MahjongCount, WinPlayer, [Fail|_FailPlayerList], MahjongPlayerList, ScoreList) ->
	MyPackId = get_mahjong_fight_pack(WinPlayer),
	MyId = WinPlayer#mahjong_fight_info.player_id,
	FailId = Fail#mahjong_fight_info.player_id,
	MatchPlayerList = lists:keydelete(MyId, #mahjong_fight_info.player_id, MahjongPlayerList),
	F = fun(E, AccScoreList) ->
			EId = E#mahjong_fight_info.player_id,
			case check_mahjong_fight_pack(MyPackId, E) of
				true when FailId =/= EId ->
					AddNum = 2 * MahjongCount, 
					[{MyId, AddNum}, {EId, -AddNum}|AccScoreList];
				_ ->
					AccScoreList
			end
		end,
	NewScoreList = lists:foldl(F, ScoreList, MatchPlayerList),
	NewScoreList;
get_mahjong_fight_pack_score_by_myself(_IsTouch, _MahjongCount, _WinPlayer, _FailPlayerList, _MahjongPlayerList, ScoreList) ->
	%%io:format("get_mahjong_fight_pack_score_by_myself~p~n", [{IsTouch, ScoreList}]),
	ScoreList.

%% 计算别人包我的
get_mahjong_fight_pack_score_by_other(true, MahjongCount, WinPlayer, FailPlayerList, _MahjongPlayerList, ScoreList) ->
	MyId = WinPlayer#mahjong_fight_info.player_id,
	F = fun(E, AccScoreList) ->
			EId = E#mahjong_fight_info.player_id,
			case check_mahjong_fight_pack(E, WinPlayer) of
				true ->
					AddNum = 5 * MahjongCount,
					[{MyId, AddNum},{EId, -AddNum}|AccScoreList];
				false ->
					AccScoreList
			end
		end,
	lists:foldl(F, ScoreList, FailPlayerList);
get_mahjong_fight_pack_score_by_other(false, MahjongCount, WinPlayer, [Fail|_FailPlayerList], MahjongPlayerList, ScoreList) ->
	MyId = WinPlayer#mahjong_fight_info.player_id,
	FailId = Fail#mahjong_fight_info.player_id,
	MatchPlayerList = lists:keydelete(MyId, #mahjong_fight_info.player_id, MahjongPlayerList),
	F = fun(E, AccScoreList) ->
			EId = E#mahjong_fight_info.player_id,
			case check_mahjong_fight_pack(E, WinPlayer) of
				true when FailId =/= EId ->
					AddNum = 2 * MahjongCount,
					[{MyId, AddNum}, {EId, -AddNum}|AccScoreList];
				_ ->
					AccScoreList
			end
		end,
	lists:foldl(F, ScoreList, MatchPlayerList).

%% 检测是否3包玩家
check_mahjong_fight_pack(My, Other) when is_record(My, mahjong_fight_info) -> 
	Pack = get_mahjong_fight_pack(My),
	%% 如果我没有包任何人，那返回是fasle, 不会与other_player_id 相等，返回false
	Pack == Other#mahjong_fight_info.player_id;
check_mahjong_fight_pack(MyPackId, Other) ->
	MyPackId == Other#mahjong_fight_info.player_id.
%% 获取三包对象
get_mahjong_fight_pack(My) -> %% 只会包一家了，因为只有最多4组
	F = fun({_, PlayerId, _}, {AccCountList, PackId}) ->
			case lists:keyfind(PlayerId, 1, AccCountList) of
				false ->
					{[{PlayerId,1}|AccCountList], PackId};
				{PlayerId, Num} ->
					NewNum = Num +1,
					NewAccCountList = lists:keystore(PlayerId, 1, AccCountList, {PlayerId, NewNum}),
					if
						NewNum >= 3 ->
							{NewAccCountList, PlayerId};
						true ->
							{NewAccCountList, PackId}
					end
			end
		end,
	case lists:foldl(F, {[], 0}, My#mahjong_fight_info.meld) of
		{_, Pack} when Pack > 0 ->
			Pack;
		_ ->
			false
	end.


%% 这是从组合中 哪暗自比较多的组合
get_mahjong_fight_combination(CombinationList, Meld) ->
	F = fun({_, _, E}) ->
			E
		end,
	Combination = lists:map(F, Meld),
	WinCombination = get_mahjong_fight_combination(CombinationList, 0, []),
	#r_mahjong_fight_win_combination{
						combination = Combination1,
						pairs = PairsId
	} = WinCombination,
	Combination ++ Combination1 ++ [{PairsId, PairsId}].

get_mahjong_fight_combination([], _Acc, Combination) ->
	Combination;
get_mahjong_fight_combination([E|CombinationList], AccCount, Combination) ->
	Num = get_mahjong_fight_combination_meld_count(E#r_mahjong_fight_win_combination.combination),
	if
		Num >= AccCount -> %% 处理一开始的0
			get_mahjong_fight_combination(CombinationList, Num, E);
		true ->
			get_mahjong_fight_combination(CombinationList, AccCount, Combination)
	end.

%% 获取暗子的数量
get_mahjong_fight_combination_meld_count(E) ->
	get_mahjong_fight_combination_meld_count(E, 0).

get_mahjong_fight_combination_meld_count([], Count) ->
	Count;
get_mahjong_fight_combination_meld_count([{MahjongId1,MahjongId2, _MahjongId3}|CombinationList], Count) ->
	if
		MahjongId1 == MahjongId2 ->
			NewCount = Count +1;
		true ->
			NewCount = Count
	end,
	get_mahjong_fight_combination_meld_count(CombinationList, NewCount).






