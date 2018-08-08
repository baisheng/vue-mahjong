%% icolun
%% 麻将进程字典
-module(lib_mahjong_fight_procdict).
-include("mahjong_fight.hrl").
-include("record.hrl").
-include("common.hrl").
-include_lib("db/include/record.hrl").
% -compile(export_all).
-export([
		get_dic_mahjong_player_list/0,                      %% 战斗玩家数据
		get_mahjong_by_position/1,							%% 通过位置获取玩家
		get_mahjong_by_player_id/1,							%% 通过id
		get_mahjong_player_id_by_position/1,				%% 通过id获取位置
		get_mahjong_position_by_player_id/1,				%% 通过位置获取id

		update_dic_mahjong_player_list/1,					%% 
		update_mahjong_by_position/1,
		update_mahjong_by_player_id/1,
		delete_mahjong_by_player_id/1,

		get_dic_mahjong_fight_state/0,						%% 玩家公共牌局信息 与玩家无关的信息
		update_dic_magjong_fight_state/1,

		get_dic_mahjong_fight_watch_list/0,					%% 观战人员信息
		delete_mahjong_fight_watch/1,
        get_mahjong_watch_by_player_id/1,                   %% 获取观战信息

		update_dic_mahjong_fight_watch_list/1,
		update_mahjong_fight_watch/1,

		get_dic_mahjong_common_record/0,					%% 当前牌局战绩
		update_dic_mahjong_common_record/1
		]).

%% 进程字典
%% 玩家信息列表
get_dic_mahjong_player_list() ->
	case get(?DIC_MAHJONG_PLAYER_LIST) of
		?UNDEFINED ->
			[];
		List ->
			List
	end.
get_mahjong_by_player_id(PlayerId) ->
	List = get_dic_mahjong_player_list(),
	get_mahjong_by_player_id(PlayerId, List).
get_mahjong_by_player_id(PlayerId, List) ->
	lists:keyfind(PlayerId, #mahjong_fight_info.player_id, List).
get_mahjong_by_position(Position) ->
	List = get_dic_mahjong_player_list(),
	lists:keyfind(Position, #mahjong_fight_info.position, List).
get_mahjong_player_id_by_position(Position) ->
	case get_mahjong_by_position(Position) of
		false ->
			0;
		MahjongPlayer ->
			MahjongPlayer#mahjong_fight_info.player_id
	end.
get_mahjong_position_by_player_id(PlayerId) ->
	case get_mahjong_by_player_id(PlayerId) of
		false ->
			0;
		MahjongPlayer ->
			MahjongPlayer#mahjong_fight_info.position
	end.

update_dic_mahjong_player_list(List) ->
	F = fun(E, Acc) ->
			#mahjong_fight_info{
						player_id = PlayerId,
						player_name = PlayerName,
						position = Position,       	%% 这个位置是唯一的，就像玩家id一样，好比座位
						circle = Ciecle,             %% 圈位
						order = Order    
			} = E,
			[{PlayerId, PlayerName, Position, Order, Ciecle}|Acc]
			end,
	List1 = lists:foldl(F, [], List),
	io:format("update_dic_mahjong_player_list~p~n", [List1]),
	put(?DIC_MAHJONG_PLAYER_LIST, List).
update_mahjong_by_player_id(Info) ->
	List = get_dic_mahjong_player_list(),
	NewList = lists:keystore(Info#mahjong_fight_info.player_id, #mahjong_fight_info.player_id, List, Info),
	update_dic_mahjong_player_list(NewList).
update_mahjong_by_position(Info) ->
	List = get_dic_mahjong_player_list(),
	NewList = lists:keystore(Info#mahjong_fight_info.position, #mahjong_fight_info.position, List, Info),
	update_dic_mahjong_player_list(NewList).

%% 删除
delete_mahjong_by_player_id(PlayerId) ->
	List = get_dic_mahjong_player_list(),
	NewList = lists:keydelete(PlayerId, #mahjong_fight_info.player_id, List),
	update_dic_mahjong_player_list(NewList).
%% 观战人员信息
get_dic_mahjong_fight_watch_list() ->
	case get(?DIC_MAHJONG_WATCH_LIST) of
		?UNDEFINED ->
			[];
		List ->
			List
	end.
get_mahjong_watch_by_player_id(PlayerId) ->
	List = get_dic_mahjong_fight_watch_list(),
	lists:keyfind(PlayerId, #mahjong_fight_info.player_id, List).
update_dic_mahjong_fight_watch_list(List) ->
	put(?DIC_MAHJONG_WATCH_LIST, List).
update_mahjong_fight_watch(Info) ->
	List = get_dic_mahjong_fight_watch_list(),
	NewList = lists:keystore(Info#mahjong_fight_info.player_id, #mahjong_fight_info.player_id, List, Info),
	update_dic_mahjong_fight_watch_list(NewList).
delete_mahjong_fight_watch(PlayerId) ->
	List = get_dic_mahjong_fight_watch_list(),
	NewList = lists:keydelete(PlayerId, #mahjong_fight_info.player_id, List),
	update_dic_mahjong_fight_watch_list(NewList).	

%% 麻将战斗信息
get_dic_mahjong_fight_state() ->
	case get(?DIC_MAHJONG_FIGHT_STATE) of
		?UNDEFINED ->
			#r_mahjong_fight_state{};
		Info ->
			Info
	end.
update_dic_magjong_fight_state(Info) ->
	io:format("update_dic_magjong_fight_state~p~n",[Info#r_mahjong_fight_state.banker]),
	put(?DIC_MAHJONG_FIGHT_STATE, Info).

%% 牌局记录数据
get_dic_mahjong_common_record() ->
	case get(?DIC_MAHJONG_COMMON_RECORD) of
		?UNDEFINED ->
			#common_record{};
		Info ->
			Info
	end.
update_dic_mahjong_common_record(Info) ->
	put(?DIC_MAHJONG_COMMON_RECORD, Info),
	lib_record_process:update_common_record(Info).
