%% @author 
%% @doc 麻将玩法   

-module(ge_mahjong_fight).

-include("common.hrl").
-include_lib("protob/include/p03_mahjong_fight_pb.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).
%% 掷骰子
handle(#cs_mahjong_fight_die{} = _Msg) ->
    lib_mahjong_fight_player:cs_mahjong_fight_die();

%% 出牌
handle(#cs_mahjong_fight_play{} = Msg) ->
	lib_mahjong_fight_player:cs_mahjong_fight_play(Msg#cs_mahjong_fight_play.id);

%% 碰杠吃
handle(#cs_mahjong_fight_meld{} = Msg) ->
	#cs_mahjong_fight_meld{
						% type = Type,
						meld = PbMeld
	} = Msg,
	%%io:format("___cs_mahjong_fight_meld-~p~n",[Msg]),
	lib_mahjong_fight_player:cs_mahjong_fight_meld(PbMeld);

%% 取消 胡碰杠吃
handle(#cs_mahjong_fight_cancel{} = _Msg) ->
	lib_mahjong_fight_player:cs_mahjong_fight_cancel();

%% 听牌
handle(#cs_mahjong_fight_ready{} = Msg) ->
	% %%io:format("___-~p~n",[Msg]),
	lib_mahjong_fight_player:cs_mahjong_fight_ready(Msg#cs_mahjong_fight_ready.id);

handle(#cs_mahjong_fight_prepare{} = _Msg) ->
	lib_mahjong_fight_player:cs_mahjong_fight_prepare();

handle(#cs_mahjong_fight_win{} = _Msg) ->
	% %%io:format("————cs_mahjong_fight_win-~p~n",[_Msg]),
	lib_mahjong_fight_player:cs_mahjong_fight_win();

%% 查看记录
handle(#cs_mahjong_fight_record{} = _Msg) ->
	% %%io:format("cs_mahjong_fight_record-~p~n",[_Msg]),
	lib_mahjong_fight_player:cs_mahjong_fight_record();
%% 请求牌局信息
handle(#cs_mahjong_fight_update{} = _Msg) ->
	% %%io:format("cs_mahjong_fight_record-~p~n",[_Msg]),
	lib_mahjong_fight_player:cs_mahjong_fight_update();
handle(Msg) ->
    logger:warning_msg("~p not match! ~p~n", [{?MODULE, ?LINE}, Msg]).
