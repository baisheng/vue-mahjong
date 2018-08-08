%% @author 
%% @doc 玩家相关

-module(ge_player).

-include("common.hrl").
-include_lib("protob/include/p04_player_pb.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

% %% 查询牌局统计
% handle(#cs_query_game_statistics{} = _Msg) ->
%     ok;

% %% 查找战绩
% handle(#cs_query_game_record{} = _Msg) ->
%     ok;
%% 充值验证
handle(#cs_player_recharge{} = Msg) ->
    lib_pay:pay_handle(Msg);
handle(Msg) ->
    logger:warning_msg("~p not match! ~p~n", [{?MODULE, ?LINE}, Msg]).
