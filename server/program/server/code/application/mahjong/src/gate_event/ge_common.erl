%% @author 
%% @doc 记录 公用相关

-module(ge_common).

-include("common.hrl").
-include_lib("protob/include/p05_common_pb.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

%% 查询牌局统计
handle(#cs_query_game_statistics{} = _Msg) ->
    lib_record:cs_query_game_statistics();

%% 查找战绩
handle(#cs_query_game_record{} = Msg) ->
    lib_record:cs_query_game_record(Msg#cs_query_game_record.recordid);

%% 查找
handle(#cs_query_game_find{} = _Msg) ->
    lib_record:cs_query_game_find();
%% 聊谈
handle(#cs_common_chat{} = Msg) ->
	#cs_common_chat{
				type = Type,
				content = Content,
				toid = ToId
	} = Msg,
    lib_chat:cs_chat(Type, Content, ToId);

handle(Msg) ->
    logger:warning_msg("~p not match! ~p~n", [{?MODULE, ?LINE}, Msg]).
