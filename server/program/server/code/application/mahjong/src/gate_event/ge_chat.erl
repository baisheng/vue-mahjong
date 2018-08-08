%% @author 
%% @doc 聊天

-module(ge_chat).

-include("common.hrl").
-include_lib("protob/include/p05_common_pb.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).


%% 查找
handle(#cs_common_chat{} = Msg) ->
	#cs_common_chat{
				type = Type,
				content = Content,
				toid = ToId
	} = Msg,
    lib_chat:cs_chat(Type, Content, ToId);

handle(Msg) ->
    logger:warning_msg("~p not match! ~p~n", [{?MODULE, ?LINE}, Msg]).
