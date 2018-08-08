%% @author 
%% @doc 反馈

-module(ge_feedback).

-include("common.hrl").
-include_lib("protob/include/p06_feedback_pb.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).
%% 反馈
handle(#cs_feedback_fill{} = Msg) ->
    lib_feedback:cs_feedback_fill(Msg);

handle(Msg) ->
    logger:warning_msg("~p not match! ~p~n", [{?MODULE, ?LINE}, Msg]).
