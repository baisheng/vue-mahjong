%% @author zouv
%% @doc 房间

-module(ge_room).

-include("common.hrl").
-include_lib("protob/include/p02_room_pb.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/1]).

handle(#cs_room_find{} = Msg) ->
    lib_room:cs_room_find(Msg);

handle(#cs_room_sit{} = Msg) ->
    lib_room:cs_room_sit(Msg);

handle(#cs_room_quit{} = _Msg) ->
    lib_room:cs_room_quit();

handle(#cs_room_start_mahjong{} = Msg) ->
    lib_room:cs_room_start_mahjong(Msg#cs_room_start_mahjong.roomid);

handle(Msg) ->
    logger:warning_msg("~p not match! ~p~n", [{?MODULE, ?LINE}, Msg]).
