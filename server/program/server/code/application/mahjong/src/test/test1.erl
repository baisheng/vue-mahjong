%% @author zouv
%% @doc 房间信息管理

-module(test1).

-include("common.hrl").
-include_lib("protob/include/p02_room_pb.hrl").

-export([
    test1/0
    
 ]).

test1() ->
    Str = "账号无效",
    io:format("___11 ~p~n", [{Str, unicode:characters_to_binary(Str)}]),
    % #sc_room_enter_response{
    %     result = 1,
    %     % reason = list_to_binary(Reason),
    %     % reason = unicode:characters_to_binary(Reason),
    %     reason = "1",
    %     members = []
    % }.
    ok.
