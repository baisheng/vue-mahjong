%% @author zouv
%% @doc 玩家表

-module(db_agent_player_room).
-behaviour(behaviour_db_game).

-include("record.hrl").
-include("table_record.hrl").

-define(DB_NAME, player_room).

-export([table_info/0]).
-export([
         get/1,
         get_by_player_id/1,
         update/1
        ]).

%% ====================================================================
%% Internal functions
%% ====================================================================
table_info() ->
    #ets_behaviour_db_game{
        table_name = ?DB_NAME,
        table_fields = record_info(fields, ?DB_NAME),
        table_values = #?DB_NAME{},
        is_auto_increment  = false,
        indices = [player_id],
        semantics = set,
        copies = disc_copies
    }.

%% ------------------------- 读写接口 -------------------------
get(Id) ->
    db_agent:read(?DB_NAME, Id).

get_by_player_id(PlayerId) ->
    db_agent:read_index(?DB_NAME, PlayerId, #player_room.player_id).

update(PlayerRoom) ->
    NewPlayerRoom = PlayerRoom#player_info{other = ""},
    db_agent:write(NewPlayerRoom).
