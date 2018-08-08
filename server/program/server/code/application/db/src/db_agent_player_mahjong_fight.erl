%% @author icolun
%% @doc 麻将信息

-module(db_agent_player_mahjong_fight).
-behaviour(behaviour_db_game).

-include("record.hrl").
-include("table_record.hrl").

-define(DB_NAME, player_mahjong_fight).

-export([table_info/0]).
-export([
         get/1,
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
        indices = [],
        semantics = set,
        copies = disc_copies
    }.

%% ------------------------- 读写接口 -------------------------
get(PlayerId) ->
    db_agent:read(?DB_NAME, PlayerId).

update(PlayerMahjongFight) ->
    NewPlayerMahjongFight = PlayerMahjongFight#player_mahjong_fight{other = ""},
    db_agent:write(NewPlayerMahjongFight).
