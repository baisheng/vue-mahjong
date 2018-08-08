%% @author zouv
%% @doc 玩家表自增Id

-module(db_agent_common_auto_id).
-behaviour(behaviour_db_game).

-include("record.hrl").
-include("table_record.hrl").

-define(DB_NAME, common_auto_id).

-export([table_info/0]).
-export([
         get/1,
         update/2
        ]).

%% ====================================================================
%% Internal functions
%% ====================================================================
table_info() ->
    #ets_behaviour_db_game{
        table_name = ?DB_NAME,
        table_fields = record_info(fields, ?DB_NAME),
        table_values = #?DB_NAME{},
        is_auto_increment  = true,
        indices = [],
        semantics = set,
        copies = disc_copies
    }.

%% ------------------------- 读写接口 -------------------------
get(Flag) ->
    case db_agent:read(?DB_NAME, Flag) of
        [Record] ->
            Record#?DB_NAME.index;
        _ ->
            0
    end.

update(Flag, Index)->
    db_agent:write({?DB_NAME, Flag, Index}).
