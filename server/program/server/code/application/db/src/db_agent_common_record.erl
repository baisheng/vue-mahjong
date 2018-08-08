%% @author icolun
%% @doc 记录进行数据

-module(db_agent_common_record).
-behaviour(behaviour_db_game).

-include("record.hrl").
-include("table_record.hrl").

-define(DB_NAME, common_record).

-export([table_info/0]).
-export([
         get/1,
         get/0,
         update/1,
         delete/1
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
get(Id) ->
    db_agent:read(?DB_NAME, Id).
get() ->
    db_agent:read(?DB_NAME).
update(Info)->
    db_agent:write(Info#common_record{other = ""}).
delete(Id) ->
    db_agent:delete(?DB_NAME, Id).
