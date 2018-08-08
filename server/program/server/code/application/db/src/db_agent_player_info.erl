%% @author zouv
%% @doc 玩家表

-module(db_agent_player_info).
-behaviour(behaviour_db_game).

-include("record.hrl").
-include("table_record.hrl").

-define(DB_NAME, player_info).

-export([table_info/0]).
-export([
         get/1,
         get_by_account/1,
         get_by_name/1,
         update/1,
         get_first/0,
         get_next/1
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
        indices = [account],
        semantics = set,
        copies = disc_copies
    }.

%% ------------------------- 读写接口 -------------------------
get(PlayerId) ->
    db_agent:read(?DB_NAME, PlayerId).

get_by_account(Account) ->
    db_agent:read_index(?DB_NAME, Account, #player_info.account).

get_by_name(Name) ->
    MatchHead = #player_info{name = Name, _ = '_'},
    MatchGuard = [],
    MatchResult = ['$_'],
    db_agent:read_select(?DB_NAME, MatchHead, MatchGuard, MatchResult).

update(PlayerInfo) ->
    NewPlayerInfo = PlayerInfo#player_info{other = ""},
    db_agent:write(NewPlayerInfo).

get_first() ->
    {ok, Id} = db_agent:first(?DB_NAME),
    Id.

get_next(Key) ->
    {ok, Id} = db_agent:next(?DB_NAME, Key),
    Id.
