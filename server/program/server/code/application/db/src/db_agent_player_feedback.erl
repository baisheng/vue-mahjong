%% @author icolun
%% @doc 反馈记录

-module(db_agent_player_feedback).
-behaviour(behaviour_db_game).

-include("record.hrl").
-include("table_record.hrl").

-define(DB_NAME, player_feedback).

-export([table_info/0]).
-export([
         get/1,
         get_by_player_id/1,
         get_by_feed_type/1,
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
        is_auto_increment  = true,
        indices = [],
        semantics = set,
        copies = disc_copies
    }.

%% ------------------------- 读写接口 -------------------------
get(Id) ->
    db_agent:read(?DB_NAME, Id).

get_by_player_id(PlayerId) ->
    MatchHead = #player_feedback{player_id = PlayerId, _ = '_'},
    MatchGuard = [],
    MatchResult = ['$_'],
    db_agent:read_select(?DB_NAME, MatchHead, MatchGuard, MatchResult).

get_by_feed_type(FeedType) ->
    MatchHead = #player_feedback{feed_type = FeedType, _ = '_'},
    MatchGuard = [],
    MatchResult = ['$_'],
    db_agent:read_select(?DB_NAME, MatchHead, MatchGuard, MatchResult).
update(Info) ->
    NewInfo = Info#player_feedback{other = ""},
    db_agent:write(NewInfo).
