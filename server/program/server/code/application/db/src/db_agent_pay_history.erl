%% @author icolun
%% @doc 充值验证记录
-module(db_agent_pay_history).
-behaviour(behaviour_db_game).


-include("record.hrl").
-include("table_record.hrl").


-define(DB_NAME, pay_history).

-export([table_info/0]).
-export([
         get/1,
         get_by_account/1,
         get_by_order_no/1,
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
        indices = [order_no],
        semantics = set,
        copies = disc_copies
    }.

%% ------------------------- 读写接口 -------------------------
get(Id) ->
    db_agent:read(?DB_NAME, Id).
%% 通过微信账号查找
get_by_account(Account) ->
    MatchHead = #pay_history{wechat_id = Account, _ = '_'},
    MatchGuard = [],
    MatchResult = ['$_'],
    db_agent:read_select(?DB_NAME, MatchHead, MatchGuard, MatchResult).
%% 通过支付订单号查找
get_by_order_no(OrderNo) ->
     db_agent:read_index(?DB_NAME, OrderNo, #pay_history.order_no).

update(PayHistory) ->
    db_agent:write(PayHistory).

get_first() ->
    {ok, Id} = db_agent:first(?DB_NAME),
    Id.

get_next(Key) ->
    {ok, Id} = db_agent:next(?DB_NAME, Key),
    Id.
