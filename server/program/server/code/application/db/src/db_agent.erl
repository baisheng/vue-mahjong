%% @author zouv
%% @doc 数据库接口代理

-module(db_agent).

-include_lib("stdlib/include/qlc.hrl").

-export([
		 read/1, 
		 read/2, 
		 read_index/3, 
		 read_select/4,
		 write/1, 
		 delete/2,
		 clear/1,
		 first/1,
         last/1,
         next/2,
		 prev/2,
		 
		 %% 事务
		 t_write/1,
		 t_delete/2,
		 t_abort/1,
         run_transaction/1
		]).

%% ---------------------------------------------
read(Table)->
	ReadFun = fun()-> qlc:e(qlc:q([X || X <- mnesia:table(Table)])) end,
	case mnesia:transaction(ReadFun) of
		{aborted, Reason} -> logger:msg("read error ~p Table ~p ~n",[Reason,Table]), {failed, Reason};
		{atomic, []} -> [];
		{atomic, Result}-> Result
	end.

read(Table, Key)->
	case catch  mnesia:dirty_read({Table,Key}) of
		{'EXIT',Reason} -> logger:msg("read error ~p Table ~p ~n",[Reason,Table]), {failed, Reason};
		Result when is_list(Result) -> Result;
		Result -> logger:msg("read error ~p ~n", [Result]), {failed, Result}
	end.

read_index(Table, SecondaryKey, Pos)->
	case catch  mnesia:dirty_index_read(Table, SecondaryKey, Pos) of
		{'EXIT', Reason} -> logger:msg("read_index error ~p Table ~p ~n", [Reason, Table]), {failed, Reason};
		Result when is_list(Result) -> Result;
		Result -> logger:msg("read_index error ~p ~n", [Result]), {failed, Result}
	end.

read_select(Table, MatchHead, MatchGuard, MatchResult) ->
	Fun = 
		fun() ->
			mnesia:select(Table, [{MatchHead, MatchGuard, MatchResult}])
		end,
	case mnesia:transaction(Fun) of
		{aborted, Reason} -> logger:msg("read error ~p Table ~p ~n", [Reason, Table]), {failed, Reason};
        {atomic, List} -> List
	end.

write(Object)->
	case catch mnesia:dirty_write(Object) of
		{'EXIT', Reason} -> logger:msg("write error ~p~n Object ~p ~n", [Reason, Object]), {failed, Reason};
		ok -> 'ok'
	end.

delete(Table, Key) ->
	case catch mnesia:dirty_delete({Table, Key}) of
		{'EXIT', Reason} -> {failed, Reason};
		ok -> 'ok'
	end.

clear(Table) ->
	case mnesia:clear_table(Table) of
		{aborted, Reason} -> logger:msg("clear error ~p~n Object ~p ~n", [Reason, Table]), {failed, Reason}; 
        {atomic, ok} -> 'ok'
	end.

first(Table) ->
    case catch mnesia:dirty_first(Table) of
        {'EXIT', Reason} ->
            logger:msg("get first key ~p Table ~p ~n",[Reason,Table]),
            {failed, Reason};
        Result ->  
            {ok, Result}
    end.

last(Table) ->
    case catch mnesia:dirty_last(Table) of
        {'EXIT', Reason} ->
            logger:msg("get last key ~p Table ~p ~n",[Reason, Table]),
            {failed, Reason};
        Result ->
            {ok, Result}
    end.

next(Table, Key) ->
    case catch mnesia:dirty_next(Table, Key) of
        {'EXIT', Reason} ->  
            logger:msg("get next key ~p Table ~p ~n",[Reason,Table]),
            {failed, Reason};
        Result ->  
            {ok, Result}
    end.

prev(Table, Key) ->
    case catch mnesia:dirty_prev(Table, Key) of
        {'EXIT', Reason} ->
            logger:msg("get prev key ~p Table ~p ~n",[Reason,Table]),
            {failed, Reason};
        Result ->
            {ok, Result}
    end.

%% ---------------------------------------------
%% 事务写
t_write(Object) ->
    mnesia:write(Object).

%% 事务删
t_delete(Table, Key) ->
    mnesia:delete({Table, Key}).

t_abort(Reason) ->
    mnesia:abort(Reason).

run_transaction(Transaction)->
    mnesia:sync_transaction(Transaction).
