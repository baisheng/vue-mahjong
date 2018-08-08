-module(behaviour_db_game).

-include("record.hrl").

-export([
		 behaviour_info/1, 

		 init/0,
		 pick_table/1,
		 init_table/0,
		 init_table_auto_increment/0,
		 get_ets_table_list/0
		]).

behaviour_info(callbacks) ->
	[
	 {table_info, 0}
	];
behaviour_info(_Other) ->
    undefined.

init()	->
	ets:new(?ETS_BEHAVIOUR_DB_GAME, [{keypos, #ets_behaviour_db_game.module}, named_table, set, public]),
	ok.

pick_table(Path) ->
	if Path == [] ->
		ModulePath = filename:dirname(code:which(?MODULE));
	true ->
		ModulePath = Path
	end,
	ModuleList = behaviour_util:get_all_behaviour_mod(ModulePath, ?MODULE),
	pick_table1(ModuleList).

pick_table1(ModuleList) ->
	lists:foreach(fun(EMod)->
		ERecord = EMod:table_info(),
		ERecord1 = ERecord#ets_behaviour_db_game{module = EMod},
		ets:insert(?ETS_BEHAVIOUR_DB_GAME, ERecord1)
	end,
	ModuleList).

init_table() ->
	ets:foldl(fun(E, Acc)->
		#ets_behaviour_db_game{
			module = EModule,
			table_name = ETableName,
			table_fields = ETableFields,
			table_values = ETableList,
			indices = EIndices,
			semantics = ESemantics,
			copies = ECopies
		} = E,
		db_tools:create_table(ETableName, ETableFields, ETableList, EModule, EIndices, ESemantics, ECopies),
		Acc
	end,
	[], 
	?ETS_BEHAVIOUR_DB_GAME).

init_table_auto_increment() ->
	DbList = 
		ets:foldl(fun(E, Acc)->
			if E#ets_behaviour_db_game.is_auto_increment == true ->
				[E#ets_behaviour_db_game.table_name | Acc];
			true ->
				Acc
			end
		end,
		[], 
		?ETS_BEHAVIOUR_DB_GAME),
	db_auto_increment:init_table_auto_increment(DbList).

get_ets_table_list() ->
	ets:tab2list(?ETS_BEHAVIOUR_DB_GAME).
