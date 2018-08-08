
-module(behaviour_util).

-export([
	     get_path_module/1,
	     get_all_behaviour_mod/2
		]).

%% 获取模块
get_path_module(Path) ->
	{ok, ALLFiles} = file:list_dir(Path),
	lists:foldl(fun(EFileName, AccModules)->
					case get_path_module1(EFileName) of
						[] ->
							AccModules;
						ENewModule ->
							[ENewModule | AccModules]	
					end
				end,
				[],
				ALLFiles).

get_path_module1(FileName)->
	case string:right(FileName, 5) of
		".beam"->
			erlang:list_to_atom(string:substr(FileName, 1, string:len(FileName) - 5));
		_->
			[]
	end.

%% 获取指定behaviour的模块
get_all_behaviour_mod(Path, Behaviour) ->
	lists:filter(fun(Mod)-> check_mod_behaviour(Mod, Behaviour) end, get_path_module(Path)).

check_mod_behaviour(Mod, CheckBehav) ->
	check_mod_behaviour1(Mod:module_info('attributes'), CheckBehav).

check_mod_behaviour1([], _CheckBehav)-> false;
check_mod_behaviour1([{'behaviour', Behaviours} | L], CheckBehav)->
	case lists:member(CheckBehav, Behaviours) of
		true ->
			true;
		_ ->
			check_mod_behaviour1(L, CheckBehav)
	end;
check_mod_behaviour1([_ | L], CheckBehav) ->
	check_mod_behaviour1(L, CheckBehav).
