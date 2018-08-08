%% @author zouv
%% @doc @todo protobuf

-module(make_proto).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/0]).

%% ====================================================================
%% Internal functions
%% ====================================================================
create() ->
	{ok, Path} = file:get_cwd(),
	%CodePath = filename:join([filename:dirname(code:which(?MODULE)), "../"]),
	%io:format("~n CodePath ~p", [CodePath]),
	case file:list_dir(Path) of
		{ok, FileList} ->
			%% 生成
			FunCreate = 
				fun(EFileName) ->
					case string:tokens(EFileName, ".") of
						[_, "proto"] ->
							io:format("~n===~n=== filename:~p~n===", [EFileName]),
							protobuffs_compile:scan_file(EFileName),
                            protobuffs_compile:generate_source(EFileName),
							ok;
						_ ->
							skip
					end
				end,
			lists:foreach(FunCreate, FileList),
			ok;
		{error, Reason} ->
			io:format("~n error! info = ~p", [Reason])
	end,
	erlang:halt().
