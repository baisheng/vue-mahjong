%%----------------------------------------------------
%% 模块热更新
%% 
%% @author rolong@vip.qq.com
%% -modify: zouv 2013
%% 
%%----------------------------------------------------

-module(u).

-compile(export_all).

-include_lib("kernel/include/file.hrl").

%% 检测
%u:c(5, "../application/game/ebin/").
%u:c(5, "lib/game-1/ebin/").
c(N) when is_number(N) ->
	Path = "../application/game/ebin/",
	c(N, Path);
c(_) -> 
	info("ERROR======> Badarg", []).

c(M, Path) when is_integer(M) ->
    case file:list_dir(Path) of
        {ok, FileList} ->
            Files = get_new_file(FileList, M * 60, Path),
			info("---------check modules---------~n~p ~p", [length(Files), Files]);
        Any -> 
			info("Error Dir: ~w", [Any])
    end.

%% 更新
%u:u(5, "../application/game/ebin/").
%u:u(5, "../application/network_proto/ebin/").
%u:u(5, "lib/game-1/ebin/").
%u:u(5, "lib/db-1/ebin/").
%u:u(5, "lib/network_proto-1/ebin/").
u(m) ->
	StartTime = util:unixtime(),
	info("----------makes----------", []),
	c:cd("../"),
	make:all(),
	c:cd("ebin"),
	EndTime = util:unixtime(),
	Time = EndTime - StartTime,
	info("Make Time : ~w s", [Time]),
	u(Time / 60);
u(Files) when is_list(Files) ->
	info("---------modules---------~n~p~n", [Files]),
	load(Files);
u(N) when is_number(N) ->
	Path = "../application/game/ebin/",
	u(N, Path);
u(_) -> 
	info("ERROR======> Badarg", []).

u(N, Path) when is_number(N) ->
	case file:list_dir(Path) of
		{ok, FileList} -> 
			Files = get_new_file(FileList, util_math:ceil(N * 60) + 3, Path),
			info("---------modules---------~n~p ~p", [length(Files), Files]),
			load(Files);
		Any -> 
			info("Error Dir: ~w", [Any])
	end.

%% admin()->
%%     spawn(fun()->u(m) end),
%%     ok.
%% 
%% %% 编译
%% %% m(['src/data/*','src/lib/lib_goods.erl'])
%% m(Files) when is_list(Files) ->
%%     StartTime = util:unixtime(),
%%     info("----------makes----------~n~w~n", [Files]),
%%     c:cd("../"),
%%     Res = make:files(Files, [debug_info, {i, "include"}, {outdir, "ebin"}]),
%%     c:cd("ebin"),
%%     EndTime = util:unixtime(),
%%     Time = EndTime - StartTime,
%%     info("Make Time : ~w s", [Time]),
%%     Res.

%% =========================
info(V) ->
    info(V, []).

info(V, P) ->
    io:format(V ++ "~n", P).

get_new_file(Files, S, Path) -> 
    get_new_file1(Files, S, Path, []).

get_new_file1([], _S, _Path, Result) -> 
	Result;
get_new_file1([H | T], S, Path, Result) ->
	case string:tokens(H, ".") of
        [Left, Right] when Right =:= "beam" ->
			FullFile = lists:concat([Path, H]),
            case file:read_file_info(FullFile) of
                {ok, FileInfo} -> 
                    Now = calendar:local_time(),
                    case calendar:time_difference(FileInfo#file_info.mtime, Now) of
                        {Days, Times} -> 
                            Seconds = calendar:time_to_seconds(Times), 
                            case Days =:= 0 andalso Seconds < S of
                                true ->
                                    FileName = list_to_atom(Left),
                                    NewResult = [FileName | Result];
                                false -> 
									NewResult = Result
                            end;
                        _ -> 
							NewResult = Result
                    end;
                _ ->
					NewResult = Result
            end;
        _ ->
			NewResult = Result
    end,
    get_new_file1(T, S, Path, NewResult).

load([]) -> 
	ok;
load([FileName | T]) ->
    c:l(FileName),
    info("loaded: ~w", [FileName]),
    load(T).


