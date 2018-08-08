%% @author zouv
%% @doc @todo 服务器开/关接口

-module(main).

-include("core.hrl").
-include("common.hrl").

-define(APP_LIST, [asn1, compiler, crypto, public_key, ssl, inets, hipe, mnesia, tools,
                   ranch, cowlib, cowboy, protobuffs, stdlib2, meck,rfc4627_jsonrpc,
                   util, db, protob, mahjong, gateway]).

-define(APP_LIST_FIGHT, [util, mahjong]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	     start/1, 
	 	 stop/0,
	 	 stop/1
	 	]).

%% -------------------------
%% 开启服务器
%% -------------------------
start([NodeFlag]) ->
	case NodeFlag of
		'fight' ->
			AppList = ?APP_LIST_FIGHT;
		_ ->
			AppList = ?APP_LIST
	end,
	%% 开启应用
	SuccList = start_app(AppList, []),
	if
		length(SuccList) == length(AppList) ->
			io:format("~n ___SERVER APP START SUCCESSFUL !!!");
		true ->
			logger:warning_msg("server app start Error! info = ~p~n", [AppList -- SuccList])
	end,
	io:format("~n~n~n"),
	ok.

%% 开启应用
start_app([], AccList) ->
	AccList;
start_app([E | L], AccList) ->
  	case application:start(E) of
		ok ->
			io:format("~n ___start application : ~p ", [E]),
			start_app(L, [E | AccList]);
		{error, {SkipError, App}} ->
			logger:warning_msg("error! application = ~p, info = ~p~n", [E, {SkipError, App}]),
			AccList;
		{error, Reason} ->
		    io:format("~p~p~n",[E,Reason]),
			stop_app(AccList),
			throw({error, {E, Reason}}),
			AccList
	end.

%% 停止应用
stop_app([]) ->
	ok;
stop_app([E | L]) ->
	application:stop(E),
	io:format("~n ___stop application : ~p", [E]),
	stop_app(L).

%% %% 加载配置
%% load_base_config([]) ->
%% 	%mnesia_monitor:set_env(dump_log_write_threshold, 5000).
%% 	ok;
%% load_base_config([E | L]) ->
%% 	data_gen:import_config(E),
%% 	load_base_config(L).

%% -------------------------
%% 停止服务器
%% -------------------------
%main:stop().
stop() ->
    case misc:is_main_node() of
        true ->
			stop(true);
		_ ->
			init:stop()
	end.

stop(Flag) ->
    config_app:set_online_limit(0),
    % tools:eprof_stop(),
    Time = 60,
    %% 回写玩家数据
    case stop_kick_player(Time) of
        {ok, LeftTime} ->
            logger:msg("~nstop kick player success! time = ~p~n", [Time - LeftTime]),
			%% 通知战斗节点关闭
			stop_cast_fight_node(),
            %% 回写全局进程数据
           	ok = stop_cast_mod_update_to_db(),
           	if
           		Flag == true ->
            		%erlang:halt();
            		init:stop();
        		true ->
        			skip
           	end;
        Size ->
            logger:msg("~nstop failed! left = ~p~n", [Size])
    end.

stop_kick_player(LeftTime) ->
    lib_player:kick_player_all(),
    stop_kick_player1(LeftTime).

stop_kick_player1(LeftTime) when LeftTime =< 0 -> ets:info(?ETS_PLAYER_PID, size);
stop_kick_player1(LeftTime) ->
    case ets:info(?ETS_PLAYER_PID, size) of
        Size when Size > 0 ->
            logger:msg("stop kick player1, size : ~p, left time = ~p ~n", [Size, LeftTime]),
            timer:sleep(1000),
            stop_kick_player1(LeftTime - 1);
        _ ->
            {ok, LeftTime}
    end.

stop_cast_fight_node() ->
	List = misc:get_fight_node_list(),
	[rpc:cast(E, ?MODULE, stop, []) || E <- List].

stop_cast_mod_update_to_db() ->
	% 关服时全局模块的回写由这里触发
	ModList =
		[
         %mod_ranking,
         %mod_arena,
		 %mod_player_offline
		 % mod_homeland
		],
	NowSeconds = mod_time:now_long_seconds(),
	stop_cast_mod_update_to_db1(ModList, NowSeconds).

stop_cast_mod_update_to_db1([], _StartSeconds) -> ok;
stop_cast_mod_update_to_db1([Mod | L], StartSeconds) ->
	Pid = erlang:apply(Mod, get_mod_pid, []),
	?CONDITIONAL_EXPRESSION(is_pid(Pid), ok, throw("stop cast mod update to db1 Error!")),
	logger:msg("mod = ~p, pid = ~p, stop cast mod update to db1~n", [Mod, Pid]),
	gen_server:cast(Pid, {'update_to_db', self()}),
	receive
		{ok, Pid} ->
			NowSeconds = mod_time:now_long_seconds(),
			logger:msg("mod = ~p, pid = ~p, time = ~p, stop cast mod update to db1 success!~n", [Mod, Pid, NowSeconds - StartSeconds]),
			stop_cast_mod_update_to_db1(L, NowSeconds);
		R ->
			logger:msg("mod = ~p, pid = ~p, stop cast mod update to db1 failed! return = ~p~n", [Mod, Pid, R]),
			fail
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
