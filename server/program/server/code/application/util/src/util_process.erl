%% @author zouv
%% @doc @todo 进程相关

-module(util_process).

-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% 注册进程(本地)
register_local(RegName, Pid) ->
	true = erlang:register(RegName, Pid),
	ok.

get_local_pid(RegName) ->
	erlang:whereis(RegName).

%% 注册进程(全局)
register_global(RegName, Pid) ->
	yes = global:re_register_name(RegName, Pid),
	ok.

get_global_pid(RegName) ->
	global:whereis_name(RegName).


