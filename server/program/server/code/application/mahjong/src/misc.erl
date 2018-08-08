%% @author zouv
%% @doc 各种处理

-module(misc).

-include("core.hrl").
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         get_mod_pid/1,
         get_local_server_mod_pid/1,
         get_cross_server_mod_pid/1,
         get_global_mod_pid/2,
         get_global_mod_pid_simple/2,

         get_player_process_name/1,
         get_room_process_name/1,
         get_local_process_name/1,
         is_process_alive/1,

         is_main_node/0,
         is_node_fight/1,
         get_main_node/0,
         get_local_main_node_for_auto_link/0,
         get_fight_node/1,
         get_fight_node_list/0,
         check_node_valid/0,
         check_node_valid_and_update/1
        ]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% 获取LocalModPid
get_mod_pid(Module) ->
	ModProcessName = get_local_process_name(Module),
	case util_process:get_local_pid(ModProcessName) of
		ModPid when is_pid(ModPid) ->
			case erlang:is_process_alive(ModPid) of
				true ->
					ModPid;
				_ ->
					get_mod_pid1(Module, ModProcessName)
			end;
		_ ->
			get_mod_pid1(Module, ModProcessName)
	end.

get_mod_pid1(Module, ModProcessName) ->
    global:set_lock({ModProcessName, undefined}),
    ModPid = 
    	case supervisor:start_child(?APP_SUP, {Module, {Module, start_link, [ModProcessName]}, permanent, 10000, worker, [Module]}) of
    		{ok, Pid} ->
    			Pid;
    		Error ->
    			logger:warning_msg("start mod pid1 Error! module = ~p, ~ninfo = ~p ~n~p", [Module, Error, erlang:get_stacktrace()]),
    			undefined
    	end,
    global:del_lock({ModProcessName, undefined}),
    ModPid.

%% 获取GolbaoModPid
get_global_mod_pid(Module, GlobalUniqueFlag) ->
    ModProcessName = get_global_process_name(Module, GlobalUniqueFlag),
    case util_process:get_global_pid(ModProcessName) of
        ModPid when is_pid(ModPid) ->
            case misc:is_process_alive(ModPid) of
                true ->
                    ModPid;
                _ ->
                    get_mod_pid1(Module, ModProcessName)
            end;
        _ ->
            get_mod_pid1(Module, ModProcessName)
    end.

get_global_mod_pid_simple(Module, GlobalUniqueFlag) ->
    ModProcessName = get_global_process_name(Module, GlobalUniqueFlag),
    case util_process:get_global_pid(ModProcessName) of
        ModPid when is_pid(ModPid) ->
            case misc:is_process_alive(ModPid) of
                true ->
                    ModPid;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

%% 获取LocalServerModPid
get_local_server_mod_pid(Module) ->
    ModProcessName = get_local_server_process_name(Module),
    case util_process:get_global_pid(ModProcessName) of
        ModPid when is_pid(ModPid) ->
            case misc:is_process_alive(ModPid) of
                true ->
                    ModPid;
                _ ->
                    get_local_server_mod_pid1(Module, ModProcessName)
            end;
        _ ->
            get_local_server_mod_pid1(Module, ModProcessName)
    end.

get_local_server_mod_pid1(Module, ModProcessName) ->
    case is_main_node() of
        true ->
            get_mod_pid1(Module, ModProcessName);
        _ ->
            undefined
    end.

%% 获取CrossServerModPid
get_cross_server_mod_pid(Module) ->
    ModProcessName = get_cross_server_process_name(Module),
    case util_process:get_global_pid(ModProcessName) of
        ModPid when is_pid(ModPid) ->
            case misc:is_process_alive(ModPid) of
                true ->
                    ModPid;
                _ ->
                    get_cross_server_mod_pid1(Module, ModProcessName)
            end;
        _ ->
            get_cross_server_mod_pid1(Module, ModProcessName)
    end.

get_cross_server_mod_pid1(Module, ModProcessName) ->
    case config:get_server_id() == config:get_cross_server_id() andalso misc:is_main_node() of
        true ->
            get_mod_pid1(Module, ModProcessName);
        _ ->
            undefined
    end.

%% 获取玩家进程名
get_player_process_name(PlayerId) when erlang:is_integer(PlayerId) ->
    lists:concat(["player_process_", PlayerId]).

get_room_process_name(UniqueFlag) ->
    ServerId = config:get_server_id(),
    lists:concat(["room_process_", ServerId, "_", UniqueFlag]).

%% 获取本地进程名
get_local_process_name(Module) ->
	erlang:list_to_atom(lists:concat([Module, "_local_", 0])).

%% 获取全局进程名
get_global_process_name(Module, GlobalUniqueFlag) ->
    erlang:list_to_atom(lists:concat([Module, "_global_", GlobalUniqueFlag])).

%% 获取本服进程名（每个服，主节点唯一）
get_local_server_process_name(Module) ->
    ServerId = config:get_server_id(),
    NodeId = ?MAIN_NODE_ID,
    erlang:list_to_atom(lists:concat([Module, "_", ServerId, "_", NodeId])).

%% 获取跨服进程名（每个跨服组，主节点唯一）
get_cross_server_process_name(Module) ->
    ServerId = config:get_cross_server_id(),
    NodeId = ?MAIN_NODE_ID,
    erlang:list_to_atom(lists:concat([Module, "_", ServerId, "_", NodeId])).

%% 跨节点进程是否存活
is_process_alive(Pid) ->    
    try 
        if
            is_pid(Pid) ->
                case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                    {badrpc, _Reason}  ->
                        false;
                    Res ->
                        Res
                end;
            true ->
                false
        end
    catch 
        _:_ -> 
            false
    end.

is_main_node() ->
    config:get_node_id() == ?MAIN_NODE_ID.

%% ========================= 依賴約定的節點命名規則
%% 备注：
%%  1、此处约定主节点名称需与Cookie名称一致
%%  2、戰鬥節點名称為主节点名称拼上"_f節點Id"：game_f10@ip
is_node_fight(NodeName) ->
    StrNodeName = atom_to_list(NodeName),
    case string:tokens(StrNodeName, "@") of
        [PreName, _IP] ->
            Cookie = erlang:get_cookie(),
            BaseName = atom_to_list(Cookie),
            case string:tokens(PreName, "_") of
                [BaseName, TyptName] ->
                    is_node_fight1(TyptName);
                _ ->
                    false
            end;
        _ ->
            false
    end.

is_node_fight1(TyptName) ->
    case string:str(TyptName, "f") == 1 of
        true ->
            case string:to_integer(string:sub_string(TyptName, 2)) of % 找到"_f10"即认为是战斗节点
                {error, _Reason} ->
                    false;
                {_NodeId, _Rest} ->
                    true
            end;
        _ ->
            false
    end.

get_main_node() ->
    case is_main_node() of
        true ->
            node();
        _ ->
            [MainNodeInfo] = ets:lookup(?ETS_SERVER_NODE, ?MAIN_NODE_ID),
            Cookie = erlang:get_cookie(),
            list_to_atom(lists:concat([Cookie, "@", MainNodeInfo#ets_server_node.ip]))
    end.

get_local_main_node_for_auto_link() ->
    StrNodeName = atom_to_list(node()),
    [_PreName, IP] = string:tokens(StrNodeName, "@"),
    Cookie = erlang:get_cookie(),
    list_to_atom(lists:concat([Cookie, "@", IP])).

get_fight_node(NodeId) when NodeId < ?FIGHT_NODE_ID_INDEX ->
    '';
get_fight_node(NodeId) ->
    [RServerNode] = ets:lookup(?ETS_SERVER_NODE, NodeId),
    Cookie = erlang:get_cookie(),
    list_to_atom(lists:concat([Cookie, "_f", NodeId, "@", RServerNode#ets_server_node.ip])).
%% ========================= 依賴約定的節點命名規則

%% 获取战斗节点列表 
get_fight_node_list() ->
    check_node_valid(),
    ets:foldl(fun(E, Acc) ->
        ENodeName = get_fight_node(E#ets_server_node.node_id),
        case is_node_fight(ENodeName) of
            true when E#ets_server_node.is_link == true ->
                [ENodeName | Acc];
            _ ->
                Acc
        end
    end,
    [],
    ?ETS_SERVER_NODE).

check_node_valid() ->
    ets:foldl(fun(E, Acc) ->
        check_node_valid_and_update(E),
        Acc
    end,
    [],
    ?ETS_SERVER_NODE).

check_node_valid_and_update(NodeInfo) ->
    AllNodes = [node() | nodes()],
    NodeName = get_fight_node(NodeInfo#ets_server_node.node_id),
    case lists:member(NodeName, AllNodes) of
        false -> 
            NewNodeInfo = NodeInfo#ets_server_node{is_link = false},
            ets:insert(?ETS_SERVER_NODE, NewNodeInfo),
            {false, NewNodeInfo};
        _ ->
            true
    end.
