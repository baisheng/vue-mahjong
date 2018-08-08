%% @author zouv
%% @doc 初始化处理

-module(game_init).

-include("core.hrl").
-include("common.hrl").
% -include_lib("db_mysql/include/record.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         init_ets_create/0,
         init_ets_base_data/0,
         init_ets_base_data/1,
         init_ets_base_data_handle/2,
         start_mod/0,
         init_other/0,
         link_main_node/1,
         link_main_node_call_to/0
        ]).

%% ====================================================================
%% 其他通用ETS
%% ====================================================================
init_ets_create() ->
    init_ets_common(),
    init_ets_base(),
    ok.

init_ets_common() ->
    init_ets_common_normal(),
    case misc:is_main_node() of
        true ->
            init_ets_common_main();
        _ ->
            skip
    end,
    ok.

%% 主节点
init_ets_common_main() ->
    ets:new(?ETS_ONLINE,                [{keypos, #ets_online.player_id}, named_table, set, public]),
    ets:new(?ETS_PLAYER_PID,            [{keypos, #ets_player_pid.player_id}, named_table, set, public]),
    ok.

%% 通用
init_ets_common_normal() ->
    ets:new(?ETS_SERVER_NODE,           [{keypos, #ets_server_node.node_id}, named_table, set, public]),
    ets:new(?ETS_ROOM_INFO,             [{keypos, #ets_room_info.id}, named_table, set, public]),
    ets:new(?ETS_ROOM_PID,              [{keypos, #ets_room_pid.unique_flag}, named_table, set, public]),
    ok.

%% ====================================================================
%% 配置表ETS
%% ====================================================================
init_ets_base() ->
    ok.

%game_init:init_ets_base_data().
init_ets_base_data() ->
    init_ets_base_data([]).

init_ets_base_data(MainNodeName) ->
    EtsNameList = ets:all(),
    List = 
        lists:filter(fun(EName) ->
            if 
                is_atom(EName) ->
                    string:str(atom_to_list(EName), ?BASE_DB_NAME_PRE) == 1; % 仅更新base表
                true ->
                    false
            end
        end,
        EtsNameList),
    List1 = lists:sort(List),
    case misc:is_main_node() of
        true ->
            init_ets_base_data_main(List1);
        _ ->
            init_ets_base_data_fight(MainNodeName, List1)
    end.

%% 主节点Base数据初始化
%%：从数据库获取数据
init_ets_base_data_main(List) ->
    % io:format("~n~n  init base ets: ~n"),
    lists:foreach(fun(E) ->
        io:format("     ets name: ~p~n", [E]),
        init_ets_base_data1(E)
    end,
    List).

%% 战斗节点Base数据初始化
%%：从主节点或文件中获取数据
init_ets_base_data_fight(MainNodeName, List) ->
    io:format("~n~n  init base ets: ~n"),
    lists:foreach(fun(E) ->
        io:format("    ets name: ~p~n", [E]),
        case rpc:call(MainNodeName, ets, tab2list, [E]) of
            {badrpc, _} ->
                logger:warning_msg("init ets base data fight ERROR!!! ~p~n", [{MainNodeName, E}]);
            EEtsData ->
                init_ets_base_data2(E, EEtsData, false)
        end
    end,
    List).

%% ----- 从数据库读取 -----
init_ets_base_data1(DB) ->
    List = db_agent_base:get_all(DB),
    init_ets_base_data2(DB, List, true).
init_ets_base_data2(DB, List, IsFromDb) ->
    ets:delete_all_objects(DB),
    init_ets_base_data_handle(List, DB, IsFromDb).

%% -----
init_ets_base_data_handle(List, DB) ->
    IsFromDb = true,
    init_ets_base_data_handle(List, DB, IsFromDb).

init_ets_base_data_handle([], _DB, _IsFromDb) -> ok;
%% 其他
init_ets_base_data_handle([E | L], DB, IsFromDb) ->
    Record = ?CONDITIONAL_EXPRESSION(IsFromDb, db_agent_base_util:transfer_from_db(DB, E), E),
    ets:insert(DB, Record),
    init_ets_base_data_handle(L, DB, IsFromDb).

%% ====================================================================
%% 启动控制进程
%% ====================================================================
start_mod() ->
    start_cross_server_mod(),   % 开启跨服进程
    start_local_mod(),          % 开启本地进程
    start_local_server_mod(),   % 开启本服进程
    start_global_mod().         % 开启全局进程

start_cross_server_mod() ->
    CrossModList = [],
    [misc:get_cross_server_mod_pid(Module) || Module <- CrossModList].

start_local_mod() ->
    ModList = 
        [
         mod_kernel,            % 核心管理
         mod_time,              % 时间管理
         mod_increase_id        % 临时自增Id
        ],
    [misc:get_mod_pid(Module) || Module <- ModList].

start_local_server_mod() ->
    ModList =
        [
         mod_monitor,           % 监督进程
         mod_room_info,         % 房间信息管理
         mod_room_manager       % 房间进程管理
        ],
    [misc:get_local_server_mod_pid(Module) || Module <- ModList].

start_global_mod() ->
    GlobalModList = [], % TODO GlobalUniqueFlag = 对应ServerId, 
    [misc:get_global_mod_pid(EModule, EGlobalUniqueFlag) || {EModule, EGlobalUniqueFlag} <- GlobalModList].

%% ====================================================================
%% 初始化数据库
%% ====================================================================

%% ====================================================================
%% 其他
%% ====================================================================
init_other() ->
    %% 初始化日志系统
    LogFilePath = config:get_logfile_path(),
    filelib:ensure_dir(LogFilePath),
    FileName = LogFilePath ++ "/log_node.log", 
    error_logger:logfile({open, FileName}),
    %% 本节点信息
    StrNodeName = atom_to_list(node()),
    [_PreName, IP] = string:tokens(StrNodeName, "@"),
    SelfNodeInfo = #ets_server_node{node_id = config:get_node_id(), ip = IP, weight = 100, is_link = true},
    ets:insert(?ETS_SERVER_NODE, SelfNodeInfo),
    ok.

%% 链接节点
link_main_node(MainNodeName) ->
    case misc:is_main_node() of
        true ->
            throw("link main node Error1!");
        _ ->
            case net_adm:ping(MainNodeName) of
                pong ->
                    case rpc:call(MainNodeName, ?MODULE, link_main_node_call_to, []) of
                        {badrpc, R} ->
                            throw(lists:concat(["link main node call to Error3! ", R]));
                        {[], _ServerId, _CrossServerId} ->
                            false;
                        {MainNodeInfo, ServerId, CrossServerId} ->  % 主节点初始化完才能连接
                            ets:insert(?ETS_SERVER_NODE, MainNodeInfo),
                            config_app:set_server_id(ServerId),
                            config_app:set_cross_server_id(CrossServerId),
                            true
                    end;
                _ ->
                    false
            end
    end.

link_main_node_call_to() ->
    [MainNodeInfo] = ets:lookup(?ETS_SERVER_NODE, ?MAIN_NODE_ID),
    {MainNodeInfo, config:get_server_id(), config:get_cross_server_id()}.
