%% @author zouv
%% @doc 应用配置参数获取接口

-module(config_app).

-include("core.hrl").
-include("common.hrl").

-export([
         %% 动态编译调用接口
         get_server_id/0,
         get_tcp_port/0,
         get_node_id/0,
         get_gm_cmd_on/0,
         get_server_type/0,
         is_server_type_dev/0,
         is_server_type_release/0,
         get_platform_type/0,
         get_platform_list/0,
         get_online_limit/0,
         get_logfile_path/0,
         get_start_time/0,
         get_cross_server_id/0,
         
         %% 修改环境变量
         set_online_limit/1,
         set_start_time/1,
         set_server_id/1,
         set_cross_server_id/1         
        ]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% 获取服务器id
get_server_id() ->
	% case application:get_env(?APP, 'server_id') of
	% 	{ok, ServerId} ->
	% 		ServerId;
	% 	_ ->
	% 		0
	% end.
    1.

%% 获取tcp端口
get_tcp_port() ->
	case application:get_env(?APP, 'tcp_port') of
		{ok, Port} ->
            Port;
		_ ->
			[]
	end.

%% 获取结点编号
get_node_id() ->
	% case application:get_env(?APP, 'node_id') of
	% 	{ok, NodeId} ->
	% 		NodeId;
	% 	_ ->
	% 		?MAIN_NODE_ID
	% end.
    ?MAIN_NODE_ID.

%% GM命令是否开启 
%%：1生效，0失效
get_gm_cmd_on() ->
	case application:get_env(?APP, 'gm_cmd') of
		{ok, Flag} ->
			Flag;
		_ ->
			0
	end.

%% 获取服务器类型
get_server_type() ->
	case application:get_env(?APP, 'server_type') of
		{ok, Type} ->
			Type;
		_ ->
			?SERVER_TYPE_RELEASE
	end.

%% 是否开发服
is_server_type_dev() ->
    ?SERVER_TYPE_DEV == get_server_type().

%% 是否正式服
is_server_type_release() ->
	?SERVER_TYPE_RELEASE == get_server_type().

%% 平台类型
get_platform_type() ->
    case application:get_env(?APP, 'platform_type') of
        {ok, Type} ->
            Type;
        _ ->
            0
    end.

%% 接入的平台列表
get_platform_list() ->
    case application:get_env(?APP, 'platform_list') of
        {ok, Type} ->
            Type;
        _ ->
            []
    end.

%% 在线人数限制
get_online_limit() ->
    case application:get_env(?APP, 'online_limit') of
        {ok, Type} ->
            Type;
        _ ->
            5000
    end.

%% 错误日志目录
get_logfile_path() ->
    case application:get_env(?APP, 'logfile_path') of
        {ok, Path} ->
            Path;
        _ ->
            "../log"
    end.

%% 获取开服时间
get_start_time() ->
    case application:get_env(?APP, 'start_time') of
        {ok, DataTime} ->
            DataTime;
        _ ->
            {{2014,11,7}, {9,0,0}}
    end.

%% 获取跨服节点服务器ID
get_cross_server_id() ->
    % case application:get_env(?APP, 'cross_server_id') of
    %     {ok, CrossServerId} ->
    %         CrossServerId;
    %     _ ->
    %         get_server_id()
    % end.
    ?MAIN_NODE_ID.

%% ========================= 修改环境变量 =========================
%config_app:set_online_limit(600).
set_online_limit(Num) ->
    application:set_env(?APP, 'online_limit', Num),
    dynamic_config:start().

set_start_time(DataTime) ->
    case DataTime of
        {{_, _, _}, {_, _, _}} ->
            application:set_env(?APP, 'start_time', DataTime),
            dynamic_config:start();
        _ ->
            skip
    end.

set_server_id(ServerId) ->
    application:set_env(?APP, 'server_id', ServerId),
    dynamic_config:start().

%% 跨服serverId
set_cross_server_id(CrossServerId) ->
    application:set_env(?APP, 'cross_server_id', CrossServerId),
    dynamic_config:start().
