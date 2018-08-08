%% @author zouv
%% @doc @todo 通用配置

-module(util_config).

-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================
%% 获取platform_flag
get_platform_flag(PlatformId, PlatformType) ->
    PlatformId rem (PlatformType * 10000).

%% 获取platform_id
get_platform_id(PlatformFlag, PlatformType) ->
    PlatformFlag + PlatformType * 10000.


