-module(mahjong_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = mahjong_sup:start_link(),
    dynamic_config:start(),
    game_init(),
    tools_operation:get_online_count(),
    {ok, Pid}.

stop(_State) ->
    ok.

game_init() ->
    %% 初始化
    game_init:init_ets_create(),
    game_init:init_ets_base_data(),
    game_init:init_other(),
    %% 开启控制进程
    game_init:start_mod(),
    ok.
