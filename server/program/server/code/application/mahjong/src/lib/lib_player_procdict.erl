%% @author zouv
%% @doc 玩家接口 - 进程字典

-module(lib_player_procdict).

-include("common.hrl").
-include("player.hrl").
-include_lib("db/include/record.hrl").

%% 玩家进程字典
-define(DIC_PLAYER_INFO,                    dic_player_info).               % 玩家信息
-define(DIC_GATE_PID,                       dic_gate_pid).                  % 网关Pid
-define(DIC_GATE_PROC_CLOSE_TIME,           dic_gate_proc_close_time).      % 网关关闭

-export([
    get_dic_gate_pid/0,
    update_dic_gate_pid/1,
    get_dic_player_info/0,
    update_dic_player_info/1,
    get_dic_gate_proc_close_time/0,
    update_dic_gate_proc_close_time/1
    % get_dic_login_reconnection_key/0,
    % update_dic_login_reconnection_key/1
 ]).

%% --------------- 网关Pid ---------------
get_dic_gate_pid() ->
    get(?DIC_GATE_PID).

update_dic_gate_pid(GatePid) ->
    put(?DIC_GATE_PID, GatePid).

%% --------------- 玩家数据 ---------------
get_dic_player_info() ->
    get(?DIC_PLAYER_INFO).

update_dic_player_info(PlayerInfo) ->
    NewPlayerInfo = PlayerInfo#player_info{other = ?DIRTY_STATE_UPDATE},
    put(?DIC_PLAYER_INFO, NewPlayerInfo).

%% --------------- 网关关闭时间 ---------------
update_dic_gate_proc_close_time(Time) ->
    put(?DIC_GATE_PROC_CLOSE_TIME, Time).

get_dic_gate_proc_close_time() ->
    case get(?DIC_GATE_PROC_CLOSE_TIME) of
        ?UNDEFINED ->
            [];
        Time ->
            Time
    end.

% get_dic_login_reconnection_key() ->
%     case get(?DIC_LOGIN_RECONNECTION_KEY) of
%         ?UNDEFINED ->
%             logger:warning_msg("get dic login reconnection Error! ~p~n", [lib_player:get_dic_player_info()]),
%             "";
%         Key ->
%             Key
%     end.

% update_dic_login_reconnection_key(Key) ->
%     put(?DIC_LOGIN_RECONNECTION_KEY, Key).

