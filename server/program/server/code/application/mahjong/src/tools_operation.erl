%% @author zouv
%% @doc 运维接口
%% 警告！此模块接口仅运维调试用

-module(tools_operation).

-include("common.hrl").

-export([
         get_online_count/0,
         get_player_pid_count/0,
         
         %% 热更
         load_module_windows/0,
         load_module_windows/2,
         load_module_rebar/0,
         load_module_rebar/2,

         %% 其他
         list_to_chinese/1,
         export_lack_char/0
        ]).

%tools_operation:get_online_count().
get_online_count() ->
    ets:info(?ETS_ONLINE, size).

%tools_operation:get_player_pid_count().
get_player_pid_count() ->
    ets:info(?ETS_PLAYER_PID, size).

%% =======================================================
%% 加载变更模块
%tools_operation:load_module_windows().
%tools_operation:load_module_windows(c, 100).
load_module_windows() ->
    tools_operation:load_module_windows(u, 5).

load_module_windows(Flag, Minute) ->
    if
        %% 加载
        Flag == u ->
            traversal_bin_path_windows(fun u:u/2, Minute);
        %% 检测
        Flag == c ->
            traversal_bin_path_windows(fun u:c/2, Minute);
        true ->
            skip
    end.

traversal_bin_path_windows(Fun, Minute) ->
    PathList = code:get_path(),
    lists:foreach(fun(EStrPath) ->
                      case lists:sublist(EStrPath, 17) of
                          "../../application" ->
                              Fun(Minute, EStrPath ++ "/");
                          _ ->
                            % case lists:sublist(EStrPath, 10) of
                            %     "../../deps" ->
                            %         Fun(Minute, EStrPath ++ "/");
                            %     _ ->
                            %         skip
                            % end
                            skip
                      end
                  end,
                  PathList).

%% 加载变更模块
load_module_rebar() ->
    tools_operation:load_module_rebar(u, 5).

load_module_rebar(Flag, Minute) ->
    if
        %% 加载
        Flag == u ->
            traversal_bin_path_rebar(fun u:u/2, Minute);
        %% 检测
        Flag == c ->
            traversal_bin_path_rebar(fun u:c/2, Minute);
        true ->
            skip
    end.

traversal_bin_path_rebar(Fun, Minute) ->
    PathList = code:get_path(),
    lists:foreach(fun(EStrPath) ->
                      Fun(Minute, EStrPath ++ "/")
                      %case lists:sublist(EStrPath, 17) of
                      %    "../../application" ->
                      %        Fun(Minute, EStrPath ++ "/");
                      %    _ ->
                      %        skip
                      %end
                  end,
                  PathList).

% %% 清理无效玩家物品
% %tools_operation:clear_db_unable_player_item().
% clear_db_unable_player_item() ->
%     case dal:first_rpc(player_items) of
%         {ok, FirstId} when FirstId /= '$end_of_table' ->
%             clear_db_unable_player_item1(FirstId, 1);
%         _ ->
%             skip
%     end.

% clear_db_unable_player_item1(Id, Count) ->
%     case dal:next_rpc(player_item, Id) of
%         {ok, NextId} when NextId /= '$end_of_table' ->
%             {ok, [Info]} = player_item_db:get(Id),
%             BaseId = Info#player_item.base_id,
%             case base_item_db:get(BaseId) of
%                 {ok, []} ->
%                     NewCount = Count + 1,
%                     db_agent_player_item:delete(Id);
%                 {ok, [_Base]} ->
%                     NewCount = Count
%             end,
%             clear_db_unable_player_item1(NextId, NewCount);
%         _ ->
%             Count
%     end.

%% 转中文
list_to_chinese(List) ->
    io:format("~ts~n", [list_to_binary(List)]).

%% 导出客户端字库缺少字
%tools_operation:export_lack_char().
export_lack_char() ->
    case ets:tab2list(ets_common_lack_char) of
        [{_, List}]  -> ok;
        _ -> List = []
    end,
    {ok, FileDevice} = file:open("export_lack_char.txt", [write]),
    lists:foreach(fun(E) ->
        E1 = unicode:characters_to_binary(E),
        E2 = string:strip(lists:flatten(erlang:binary_to_list(E1))),
        io:fwrite(FileDevice, "~ts ", [E2])
    end,
    List),
    file:close(FileDevice),
    ok.
