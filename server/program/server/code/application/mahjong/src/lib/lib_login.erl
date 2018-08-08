%% @author 
%% @doc 登录相关

-module(lib_login).

-include("common.hrl").
-include_lib("db/include/record.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
    is_validate_login/0,
    login_query_account_info/3
 ]).

is_validate_login() ->
    ok.

login_query_account_info(Account, IconUrl, Name) ->
    case db_agent_player_info:get_by_account(Account) of
        [PlayerInfo] -> 
            _IsNew = false;
        _ -> 
            PlayerInfo = lib_player:create_role(Account),
            _IsNew = true
    end,
    if is_list(IconUrl) ->
        NewIconUrl = IconUrl;
    true ->
        NewIconUrl = ""
    end,
    RLoginData = 
        #r_login_data{
            platform_flag = 0,          % 平台标识
            account = Account,
            version = "0.0.1",          % 客户端版本,
            player_icon = NewIconUrl,
            player_name = Name
        },
    {PlayerInfo#player_info.id, true, RLoginData}.
