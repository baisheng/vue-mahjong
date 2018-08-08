-module(socket_callback).

-export([
		 get_client_mod/0,
		 on_socket_receive/6, 
		 on_socket_close/5
		]).

get_client_mod()->
	[
	 {?MODULE, on_socket_receive, []},
	 {?MODULE, on_socket_close, []}
	].

%% 接收客户端的数据包
on_socket_receive(GatePid, BinData, ClientSocket, ClientIp, PlayerPid, ProtoKey)->
    try
        case socket_buffer:decode(BinData) of
            {ok, Msg} ->
                message_dispatcher:handle(Msg, GatePid, ClientSocket, PlayerPid);
            _ ->
                skip
        end
    catch
        E:R->
            logger:warning_msg("socket receive error! ~nE = ~p ~nR = ~p ~ninfo = ~p~n", [E, R, {?MODULE, ClientIp, BinData, erlang:get_stacktrace()}]),
            []
    end.

%% 关闭网关
on_socket_close(_Reason, _ClientSocket, _PlayerPid, _PlayerId, _Account) ->
	%logger:msg("~p close gate! info = ~w ~s~n\n", [?MODULE, {Reason, ClientSocket, PlayerId}, Account]),
	ok.
