%% @author zouv
%% @doc socket数据缓存

-module(socket_buffer).

-include_lib("protob/include/p01_login_pb.hrl").

-define(COUNT_BIT,                      32).

%% 进程字典
-define(DIC_CLIENT_PROTO_COUNT,         dic_client_proto_count).        % 客户端协议计数
-define(DIC_SERVER_PROTO_COUNT,         dic_server_proto_count).        % 服务端协议计数
-define(DIC_PROTO_DATA_BUFFER,          dic_proto_data_buffer).         % 协议缓存
-define(DIC_REISSUE_COUNT,              dic_reissue_count).             % 记录协议补发次数

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         decode/1,
         encode/1
        ]).

%% ====================================================================
%% Internal functions
%% ====================================================================
decode(BinData) ->
    % <<Count:?COUNT_BIT, ProtoBinData/binary>> = BinData,
    % ClientProtoCount = get_dic_client_proto_count(),
    % NewClientProtoCount = ClientProtoCount + 1,
    % if
    %     Count == 0 ->
    %         skip;
    %     Count == NewClientProtoCount andalso Count rem ?CLIENT_CLEAN_INTERVAL == 0 ->
    %         send_proto_clean(NewClientProtoCount);
    %     true ->
    %         skip
    % end,
    % if
    %     Count == 0 ->
    %         {ok, Count, ProtoBinData};
    %     Count == NewClientProtoCount ->
    %         update_dic_client_proto_count(NewClientProtoCount),
    %         {ok, Count, ProtoBinData};
    %     true ->
    %         io:format("send_proto_count______1 ~p~n", [{Count, NewClientProtoCount}]),
    %         send_proto_count(ClientProtoCount)
    % end.
    {ok, ProtoId, _IsCrypto, _IsCompress, MsgBin} = message_code:split_bin(BinData),
    {ok, Msg} = message_code:decode_msg_bin(ProtoId, MsgBin),
    case Msg of
        #cs_login_heartbeat{} ->
            skip;
        _ ->
            % ok
             io:format("decode___~p~n",[Msg])
            % catch io:format("recv _______________ ~ts ~p ~w~n", [get(dic_account), element(1, Msg), {erlang:process_info(self(), memory)}])
    end,
    {ok, Msg}.

encode(Msg) ->
    % NextServerProtoCount = get_next_server_proto_count(Msg),
    % if
    %     NextServerProtoCount == 0 ->
    %         skip;
    %     true ->
    %         update_dic_server_proto_count(NextServerProtoCount),
    %         add_proto_data_buffer(NextServerProtoCount, Msg, ProtoBinData)
    % end,
    % % case Msg of
    % %     #sc_login_heartbeat{} ->
    % %         skip;
    % %     _ ->
    % %         case byte_size(ProtoBinData) of
    % %             ByteSize when ByteSize > 0 ->
    % %                 catch io:format("send_______________ ~ts ~p ~p ~p~n", [get(dic_account), NextServerProtoCount, element(1, Msg), {ByteSize, byte_size(zlib:zip(ProtoBinData)), byte_size(zlib:gzip(ProtoBinData))}]);
    % %             _ ->
    % %                 skip
    % %         end
    % % end,
    % <<NextServerProtoCount:?COUNT_BIT, ProtoBinData/binary>>.
    {ok, _IsCrypto, _IsCompress, ProtoBinData} = message_code:encode_msg_bin(Msg),
    {ok, BinData} = message_code:merge_bin(Msg, ProtoBinData),
    case Msg of
        #sc_login_heartbeat{} ->
            skip;
        _ ->
            R = element(1, Msg),
            if
                R == sc_mahjong_fight_left_time ->
                    skip;
                true ->
                    % ok
                      io:format("___~p~n",[Msg])
                    % catch io:format("send_______________ ~ts ~p ~p~n", [get(dic_account), element(1, Msg), {byte_size(ProtoBinData), byte_size(zlib:zip(ProtoBinData)), byte_size(zlib:gzip(ProtoBinData))}])
            end
    end,
    BinData.
