-module(lib_chat).
-include("common.hrl").
-include("record.hrl").
-include_lib("db/include/record.hrl").
-include_lib("protob/include/p05_common_pb.hrl").

-define(NOTICE_TIPS,      1).  %% 走马灯
-define(NOTICE_VOICE,     2).	%% 牌局语音提示出牌

-export([
	cs_chat/3,
	send_common_notice/1,
	send_common_notice/2
	]).
%% 暂时没有私聊的，直接是房间里的广播
cs_chat(Type, Content, ToId) ->
	case pre_chat(Type, Content, ToId) of
		{true, Dict} ->
			RRoomInfo = dict:fetch('room_info', Dict),
			PlayerInfo = dict:fetch('player_info', Dict),
			PlayerId = PlayerInfo#player_info.id,
			MemberList = RRoomInfo#ets_room_info.members,
			%% 广播消息
			Msg = #sc_common_chat{
								result = true,
								type = Type,
								content = Content,
								toid = ToId,
								fromid = PlayerId
			},
			lib_room_info:send_room_msg_to_member_list(MemberList, Msg),
			ok;
		{false, Reason} ->
			PlayerInfo = lib_player_procdict:get_dic_player_info(),
			Msg = #sc_common_chat{
								result = false,
								type = Type,
								content = Content,
								toid = ToId,
								fromid = PlayerInfo#player_info.id
			},
			GatePid = lib_player_procdict:get_dic_gate_pid(),
    		lib_gate:send_data(GatePid, Msg)
	end.

pre_chat(Type, Content, ToId) ->
	CheckList = [
				'check_cd',
				'check_content',
				'check_room'
				% 'check_id'
				],
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	Dict = dict:from_list([
							{'player_info', PlayerInfo},
							{'content', Content},
							{'to_id', ToId},
							{'type', Type}
							]),
	pre_chat_check(Dict, CheckList).

pre_chat_check(Dict, []) ->
	{true, Dict};
pre_chat_check(Dict, ['check_cd'|Z]) ->
	pre_chat_check(Dict, Z);
pre_chat_check(Dict, ['check_content'|Z]) ->
	pre_chat_check(Dict, Z);
pre_chat_check(Dict, ['check_room'|Z]) ->
	PlayerInfo = dict:fetch('player_info', Dict),
	RoomId = PlayerInfo#player_info.touch_room_id,
	case ets:lookup(?ETS_ROOM_INFO, RoomId) of
		[RRoomInfo] ->
			Dict1 = dict:store('room_info', RRoomInfo, Dict),
			pre_chat_check(Dict1, Z);
		% [RRoomInfo] ->
		% 	{false, "牌局没开始"};
		_ ->
			{false, "不在房间中，不能聊天"}
	end.

%% 消息发送

%% 房间里的提示
send_common_notice(Content) ->
	send_common_notice(?NOTICE_VOICE, Content).
send_common_notice(Type, Content) ->
	Msg = #sc_common_notice{
						type = Type,
						content = Content
	},
	lib_mahjong_fight_msg:send_to_list(Msg),
	ok.



