%% 意见反馈
-module(lib_feedback).

-include("common.hrl").
-include_lib("db/include/record.hrl").
-include_lib("protob/include/p06_feedback_pb.hrl").
% -define(DIC_PLAY_FEEDBACK,  dic_play_feedback).

%% ====================================================================
%% API functions
%% ====================================================================
-export([cs_feedback_fill/1]).

%% 每一条直接写入数据库
save_player_feedback(Info) ->
	db_agent_player_feedback:update(Info).
%% 反馈填写
cs_feedback_fill(Msg) -> 
	case pre_feedback(Msg) of
		{true, Dict} ->
			FeedType = dict:fetch('feed_type', Dict),
			GameType = dict:fetch('game_type', Dict),
			Content = dict:fetch('content', Dict),
			Name = dict:fetch('name', Dict),
			Contact = dict:fetch('contact', Dict),
			PlayerInfo = lib_player_procdict:get_dic_player_info(),
			PlayerId = PlayerInfo #player_info.id,
			%% 生成自增id
			PlayerFeedBack = #player_feedback{},
			FeedId = db_auto_increment:get_table_auto_increment(PlayerFeedBack),
			NewPlayerFeedBack = #player_feedback{
					                        id = FeedId,
					                        player_id = PlayerId,
					                        feed_type = FeedType,                   %% 什么反馈
					                        game_type= GameType,
					                        content = Content,
					                        name = Name, 
					                        contact = Contact                     %% 联系方式
					                     },
			%% 保存数据库
			save_player_feedback(NewPlayerFeedBack),
			Result = true,
			Reason = "";
		{false, Reason} ->
			Result = false
	end,
	ResponseMsg = #sc_feedback_fill{
						result = Result,
						reason = Reason
	},
	%%io:format("cs_feedback_fill~p~n",[ResponseMsg]),
	GatePid = lib_player_procdict:get_dic_gate_pid(),
    lib_gate:send_data(GatePid, ResponseMsg).

pre_feedback(Msg) ->
	CheckList = [
				'feed_type',
				'game_type',
				'content'
				],
	#cs_feedback_fill{
					feedtype = FeedType,        
					gametype = GameType,
					content = Content,
					name = Name,
					contact = Contact
	} = Msg,
	Dict = dict:from_list([
						{'feed_type', FeedType},
						{'game_type', GameType},
						{'content', Content},
						{'name', Name},
						{'contact', Contact}
						]),
	pre_feedback_check(Dict, CheckList).

pre_feedback_check(Dict, []) ->
	{true, Dict};
pre_feedback_check(Dict, ['feed_type'|Z]) ->
	FeedType = dict:fetch('feed_type', Dict),
	case lists:member(FeedType, ?FEEDBACK_LIST) of
		true ->
			NewFeedType = FeedType;
		_ ->
			NewFeedType = ?FEEDBACK_OTHER
	end,
	Dict1 = dict:store('feed_type', NewFeedType, Dict),
	pre_feedback_check(Dict1, Z);
pre_feedback_check(Dict, ['game_type'|Z]) ->
	GameType = dict:fetch('game_type', Dict),
	case lists:member(GameType, ?ROOM_TYPE_LIST) of
		true ->
			pre_feedback_check(Dict, Z);
		_ ->
			{false, "对不起没有对应游戏类型"}
	end;
pre_feedback_check(Dict, ['content'|Z]) ->
	%% 限制内容长度
	pre_feedback_check(Dict, Z);
pre_feedback_check(Dict, [_|Z]) ->
	pre_feedback_check(Dict, Z).
