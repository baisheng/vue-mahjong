%% @author icolun
%% @ 充值验证
%% 之前考虑单独应用，但觉得没必要,直接玩家进程验证
-module(lib_pay).

-include("common.hrl").
-include_lib("db/include/record.hrl").
-include_lib("protob/include/p04_player_pb.hrl").
-include_lib("protob/include/p05_common_pb.hrl").

-export([pay_handle/1]).

%% 后台充值验证   
pay_handle(Msg) ->
	#cs_player_recharge{
				orderid = OrderId,
				price = Price
	} = Msg,
	PlayerInfo = lib_player_procdict:get_dic_player_info(),
	#player_info{
			account = WechatId,
			id = PlayerId,
			diamond = Diamond
	} = PlayerInfo,
	%% 为扩展性，和充值一个真正检测，分开到指定模块
	case is_validate_pay(OrderId, Price, PlayerId) of
		true ->
			case lib_pay_util:check([WechatId, PlayerId, OrderId, Price]) of
				{true, [NewWeChatId, NewPrice, PayTime]} ->
					PayHistory = #pay_history{},
					PayId = db_auto_increment:get_table_auto_increment(PayHistory),
					NowTime = mod_time:now_seconds(),
					NewPayHistory = #pay_history{
										id = PayId,                     %% 游戏服自增id
					                    is_test = false,                %% 是否测试数据
					                    player_id = PlayerId,           %% 玩家id
					                    wechat_id = NewWeChatId,        %% 玩家微信账号id
					                    order_no = OrderId,             %% 客户端支付成功发送过来的订单id
					                    pay_time = PayTime,             %% 支付时间
					                    confirm_time = NowTime,         %% 验证时间
					                    amount = Price,                 %% 支付数量
					                    status = 1                      %% 验证状态（1.成功 2.失败）
					                    },
					db_agent_pay_history:update(NewPayHistory),
					%% 验证直接用玩家进程，会好一些
					AddDiamond = ?MONEY_TO_DIAMOND(NewPrice),
					NewPlayerInfo = PlayerInfo#player_info{diamond = Diamond + AddDiamond},
					lib_player:save_player_info(NewPlayerInfo),
					lib_player:send_player_info(),
					Result = true,
					Reward = [#pb_reward_info{type = ?REWARD_TYPE_DIAMOND, num = AddDiamond}],
					Reason = "";
				{false, _Reason1} ->
					Result = false,
					Reason = "订单不合法",
					Reward = []
			end;
		{false, Reason} ->
			Result = false,
			Reward = []
	end,
	ReplyMsg = #sc_player_recharge{
						result = Result,
						orderid = OrderId,
						reward = Reward,
						reason = Reason

	},
	io:format("pay_handle~p~n",[ReplyMsg]),
	lib_player:send_msg_to_self(ReplyMsg),
	ok.

%% 检测是否验证过了
is_validate_pay(OrderId, Price, PlayerId) ->
	case db_agent_pay_history:get_by_order_no(OrderId) of
		[OrderInfo|_] when OrderInfo#pay_history.player_id =/= PlayerId ->
			{false, "过期订单，并且不是玩家充值订单"};
		[OrderInfo|_] when OrderInfo#pay_history.amount =/= Price ->
			{false, "过期订单，并且充值金钱不对应"};
		[OrderInfo|_]  ->
			{false, "过期订单，重复验证订单"};
		_ ->
			true
	end.





