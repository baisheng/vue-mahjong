
-ifndef(GAME_COMMON_HRL).
-define(GAME_COMMON_HRL, true).

-include("data.hrl").
-include("record.hrl").

-define(P(S),       io:format(S)).
-define(P(F, A),    io:format(F, A)).
-define(PRINT(List),    io:format("[print] ~p~n", [List])).
-define(STACK_TRACE, try throw("") catch _:_ -> logger:msg("~n Pid:~p, get_stacktrace:~p", [self(), erlang:get_stacktrace()]) end).
-define(MAKE_FUN(Expr), fun() -> Expr end).
-define(RECORD_DEFAULT(R, T), (#R{})#R.T).
-define(CONDITIONAL_EXPRESSION(A, B, C), case A of true -> B; _ -> C end).                  % 条件表达式
-define(RECORD_TYPE(Record), element(1, Record)).
-define(MEDIAN_EXPRESSION(V, Min, Max), if V < Min -> Min; V > Max -> Max; true -> V end).  % 取中间范围值
-define(UNDEFINED,          undefined).
-define(INT32_LIMINT,       2147483647). % 最大值 math:pow(2,31) - 1
-define(UINT32_LIMINT,      4294967295). % 最大值 math:pow(2,32) - 1
-define(BOOL_TO_NUM(Bool),  if Bool == ture -> 1; true -> 0 end).
-define(NUM_TO_BOOL(Num),   if Num > 0 -> true; true -> false end).

% 服务器类型
-define(SERVER_TYPE_DEV,                            0).         % 开发服
-define(SERVER_TYPE_RELEASE,                        1).         % 正式服

%% ----- ETS NAME -----
-define(ETS_SERVER_TIME,                            ets_server_time).                       % 服务器时间
-define(ETS_SERVER_NODE,                            ets_server_node).                       % 节点信息
-define(ETS_ONLINE,                                 ets_online).                            % 在线玩家（客户端在线）
-define(ETS_PLAYER_PID,                             ets_player_pid).                        % 玩家进程pid
-define(ETS_ROOM_PID,                               ets_room_pid).                          % 房间进程pid
-define(ETS_ROOM_INFO,                              ets_room_info).                         % 房间信息
%% ----- ets base -----
%% 注：ets表定义名称需要与数据表名相同
-define(ETS_BASE_SKILL,                             base_skill).                            % 技能配置

%% ----- 公告 -----
%% 系统公告类型
-define(SYS_NOTICE_TIPS,                            1).			% 普通提示
-define(SYS_NOTICE_ALERT,                           2).			% 弹窗提示
-define(SYS_NOTICE_MARQUEE,                         3).         % 跑马灯公告

%% ----- 房间 -----
-define(ROOM_NUM_LIMIT,                             8).         % 房间内人数限制
-define(ROOM_SIT_NUM_LIMIT,                         4).         % 房间内座位数
-define(ROOM_MAIN_POSITION,                         1).         % 庄主位置

%% ----- 前端初始化类型 -----
-define(GAME_INIT_FLAG_MAIN,                        1).         % 大厅
-define(GAME_INIT_FLAG_ROOM,                        2).         % 房间


%% 奖励类型
-define(REWARD_TYPE_POINT,                          1).         % 积分
-define(REWARD_TYPE_DIAMOND,                        2).         % 龙玉

%% 房间类型
-define(ROOM_TYPE_1,         1).            %% 象山麻将
-define(ROOM_TYPE_LIST, [?ROOM_TYPE_1]).

%% 房间时长消费
-define(ROOM_TIME_1,        15 * 60).
-define(ROOM_TIME_2,        30 * 60).
-define(ROOM_TIME_3,        60 * 60).

-define(ROOM_COST_1,        100).
-define(ROOM_COST_2,        150).
-define(ROOM_COST_3,        200).

-define(ROOM_TIME_COST_LIST,  [
								{?ROOM_TIME_1, ?ROOM_COST_1},  %% 15分钟 10龙玉
								{?ROOM_TIME_2, ?ROOM_COST_2},	%% 60分钟 100龙玉
								{?ROOM_TIME_3, ?ROOM_COST_3}	%% 60分钟 100龙玉
								]).
%% 房间等待时间统一是15分钟
-define(ROOM_WAIT_START_TIME,       15 * 60).


%% 意见反馈
-define(FEEDBACK_SUGGESTION,           1).         %% 反馈意见
-define(FEEDBACK_ILLEGAL_REPORT,       2).  %% 违法举报
-define(FEEDBACK_GAME_SUGGESTION,      3).  %% 游戏意见
-define(FEEDBACK_RECHARGE,             4).  %% 充值问题
-define(FEEDBACK_DIAMOND,              5).  %% 扣钻问题
-define(FEEDBACK_DROP_LINE,            6).  %% 掉线问题
-define(FEEDBACK_BUSINESS,             7).%% 商务合作
-define(FEEDBACK_OTHER,                8).
%% 反馈建议列表
-define(FEEDBACK_LIST, [?FEEDBACK_SUGGESTION, ?FEEDBACK_ILLEGAL_REPORT, ?FEEDBACK_GAME_SUGGESTION,
						?FEEDBACK_RECHARGE, ?FEEDBACK_DIAMOND, ?FEEDBACK_DROP_LINE,
						?FEEDBACK_BUSINESS, ?FEEDBACK_OTHER]).
%%充值订单
%% 订单id
-define(RECHARGE_1,      1).
-define(RECHARGE_2,      2).
%% 充钱数量
-define(RECHARGE_MONEY_1,    10).
-define(RECHARGE_MONEY_2,    100).
%% 获得龙玉
-define(RECHARGE_DIAMOND_1,  100).
-define(RECHARGE_DIAMOND_2,  1000).
%% 人民币转换龙玉倍数
-define(MONEY_TO_DIAMOND(Money), 100*Money).

-define(RECHARGE_LIST,   [
						{?RECHARGE_1, ?RECHARGE_MONEY_1, ?RECHARGE_DIAMOND_1},
						{?RECHARGE_2, ?RECHARGE_MONEY_2, ?RECHARGE_DIAMOND_2}
						]).
-endif.
