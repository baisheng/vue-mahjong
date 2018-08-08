
-ifndef(MAHJONG_FIGHT_HRL).
-define(MAHJONG_FIGHT_HRL, true).

-include("common.hrl").
%% 麻将分类                 ps :对应分类的id有   :对应每张牌有多少张   总共多少张
-define(MAHJONG_FLOWER,           0).     %% 花牌 1~8     1 8
-define(MAHJONG_CHARACTER,        1).     %% 万子 11~19   4 36
-define(MAHJONG_DOT,              2).     %% 筒子 21~29   4 36
-define(MAHJONG_BAMBOO,           3).     %% 索子 31~ 39  4 36
-define(MAHJONG_WIND,             4).     %% 风牌 41 ~ 44 4 16
-define(MAHJONG_HONOUR,           5).     %% 字牌 51 ~ 53 4 12

%% 每一张普通牌有4张 花牌1张
%% 获取每一张牌的类型
-define(MAHJONG_ID_TO_TYPE(MahjongId),   MahjongId div 10).
%% 每一种类的列表
% -define(MAHJONG_TYPE_LIST, [MAHJONG_SUIT]).
%% 所有牌的id， 这样不大好， 但是是最能确保洗牌安全的 每次洗牌只要4个MAHJONG_ID_LIST 打乱就可以得到初始的待取牌 
-define(FLOWER_ID_LIST, [1, 2, 3, 4, 5, 6, 7, 8]).    %% 花牌
-define(MAHJONG_ID_LIST, [
						  11, 12, 13, 14, 15, 16, 17, 18, 19		%% 万子
						, 21, 22, 23, 24, 25, 26, 27, 28, 29		%% 筒子	
						, 31, 32, 33, 34, 35, 36, 37, 38, 39		%% 索子
						, 41, 42, 43, 44							%% 风牌
						, 51, 52, 53								%% 字牌
						]).
%% 判断是否花牌
-define(IS_FLOWER(MahjongId),      MahjongId > 0 andalso MahjongId < 10).
%% 判断是合法位置
-define(IS_POSITION(Position),     Position >= 1 andalso Position =< 4).
%% 判断是可以吃牌类型 万子 筒子 索子才可以吃
-define(IS_CAN_EAT(MahjongId),     ?MAHJONG_ID_TO_TYPE(MahjongId) >= ?MAHJONG_CHARACTER andalso ?MAHJONG_ID_TO_TYPE(MahjongId) =< ?MAHJONG_BAMBOO).
%% 每次拿牌拿13张，之后庄家发牌算抓牌
-define(MAX_MAHJONG_NUM,       13).
%% 出现花牌后的流局基础数
-define(FLOWER_WILD_NUM(FlowerNum), ?CONDITIONAL_EXPRESSION(FlowerNum < 8, 14, 16)).
%% 每一局麻将 只有4个位置
-define(POSITION_ID_LIST,      [1, 2, 3, 4]).
%% 最多有多少个位置
-define(MAX_POSITION_NUM,       4).
%% 获取下一家位置
-define(GET_NEXT_POSITION(Position),    ?CONDITIONAL_EXPRESSION(Position >= 4, 1, Position + 1)).
%% 获取玩家是我的上家，下家还是对家也就是位置距离，比如我就是0，下家是1，对家是2，上家是3
-define(GET_DISTANCE_POSITION(Position1, Position2),  ?CONDITIONAL_EXPRESSION(Position1 =< Position2, Position2 -Position1 + 1, ?MAX_POSITION_NUM + Position2 - Position1 + 1)).
%% 色子总共有多少面
-define(DIE_FACE_NUM,          6).

-define(STATE_NORMAL,            0).      %% 未进入
-define(STATE_IN,                1).      %% 进入
-define(STATE_PREPARE,           2).      %% 已经准备
-define(STATE_BEING,             3).      %% 游戏正在进行
-define(STATE_LEAVEL,            4).      %% 一离开

-define(FLAG_NORMAL,             0).
-define(FLAG_READY,              1).
-define(FLAG_CANWIN,             2).
-define(FLAG_WINED,              3).
-define(FLAG_FAIL,               4).
%% 麻将玩家结构  玩家不用list 防止混淆 
-record(mahjong_fight_info, {
						player_id = 0,      	%% 玩家id
						type = 1,
						player_name = "",
						player_icon = "",
						player_pid = undefined, %% 玩家pid 直接发消息
						gate_pid = undefined,
						position = 0,       	%% 这个位置是唯一的，就像玩家id一样，好比座位
						circle = 0,             %% 圈位
						order = 0,          	%% 这个是顺序，是每局可变，庄家是1，接着 2， 3， 4
						room_id = 0,

						mahjong_id = 0,     	%% 新抓的牌
						play_mahjong_id = 0,  	%% 新出的牌
						basis = [],    			%% 手上的牌未出的牌 可以打的牌{11,1}
						meld = [],          	%% 吃的牌[{123, MahjongId}，PlayerId， {1,2,3}}， {{111，playerid}{4，4，4}}]吃也不在可以打 一组 
						out = [],      			%% 记下玩家打出的牌
						flower = [],   			%% 玩家有的花牌 用来判断8张胡 等[1,2,3]
						state = 0,          	%% 1 进入，2 准备， 3进行中，4 已经离桌
						
						flag = 0,           	%% 0正常, 1 已听牌 2 已可以胡牌 3已经选择赢牌 4 已经输牌
						canmeld = [],       
						canwin = [],         	%% 可以听的牌 {1, []}

						limit = [],          	%% 放弃吃放弃胡牌后，没过圈不能再碰，比如下家出了11，不碰，对家再出不能再碰，但是对家出不碰，下家出可以，因为过自己算过一圈
						play_limit = [],     	%% 出牌和碰杠有区别，出牌是吃过没过圈不能出，碰是没碰没圈不让碰
						score = 0,            	%% 积分熟练 输 -， 赢 + 
						is_kong = false,      	%%四模的时候，判断杠上花
						end_type = 0,			%% 胡牌时0是无关，1是点炮，2是胡，3被自摸

						re_time = 0,            %% 重连开始时间
						re_ref = undefined
	}).
%% 麻将玩法信息
-record(r_mahjong_fight_state, {

							room_id = 0,
							banker = 0,           	%% 庄家 本局庄家 用于下一盘等新庄家
							circle_banker = 0,      %% 圈位庄家，就是东圈位的位置
							feng_circle = 0,		%% 本轮风圈
							banker_list = [],       %% 这一圈已做过庄的位置
							last_position = 0,     	%% 
							last_mahjongid = 0,		%% 
							now_postion = 0,		
							dice_postion = 0,      	%% 骰子数

							basis_list = [],      	%% 基础未拿取的麻将牌列表
							out_list = [],        	%% 玩家已打的牌
							flower_num = 0,       	%% 已出的花牌 用来判断剩余多少张牌流局
							kong_num = 0,         	%% 杠牌的总数 也是用于判断流局
							% count = 0,            	%% 一共打了几局
							start_time = 0,      	%% 定时开始时间
							wait_start_time = 0,    %% 出牌定时开始
							long_time = 0,          %% 房间时长
							
							is_end = true,       	%% false 进行中   true 结束
							is_first = false,
							is_win = false,      	%% 这一局是胜利结束的，因为胡牌有可能多家胡牌，不好确定下一家的庄家
							is_kong = false,        %% 为抓牌服务的，有些牌是先碰后，但其他玩家可以抢杠胡，放弃之后，玩家是从后抓牌
							
													%% 当玩家出牌后的检测，先从胡牌 放弃后到碰牌，最后到吃牌
							circle_flag = 0,       	%% 0 是无需再检测 1是检测胡牌 2检测检测碰杠 3检测吃牌
							win_position = 0,       %% 如果本轮赢牌，从庄家下来，最近的先赢者 用来发牌
							combination = [],       %% 如果是胡牌，就是手上的牌的组合 加上碰吃杠的组合
							meld = [],              %% 胡的人手上的牌
							type_list = [],         %% 赢的时候胡牌类型{自摸，1台}
							win_id = 0,             %% 胡的是哪一张牌
							count = 0,              %% 总台数

							wait_ref = undefined,   %% 玩家等待出牌等的定时
							start_ref = undefined  %% 玩家开始的定时 比如置色子
	}).
%% 赢牌的组合
-record(r_mahjong_fight_win_combination, {
										pairs = 0,   %% 将眼
										combination = []
	}).
%%
%% 进程字典
-define(DIC_MAHJONG_FIGHT_STATE,     dic_mahjong_fight_state).       	%% 
-define(DIC_MAHJONG_PLAYER_LIST,     dic_mahjong_player_list).  %% 玩家信息
-define(DIC_MAHJONG_WATCH_LIST,      dic_mahjong_watch_list).   %% 观战人员信息
-define(DIC_MAHJONG_WAIT_REF,        dic_mahjong_wait_ref).     %% 下一个出牌定时
-define(DIC_MAHJONG_COMMON_RECORD,   dic_mahjog_common_record). %% 每一局牌内有一个自己正在进行的实时的数据
%% 下一个出牌最大定时时间
-define(MAX_WAIT_TIME,         15).
-define(MAX_START_TIME,        4).   %% 一般的定时用4秒
-define(MAX_DROP_TIME,         180). %% 掉线重连时间间隔
%% 暗杠 明杠 碰 吃
-define(MAHJONG_MELD_TYPE_CONCEALED_KONG,    2222).
-define(MAHJONG_MELD_TYPE_EXPOSED_KONG,      1111).
-define(MAHJONG_MELD_TYPE_PONG,              111).
-define(MAHJONG_MELD_TYPE_EAT,               123).

%% 循环类型
-define(CIRCLE_FLAG_NORMAL,                  0).  %% 正常抓牌，出牌
-define(CIRCLE_FLAG_WIN,                     1).  %% 检测胡牌          
-define(CIRCLE_FLAG_COMMON,                  2).  %% 检测碰杠
-define(CIRCLE_FLAG_EAT,                     3).%% 检测吃
%% 胡牌的类型
-define(MAHJONG_TIAN_HU,                      1).    %% 天胡 13
-define(MAHJONG_DI_HU,                        2).    %% 地胡 13
-define(MAHJONG_EIGHT_FLOWER,                 3).    %%八花  13 
-define(MAHJONG_SAME_SUIT,                    4).    % 清一色 10
-define(MAHJONG_HONOUR_SUIT,                  5).    % 字一色 10
-define(MAHJONG_FOUR_FLOWER,                  6).    % 四花 8
-define(MAHJONG_LARGE_CRANE,                  7).    %  大吊 6
-define(MAHJONG_PONG_HU,                      8).    %% 对对胡 6
-define(MAHJONG_HUN_SUIT,                     9).    %% 混一色 6
-define(MAHJONG_MAIN_WIND,                    10).   %% 正风 2
-define(MAHJONG_MAIN_FLOWER,                  11).   %% 正花 2
-define(MAHJONG_PLAIN_HU,                     12).   % 平湖 1
-define(MAHJONG_EDGE_HU,                      13).   % 边到
-define(MAHJONG_ORDER_HU,                     14).   % 对到
-define(MAHJONG_SET_HU,                       15).  % 嵌套
-define(MAHJONG_ONE_CRANE,                    16).   % 单吊
-define(MAHJONG_MAIN_HU,                      17).   % 自摸
-define(MAHJONG_KONG_HU,                      18).   % 杠
-define(MAHJONG_PONG_HONOUR,                  19).   % 中法白
-define(MAHJONG_CIRCLE_HONOUR,                20).   % 圈风
-define(MAHJONG_MAIN_HAND,                    21).   % 门清
-define(MAHJONG_OTHER_FLOWER,                 22).   % 野花
-define(MAHJONG_HAY,                          23).   %% 海底捞月

%% 胡牌类型
-define(MAHJONG_TYPE_HU_LIST, [
								?MAHJONG_TIAN_HU, ?MAHJONG_DI_HU, ?MAHJONG_EIGHT_FLOWER
								,?MAHJONG_SAME_SUIT, ?MAHJONG_HONOUR_SUIT, ?MAHJONG_FOUR_FLOWER
								,?MAHJONG_LARGE_CRANE, ?MAHJONG_PONG_HU, ?MAHJONG_HUN_SUIT
								,?MAHJONG_MAIN_WIND, ?MAHJONG_MAIN_FLOWER, ?MAHJONG_ONE_CRANE
								,?MAHJONG_HAY
								,?MAHJONG_MAIN_HU,?MAHJONG_KONG_HU
								,?MAHJONG_PONG_HONOUR, ?MAHJONG_CIRCLE_HONOUR, ?MAHJONG_MAIN_HAND
								,?MAHJONG_OTHER_FLOWER]).
%% 互斥 胡牌类型 与平胡相关系
-define(MAHJONG_TYPE_MUTEX_HU_LIST, [?MAHJONG_EDGE_HU, ?MAHJONG_ORDER_HU,
									?MAHJONG_SET_HU, ?MAHJONG_ORDER_HU, ?MAHJONG_PLAIN_HU]).
%% 积分类型
-define(M,     1).    %% 每台基础分

%% 记录有效期
-define(MAHJONG_RECORD_VAILD,       30).
-endif.