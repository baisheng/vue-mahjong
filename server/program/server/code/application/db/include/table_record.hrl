%%
%% 定义表记录结构
%% 表命名规范：
%%      基础表 base_
%%      玩家表 player_、common_
%%

-ifndef(TABLE_RECORD_HRL).
-define(TABLE_RECORD_HRL, true).

%% ========================= 配置数据表 =========================
%% 离线PVP排行提升奖励
-record(base_offline_pvp_promote, {
								   rank_node = 0,
								   reward_num = 0
								  }).

%% ========================= 玩家数据表 =========================
%% 自增Id
-record(common_auto_id, {
    flag = undefined, 
    index = 0
}).

%% 帐号
-record(player_account, {
    account = "",
    password = ""
}).

%% 玩家信息
-record(player_info, {
    id = 0,
    account = "",
    uid = 0,
    name = "",
    score = 0,                                 %% 总积分   
    diamond = 0,
    count = 0,                                 %% 所有// 胜利场次
    total_count = 0,                           %% 所有房间     
    icon = "",                                  % 玩家头像
    create_time = 0,                            % 帐号创建时间
    login_time = 0,                             % 登录时间
    logout_time = 0,                            % 登出时间
    last_login_ip = "",                         % 最后一次登录IP
    room_unique_flag = "",
    game_room_id = 0,                           % 正在游戏的房间号（包括离线挂机），用于重新登录返回房间继续游戏
    touch_room_id = 0,                          % 当前触摸的房间号（进入或退出即更改），用于退出房间时清理room_info
    other = []
}).

%% 玩家所开房间
-record(player_room, {
    id = 0,                                     % 房间唯一Id
    player_id = 0,                              % 所属玩家Id
    type = 0,                                   % 房间类型
    create_time = 0,                            % 创建时间
    last_time = 0,                              % 持续时间
    other = []
}).

%% 麻将玩法数据
-record(player_mahjong_fight, {
    player_id = 0,
    score = 0,                                  % 总积分
    count = 0,                                  % 赢的次数
    total_count = 0,                            % 参加的总次数
    recent_list = [],                           % 最近10场的输赢{time, winflag, score}
    open_list = [],                             % 我开的局  {time ,owner_id,room_id} 限制100场
    join_list = [],                             % 我参加的局  {time, owner_id, room_id} 限制100场
    other = []
}).

-record(common_record, {
                    id = 0,                    %% 记录id
                    type = 0,
                    owner_id = 0,              %% 房主id
                    room_id = 0,               %% 房间id
                    total_time = 0,            %% 持续时间的玩法
                    start_time = 0,            %% 房间开启时间
                    end_time = 0,              %% 牌局结束时间
                    info = [],                 %% 玩家信息积分情况
                    other = []


    }).
%% 反馈数据
-record(player_feedback, {
                        id = 0,
                        player_id = 0,
                        feed_type = 0,                   %% 什么反馈
                        game_type= 0,
                        content = "",
                        name = "", 
                        contact = 0,                     %% 联系方式
                        other = []

    }).

%% 保存验证历史，用于玩家反馈对比
-record(pay_history, {
                    id = 0,                     %% 游戏服自增id
                    is_test = 0,                %% 是否测试数据
                    player_id = 0,              %% 玩家id
                    wechat_id = 0,              %% 玩家微信账号id
                    order_no = "",              %% 客户端支付成功发送过来的订单id
                    pay_time = 0,               %% 支付时间
                    confirm_time = 0,           %% 验证时间
                    amount = 0,                 %% 支付数量
                    status = 0                  %% 验证状态（1.成功 2.失败）
}).

-endif.
