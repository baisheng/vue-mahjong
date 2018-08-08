%% zouv
%% 定义临时数据结构

-ifndef(GAME_RECORD_HRL).
-define(GAME_RECORD_HRL, true).

%% 数据表更新状态
-define(DIRTY_STATE_NULL,                           0).
-define(DIRTY_STATE_UPDATE,                         1).
-define(DIRTY_STATE_NEW,                            2).
-define(DIRTY_STATE_DEL,                            3).

%% ------------------------- ETS 记录结构定义 -------------------------
%% 节点信息
-record(ets_server_node, {
    node_id = 0,
    ip = "",
    weight = 0,
    is_link = false
 }).

%% 客户端登陆数据
-record(r_login_data, {
                       platform_flag = 0,       % 平台标识
                       account_old = "",
                       account = "",
                       version = "",            % 客户端版本
                       network_type = "",       % 网络类型
                       sys_type = 0,            % 系统类型  0-Windows 1-Android 2-iOS 3-WindowsPhone
                       mac_address = "",        % 物理地址
                       device_type = "",        % 设备机型
                       had_role = false,
                       player_icon = "",
                       player_name = ""
                      }).

%% 在线玩家(gate_pid有效)
-record(ets_online, {
                     player_id = 0,
                     account = "",
                     uid = "",
                     player_name = "",
                     player_level = 0,
                     player_icon = "",
                     player_pid = undefined,
                     gate_pid = undefined
                    }).

%% 玩家进程pid
-record(ets_player_pid, {
    player_id = 0,
    player_pid = undefined,
    uid = "",
    account = ""
 }).

%% 房间进程信息
-record(ets_room_pid, {
    unique_flag = "",
    room_id = 0,
    pid = undefined
 }).

%% 房间信息
-record(ets_room_info, {
    id = "",            % 房间Id
    type = 0,           % 房间类型
    owner_id = 0,       % 房主Id
    record_id = 0,      % 每一个房间开启的时候都会初始化一个记录id 因为现在设定关服后，房间号可以相同
    sit_member_num = 0, % 坐下的玩家数
    members = [],
    cost_list = [],     % 已经花钱坐下过的人
    create_time = 0,    %% 创建时间
    start_time = 0,     %% 战斗时间，包括之后点开始战斗，如果已经开始
    long_time = 0,      %% 这个是创建时就确定的，每一种类型，
    wait_long_time = 0,
    is_start = false,   % 是否已开始游戏
    is_march = false,    % 这个状态是房主点完开始，一直是true,因为房间战斗是60分钟
    ref = undefined     %% 等待准备定时器 
 }).

%% 房间成员信息
-record(r_room_member, {
    player_id = 0,
    player_name = "",
    player_pid = undefined,
    gate_pid = undefined,
    position = 0,
    player_icon = "",
    score = 0,
    count = 0,
    total_count = 0,
    is_leave = false
 }).

%% 记录成员
-record(r_record_member, {
          player_id = 0,
          player_name = "",
          player_icon = "",
          win_flag = true,
          score = 0
  }).

-endif.
