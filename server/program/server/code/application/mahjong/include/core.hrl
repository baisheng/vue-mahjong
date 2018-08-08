
-ifndef(GAME_CORE_HRL).
-define(GAME_CORE_HRL, true).

-define(APP,                            'mahjong').
-define(APP_SUP,                        'mahjong_sup').
-define(MAIN_NODE_ID,                   1).             % 主节点Id
-define(FIGHT_NODE_ID_INDEX,            10).            % 战斗节点Id基数
-define(DAY_SECOND,                     86400).         % 一天秒数
-define(TIME_ZONE_SECOND,               28800).         % 东八时区秒数
-define(ID_INDEX,                       100000000).     % 玩家为id基数
-define(PLAYER_ID_BASE,                 ?ID_INDEX).
-define(BASE_DB_NAME_PRE, 				"base_").		% Base表命名前缀

-endif.
