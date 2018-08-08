
-include("table_record.hrl").

-define(ETS_BEHAVIOUR_DB_GAME, ets_behaviour_db_game).

-record(ets_behaviour_db_game, {
    module = undefined,     
	table_name = undefined,         % 表名
    table_fields = [],              % 字段列表
    table_values = [],              % 默认值列表
    is_auto_increment = false,      % 主键是否自增
    indices = [],                   % 索引
    semantics = undefined,          % set类型
    copies = undefined              % 表存储方式
}).
