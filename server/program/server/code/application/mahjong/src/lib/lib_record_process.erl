%% @author icolun
%% @doc 记录管理进程

-module(lib_record_process).
-include("common.hrl").
-include_lib("db/include/record.hrl").
-include_lib("protob/include/p05_common_pb.hrl").
-export([
		init/2,
		get_common_record/1,
		update_common_record/1

		]).

-define(TIME_EFFECTIVE,        24 * 60 * 60 * 7).
-define(TIMER_INTERVAL,                 1000 * 20).

%% 将列表转成record
to_record(List) ->
	F = fun(E) ->
			{PlayerId, PlayerName, PlayerIcon, WinFlag, Score} = E,
			#r_record_member{
			          player_id = PlayerId,
			          player_name = PlayerName,
			          player_icon = PlayerIcon,
			          win_flag = WinFlag,
			          score = Score
			          }
			end,
	lists:map(F, List).
to_tuple(List) ->
	F = fun(E) ->
			#r_record_member{
			          player_id = PlayerId,
			          player_name = PlayerName,
			          player_icon = PlayerIcon,
			          win_flag = WinFlag,
			          score = Score
			          } = E,
			{PlayerId, PlayerName, PlayerIcon, WinFlag, Score}
		end,
	lists:map(F, List).


%% 读
get_common_record(Id) ->
	% NowTIme = mod_time:now_seconds(),
	% %%io:format("___-get_common_record_all~p~n",[db_agent_common_record:get()]),
	case db_agent_common_record:get(Id) of
		[E|_] ->
			E#common_record{info = to_record(E#common_record.info)};
		_ ->
			[]
	end.
	
% 存粗
update_common_record(Info) ->
	NewInfo = Info#common_record{info = to_tuple(Info#common_record.info)},
	%%io:format("update_common_record~p~n",[NewInfo]),
	db_agent_common_record:update(NewInfo),
	ok.

%% 初始化一个新增的房间记录日志
init(PlayerInfo, RRoomInfo) ->
	%% 获取新的id
	CommonRecord = #common_record{},
	RecordId = db_auto_increment:get_table_auto_increment(CommonRecord),
	#player_info{
				id = OwnerId,
				name = Name,
				icon = Icon
	} = PlayerInfo,
	Info = #r_record_member{
						player_id = OwnerId,
						player_name = Name,
						player_icon = Icon
	},
	#ets_room_info{
                id = RoomId, 
                type = Type, 
                create_time = CreateTime, 
                % start_time = NowTime, 
                long_time = LongTime   %% 这个是房间的时长 
                } = RRoomInfo ,
	NewCommonRecord = CommonRecord#common_record{
									id = RecordId,
									type = Type,
									owner_id = OwnerId,
									room_id = RoomId,
									info = [Info],
									total_time = LongTime,
									start_time = CreateTime,
								    end_time = CreateTime + LongTime
	},
	update_common_record(NewCommonRecord),
	RecordId.




