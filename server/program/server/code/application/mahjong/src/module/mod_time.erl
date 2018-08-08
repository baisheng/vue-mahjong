
-module(mod_time).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

-define(TIMER_TIME,             100).           % 更新间隔
-define(DIC_QC_TIME,            dic_qc_time).   % QC测试添加时间差
-define(KEY,                    server_time).

-define(TIME_ZONE_SECOND,       28800).         % 东八时区秒数
-define(DAY_SECOND,             86400).         % 一天秒数

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         start_link/1,
         get_mod_pid/0,
         
         info/0,
         cpu_time/0,
         now_time/0,
         now_seconds/0,
         now_long_seconds/0,
         now_long_seconds_precise/0,
         system_time/0,
         
         qc_change_time/1,
         get_today_start_second/0,
         time_string/0,
         time_string/1,
         date_time_string/0
        ]).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

start_link(ModProcessName) ->
    gen_server:start_link(?MODULE, [ModProcessName], []).

get_mod_pid() ->
    misc:get_mod_pid(?MODULE).

%% 进程初始化
init([ModProcessName]) ->
    util_process:register_local(ModProcessName, self()),
	ets:new(?ETS_SERVER_TIME, [set, protected, named_table ,{read_concurrency, true}]),
	ets:insert(?ETS_SERVER_TIME, {?KEY, {erlang:now(), 0}}),
	erlang:send_after(?TIMER_TIME, self(), {event, clock}),
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, [], State}.

%% 修改QC时间
handle_cast({'qc_change_time', ChangeSeconds}, State) ->
    update_dic_qc_time(ChangeSeconds),
    {noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%% 同步当前时间
handle_info({event, clock}, State) ->
    erlang:send_after(?TIMER_TIME, self(), {event, clock}),
    QcSeconds = get_dic_qc_time(),
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    NewSeconds = MegaSecs * 1000000 + Secs + QcSeconds,
    Now = {NewSeconds div 1000000, NewSeconds rem 1000000, MicroSecs},
    {_Total_Run_Time, Time_Since_Last_Call} = statistics(runtime),
	ets:insert(?ETS_SERVER_TIME, {?KEY, {Now, Time_Since_Last_Call}}),
	{noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------- QC测试添加时间差 ---------------
get_dic_qc_time() ->
    case get(?DIC_QC_TIME) of
        undefined ->
            0;
        QcSeconds ->
            QcSeconds
    end.

update_dic_qc_time(Time) ->
    put(?DIC_QC_TIME, Time).

info() ->
    [
     ets:info(?ETS_SERVER_TIME),
     ets:tab2list(?ETS_SERVER_TIME)
    ].

cpu_time() -> 
    [{?KEY, {_, Time_Since_Last_Call}}] = ets:lookup(?ETS_SERVER_TIME, ?KEY),
    Time_Since_Last_Call.

now_time() ->
    [{?KEY, {Now, _}}] = ets:lookup(?ETS_SERVER_TIME, ?KEY),
    Now.

%% 获取当前时间
now_seconds() ->
    [{?KEY, {Now, _}}] = ets:lookup(?ETS_SERVER_TIME, ?KEY),
    {MegaSecs, Secs, _MicroSecs} = Now,
    MegaSecs * 1000000 + Secs.

%% 获取当前时间：毫秒
now_long_seconds() ->
    [{?KEY, {Now, _}}] = ets:lookup(?ETS_SERVER_TIME, ?KEY),
    {MegaSecs, Secs, MicroSecs} = Now,
    ((MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs) div 1000.

%% 获取当前时间：毫秒（更精准）
now_long_seconds_precise() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(), % TODO 加上 时间差
    ((MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs) div 1000.

%% 获取当前时间：微秒
system_time() ->
    [{?KEY, {Now, _}}] = ets:lookup(?ETS_SERVER_TIME, ?KEY),
    {MegaSecs, Secs, MicroSecs} = Now,
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

%% 用于QC测试，添加时间差
qc_change_time(ChangeSeconds) ->
    if
        is_integer(ChangeSeconds) ->
            NewChangeSeconds = ?MEDIAN_EXPRESSION(ChangeSeconds, -100 * 356 * ?DAY_SECOND, 100 * 356 * ?DAY_SECOND);
        true ->
            NewChangeSeconds = 0
    end,
    gen_server:cast(get_mod_pid(), {'qc_change_time', NewChangeSeconds}).

%% 获取今天凌晨秒数
get_today_start_second() ->
    NowSecond = now_seconds(),
    util:get_start_second(NowSecond).

%% 日期格式 YYYY-MM-DD hh:mm:ss
time_string() ->
    NowSecond = now_seconds(),
    time_string(NowSecond).

time_string(NowSeconds) ->
    DataTime = util:seconds_to_datetime(NowSeconds),
    util:format_datetime_to_string(DataTime).

%% 日期格式 YYYY-MM-DD
date_time_string() ->
    NowSecond = now_seconds(),
    {DataTime, _} = util:seconds_to_datetime(NowSecond),
    util:datetime_to_local_string(DataTime).
