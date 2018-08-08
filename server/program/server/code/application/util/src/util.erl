
-module(util).

-compile(export_all).

-define(DAY_SECOND,					86400).			% 一天秒数
-define(TIME_ZONE_SECOND,           28800).         % 东八时区秒数  % TODO 写死时区？
-define(DIFF_SECONDS_0000_1970, 	62167219200).   % Gregorian格林高利时间(0000年) 与 UniversalTime格林尼治时间(1970年) 的时间差

-ifdef(TEST).
-define(RANDOM_SEED, skip).
-else.
-define(RANDOM_SEED, random:seed(erlang:now())).
-endif.

%% ====================================================================================================
%% ！！！以下 获取时间的接口禁止使用
%% ====================================================================================================
%% 系统时间（秒）
now_seconds() ->
	{MegaSecs, Secs, _MicroSecs} = os:timestamp(),  % 调用c底层gettimeofday函数，效率更高，但没有时间纠正
	MegaSecs * 1000000 + Secs.

%% 系统时间（秒），精确时间
now_seconds_precise() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),    % 有时间纠正机制，会对获取加锁
    MegaSecs * 1000000 + Secs.

%% 系统时间（毫秒）
now_long_seconds() ->
	{MegaSecs, Secs, MicroSecs} = erlang:now(),
	((MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs) div 1000.

%% 系统时间（精确到微秒）
system_time() ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

%% 格林高利秒
gregorian_time() ->
    now_seconds() + ?DIFF_SECONDS_0000_1970.

%% 日期格式 YYYY-MM-DD
date_time_string() ->
    LocalTime = calendar:local_time(),
    {DataTime, _} = LocalTime,
    datetime_to_local_string(DataTime).

%% 日期格式 YYYY-MM-DD hh:mm:ss
time_string() ->
    LocalTime = calendar:local_time(),
    datetime_to_local_string(LocalTime).

%% 获取星期
get_week_day() ->
    {Date, _Time} = calendar:local_time(),
    calendar:day_of_the_week(Date).

%% 获取日期
get_month_day() ->
    {{_Year, Month, Day}, _Time} = calendar:local_time(),
    {Month, Day}.
%% ====================================================================================================
%% ！！！以上 获取时间的接口禁止使用
%% ====================================================================================================

%% 获取星期几
get_week_day(Seconds) ->
    {Date, _Time} = seconds_to_datetime(Seconds),
    calendar:day_of_the_week(Date).

%% 获取日期
get_month_day(Seconds) ->
    {{_Year, Month, Day}, _Time} = seconds_to_datetime(Seconds),
    {Month, Day}.

%% 获取秒对应日期
seconds_to_datetime(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + ?DIFF_SECONDS_0000_1970),
    calendar:universal_time_to_local_time(DateTime).

datetime_to_seconds(DateTime) ->
    [UniversalTime] = calendar:local_time_to_universal_time_dst(DateTime),
    calendar:datetime_to_gregorian_seconds(UniversalTime) - ?DIFF_SECONDS_0000_1970.

%% "19:00" -> second
string_hour_to_second(String) ->
	String1 = string:strip(String, both, $"),
	[StrHour, StrMin] = string:tokens(String1, ":"),
	Hour = list_to_integer(StrHour),
	Min = list_to_integer(StrMin),
	Hour * 3600 + Min * 60.

%% "2015-01-06 12:11:00" -> {{2015,1,6},{12,11,0}}
string_datetime_to_datetime(String) ->
    case string:tokens(String, " ") of
        [StrDate, StrTime] ->
            ok;
        [StrDate] ->
            StrTime = "00:00:00"
    end,
    [StrYear, StrMonth, StrDay] = string:tokens(StrDate, "-"),
    [StrHour, StrMinute, StrSecond] = string:tokens(StrTime, ":"),
    Year = list_to_integer(StrYear),
    Month = list_to_integer(StrMonth),
    Day = list_to_integer(StrDay),
    Hour = list_to_integer(StrHour),
    Minute = list_to_integer(StrMinute),
    Second = list_to_integer(StrSecond),
    {{Year, Month, Day}, {Hour, Minute, Second}}.

string_datetime_to_seconds(String) ->
    DateTime = string_datetime_to_datetime(String),
    datetime_to_seconds(DateTime).

%% 是否同一天
%%	28800：八时区间隔，86400：每天秒长
is_same_date(Seconds1, Seconds2) ->
    Day1 = (Seconds1 + ?TIME_ZONE_SECOND) div ?DAY_SECOND,
    Day2 = (Seconds2 + ?TIME_ZONE_SECOND) div ?DAY_SECOND,
    Day1 == Day2.

%% 是否特定点后的24小时内
is_same_date_spe(SpeSeconds, Seconds1, Seconds2) ->
	SpeSeconds1 = SpeSeconds rem ?DAY_SECOND,
    Day1 = (Seconds1 + ?TIME_ZONE_SECOND - SpeSeconds1) div ?DAY_SECOND,
    Day2 = (Seconds2 + ?TIME_ZONE_SECOND - SpeSeconds1) div ?DAY_SECOND,
    Day1 == Day2.

%% 判断是否同一星期
is_same_week(Seconds1, Seconds2) ->
    {{Year1, Month1, Day1}, Time1} = seconds_to_datetime(Seconds1),
    % 星期几
    Week1  = calendar:day_of_the_week(Year1, Month1, Day1),
    % 从午夜到现在的秒数
    Diff1  = calendar:time_to_seconds(Time1),
    Monday = Seconds1 - Diff1 - (Week1 - 1) * ?DAY_SECOND,
    Sunday = Seconds1 + (7 - Week1) * ?DAY_SECOND + (?DAY_SECOND - Diff1),
    if 
		((Seconds2 >= Monday) and (Seconds2 < Sunday)) -> 
			true;
        true -> 
			false
    end.

%% 判断是否同一月
is_same_month(Seconds1, Seconds2) ->
    {{_Year1, Month1, _Day1}, _Time1} = seconds_to_datetime(Seconds1),
    {{_Year2, Month2, _Day2}, _Time2} = seconds_to_datetime(Seconds2),
	Month1 =:= Month2.

%% 获取凌晨秒数
get_start_second(Seconds) ->
    Seconds - (Seconds + ?TIME_ZONE_SECOND) rem ?DAY_SECOND.

%% 获取相差天数
get_diff_days(SpeSeconds, Seconds1, Seconds2) ->
    SpeSeconds1 = SpeSeconds rem ?DAY_SECOND,
    Day1 = (Seconds1 + ?TIME_ZONE_SECOND - SpeSeconds1) div ?DAY_SECOND,
    Day2 = (Seconds2 + ?TIME_ZONE_SECOND - SpeSeconds1) div ?DAY_SECOND,
    Day1 - Day2.

%% 根据特定点获取相差天数
get_diff_days(Seconds1, Seconds2) ->
    DateSeconds = ?DAY_SECOND,
    DifSeconds = abs(Seconds2 - Seconds1),
    DifSeconds div DateSeconds.

%% {{2015,1,6},{12,11,0}} -> "2015-01-06"
datetime_to_local_string({Year, Month, Day}) ->
    SYear = erlang:integer_to_list(Year),
    SMonth = lists:flatten(io_lib:format("~2..0w", [Month])),
    SDay = lists:flatten(io_lib:format("~2..0w", [Day])),
    SYear ++ "-" ++ SMonth ++ "-"++ SDay;
%% {{2015,1,6},{12,11,0}} -> "2015-01-06 12:11:00"
datetime_to_local_string({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    SYear = erlang:integer_to_list(Year),
    SMonth = lists:flatten(io_lib:format("~2..0w", [Month])),
    SDay = lists:flatten(io_lib:format("~2..0w", [Day])),
    SHour = lists:flatten(io_lib:format("~2..0w", [Hour])),
    SMin = lists:flatten(io_lib:format("~2..0w", [Min])),
    SSec = lists:flatten(io_lib:format("~2..0w", [Sec])),
    SYear ++ "-" ++ SMonth ++ "-"++ SDay ++ " "++ SHour ++":"++ SMin ++":"++ SSec.

%% OpenSeconds : 距离开服时间的秒数
get_time_format(OpenSeconds) ->
    MonthSeconds = ?DAY_SECOND * 30,
    YearSeconds = MonthSeconds * 12,
    Year0 = OpenSeconds div YearSeconds,
    Year = 
        case Year0 of
            0 -> "元";
            1 -> "二";
            2 -> "三";
            3 -> "四";
            4 -> "五";
            5 -> "六";
            6 -> "七";
            7 -> "八";
            8 -> "九";
            9 -> "十";
            _ -> "N"
        end,
    Month = (OpenSeconds - Year0 * YearSeconds) div MonthSeconds + 1,
    Day = (OpenSeconds - Year0 * YearSeconds - (Month - 1) * MonthSeconds) div ?DAY_SECOND + 1,
    OpenDate = io_lib:format("九剑~ts年   ~p月~p日", [Year, Month, Day]),
    OpenDate.

init_rand_seed() ->
    ?RANDOM_SEED.

uniform() ->
    rand:uniform().

%% 生成随机数
rand(Min, Max) when Min == Max ->
	Min;
rand(Min, Max) when Min >= Max ->
	rand(Max, Min);
rand(Min, Max) ->
	Min1 = Min - 1,
	rand:uniform(Max - Min1) + Min1.

%% 以正态分布随机算法，对权值随机
%%  注：单次调用取的值符合正态分布随机，多次调用取值将退化为统计学随机
gauss_rand(Wp, Num) ->
    WTotal = lists:sum(Wp),
    Wtp = [E / WTotal || E <- Wp],
    P = [normal_variate(1/E, 1 / E / 3) || E <- Wtp],
    {IndexList, _NewP} = gauss_rand_for_weight(Num, P, Wtp, []),
    IndexList.

gauss_rand_for_weight(Num, P, _Wtp, V) when Num =< 0 ->
    {V, P};
gauss_rand_for_weight(Num, P, Wtp, V) ->
    {MinIndex, MinValue} = gauss_rand_for_weight_find_min(P, 0, 0, 0),
    NewV = [MinIndex | V],
    P1 = 
        lists:map(fun(E) ->
                      E - MinValue
                  end, P),
    {_, NewP} = 
        lists:foldl(fun(E, {AccIndex, AccP}) ->
                        NewAccIndex = AccIndex + 1,
                        if
                            NewAccIndex == MinIndex ->
                                EOriginal = lists:nth(NewAccIndex, Wtp),
                                NewAccP = AccP ++ [normal_variate(1/EOriginal, 1 / EOriginal / 3)];
                            true ->
                                NewAccP = AccP ++ [E]
                        end,
                        {NewAccIndex, NewAccP}
                    end, 
                    {0, []}, 
                    P1),
    gauss_rand_for_weight(Num - 1, NewP, Wtp, NewV).

gauss_rand_for_weight_find_min([], _Index, MinIndex, Min) ->
    {MinIndex, Min};
gauss_rand_for_weight_find_min([E | List], Index, MinIndex, Min) ->
    NewIndex = Index + 1,
    if
        Min == 0 ->
            NewMinIndex = NewIndex,
            NewMin = E;
        true ->
            if
                E < Min ->
                    NewMinIndex = NewIndex,
                    NewMin = E;
                true ->
                    NewMinIndex = MinIndex,
                    NewMin = Min
            end
    end,
    gauss_rand_for_weight_find_min(List, NewIndex, NewMinIndex, NewMin).

%% 取正态分布变量
normal_variate(E, V) ->
    case get('dic_normal_variate') of
        undefined ->
            ShareTuple = {0, 0, 0};
        ShareTuple ->
            skip
    end,
    {Value, NewShareTuple} = normal_variate1(E, V, ShareTuple),
    put('dic_normal_variate', NewShareTuple),
    Value.

normal_variate1(E, V, {Z1, Z2, Generate}) ->
    DoublePi = 2.0 * 3.14159265358979323846,
    NewGenerate = 1 - Generate,
    if
        NewGenerate == 0 ->
            NewZ1 = Z1,
            NewZ2 = Z2,
            Value = NewZ2 * V + E;
        true ->
            U1 = rand:uniform(),
            U2 = rand:uniform(),
            NewZ1 = math:sqrt(-2 * math:log(U1)) * math:cos(DoublePi * U2),
            NewZ2 = math:sqrt(-2 * math:log(U1)) * math:sin(DoublePi * U2),
            Value = NewZ1 * V + E
    end,
    {Value, {NewZ1, NewZ2, NewGenerate}}.

%% 从左切分第一个匹配的字符串
lsplit(Str, List) ->
    Index = string:str(List, Str),
    EndIndex = Index + length(Str) - 1,
    if
        Index > 0 ->
            {Head, Tail} = lists:split(EndIndex, List),
            {string:substr(Head, 1, length(Head) - length(Str)), Tail};
        true ->
            {[], List}
    end.

%% 从右切分第一个匹配的字符串
rsplit(Str, List) ->
    N = string:str(List, Str),
    lists:split(N - length(Str) + 1, List).

%% bitstring转换为term <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> []; 
bitstring_to_term(<<"">>) -> [];
bitstring_to_term(BitString) ->
	if 
		is_binary(BitString)->
			string_to_term(binary_to_list(BitString));
		true->
			BitString
	end.

string_to_term("") -> [];
string_to_term(String)->
	case erl_scan:string(String++".") of
		{ok, Tokens, _}->
			case erl_parse:parse_term(Tokens) of
				{ok, Term} ->
					Term;
				_Reason ->
					undefined				
			end;
		_Reason ->
			undefined
	end.

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
    term_to_wstring(Term).

term_to_wstring(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

term_to_ptring(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~p", [Term])).

now_to_ms({A,B,C})->
	A*1000000000+B*1000 + C div 1000.

ms_to_now(MsTime)->
	C = (MsTime rem 1000)*1000,
	STime = MsTime div 1000,
	B = STime rem 1000000,
	A = STime div 1000000,
	{A,B,C}.

change_now_time({A,B,C},MsTime)->
	NowMs = now_to_ms({A,B,C}),
	RealMs = NowMs + MsTime,
	ms_to_now(RealMs).

broadcast(Members, Msg) ->
	lists:foreach(fun(H) -> H ! Msg end, Members).
  
even_div(Number,Divisor)->
	FloatNum = Number/Divisor,
	if
		 FloatNum - erlang:trunc(FloatNum)>0 ->
		 	erlang:trunc(FloatNum)+1;
		 true->	
		 	erlang:trunc(FloatNum)
	end.

format_utc_timestamp() ->
	TS = {_,_,Micro} = os:timestamp(),
	{{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_local_time(TS),
	Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}),
	io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w.~6..0w", [Day,Mstr,Year,Hour,Minute,Second,Micro]).

is_process_alive(Pid) 
  when is_pid(Pid) ->
	rpc:call(node(Pid), erlang, is_process_alive, [Pid]).

is_process_alive(undefined, _ProcName) ->
	false;
is_process_alive(_Node, undefined) ->
	false;
is_process_alive(Node, Pid)when is_pid(Pid) ->
	case rpc:call(Node, erlang, is_process_alive, [Pid]) of
		undefined ->
			false;
		_Pid ->
			true
	end;     
is_process_alive(Node, ProcName) ->
	case rpc:call(Node, erlang, whereis, [ProcName]) of
		undefined ->
			false;
		_Pid ->
			true
	end.      

make_int_str(Int)->
	integer_to_list(Int).

make_int_str2(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("0", Str);
		_-> Str
	end.

make_int_str3(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("00", Str);
		2-> string:concat("0", Str);
		_-> Str
	end.

make_int_str4(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("000", Str);
		2-> string:concat("00", Str);
		3-> string:concat("0", Str);
		_-> Str
	end.

make_int_str5(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("0000", Str);
		2-> string:concat("000", Str);
		3-> string:concat("00", Str);
		4-> string:concat("0", Str);
		_-> Str
	end.

make_int_str6(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("00000", Str);
		2-> string:concat("0000", Str);
		3-> string:concat("000", Str);
		4-> string:concat("00", Str);
		5-> string:concat("0", Str);
		_-> Str
	end.

make_int_str7(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("000000", Str);
		2-> string:concat("00000", Str);
		3-> string:concat("0000", Str);
		4-> string:concat("000", Str);
		5-> string:concat("00", Str);
		6-> string:concat("0", Str);
		_-> Str
	end.	

make_int_str8(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("0000000", Str);
		2-> string:concat("000000", Str);
		3-> string:concat("00000", Str);
		4-> string:concat("0000", Str);
		5-> string:concat("000", Str);
		6-> string:concat("00", Str);
		7-> string:concat("0", Str);
		_-> Str
	end.
make_int_str20(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("0000000000000000000", Str);
		2-> string:concat("000000000000000000", Str);
		3-> string:concat("00000000000000000", Str);
		4-> string:concat("0000000000000000", Str);
		5-> string:concat("000000000000000", Str);
		6-> string:concat("00000000000000", Str);
		7-> string:concat("0000000000000", Str);
		8-> string:concat("000000000000", Str);
		9-> string:concat("00000000000", Str);
		10-> string:concat("0000000000", Str);
		11-> string:concat("000000000", Str);
		12-> string:concat("00000000", Str);
		13-> string:concat("0000000", Str);
		14-> string:concat("000000", Str);
		15-> string:concat("00000", Str);
		16-> string:concat("0000", Str);
		17-> string:concat("000", Str);
		18-> string:concat("00", Str);
		19-> string:concat("0", Str);
		_-> Str
	end.
make_int_str30(Int)->
	Str = integer_to_list(Int),
	case string:len(Str) of
		1-> string:concat("00000000000000000000000000000", Str);
		2-> string:concat("0000000000000000000000000000", Str);
		3-> string:concat("000000000000000000000000000", Str);
		4-> string:concat("00000000000000000000000000", Str);
		5-> string:concat("0000000000000000000000000", Str);
		6-> string:concat("000000000000000000000000", Str);
		7-> string:concat("00000000000000000000000", Str);
		8-> string:concat("0000000000000000000000", Str);
		9-> string:concat("000000000000000000000", Str);
		10-> string:concat("00000000000000000000", Str);
		11-> string:concat("0000000000000000000", Str);
		12-> string:concat("000000000000000000", Str);
		13-> string:concat("00000000000000000", Str);
		14-> string:concat("0000000000000000", Str);
		15-> string:concat("000000000000000", Str);
		16-> string:concat("00000000000000", Str);
		17-> string:concat("0000000000000", Str);
		18-> string:concat("000000000000", Str);
		19-> string:concat("00000000000", Str);
		20-> string:concat("0000000000", Str);
		21-> string:concat("000000000", Str);
		22-> string:concat("00000000", Str);
		23-> string:concat("0000000", Str);
		24-> string:concat("000000", Str);
		25-> string:concat("00000", Str);
		26-> string:concat("0000", Str);
		27-> string:concat("000", Str);
		28-> string:concat("00", Str);
		29-> string:concat("0", Str);
		_-> Str
	end.
get_sql_res(Result,Field)->
	case lists:keyfind(Field,1,Result) of
		false-> [];
		{_,Value}->[{Field,Value}]
	end.

cat_atom(Atom1,Atom2)->
	Str1 = case erlang:is_atom(Atom1) of
		       true->atom_to_list(Atom1);
		       _-> Atom1
	       end,
	Str2 = case erlang:is_atom(Atom2) of
		       true->atom_to_list(Atom2);
		       _-> Atom2
	       end,
	list_to_atom(string:concat(Str1,Str2)).

cat_atom(AtomList)->
	F = fun(X)->
			    case erlang:is_atom(X) of
				    true->atom_to_list(X);
				    _-> X
			    end
	    end,
	list_to_atom(lists:concat(lists:map(F, AtomList))).

make_field_list(Fields)->
	string:join(lists:map(fun(X)-> atom_to_list(X) end,Fields),",").

safe_binary_to_list(Bin) when is_binary(Bin)->
	binary_to_list(Bin);
safe_binary_to_list(Bin)->
	Bin.

safe_list_to_binary(List) when is_list(List)->
	list_to_binary(List);
safe_list_to_binary(List)->
	List.
get_script_value(Bin)->
	Str = binary_to_list(Bin), 
	{ok,Ts,_} = erl_scan:string(Str), 
	Ts1 = case lists:reverse(Ts) of 
		      [{dot,_}|_] -> Ts; 
		      TsR -> lists:reverse([{dot,1} | TsR]) 
	      end, 
	{ok,Expr} = erl_parse:parse_exprs(Ts1), 
	{value,V,_} = erl_eval:exprs(Expr, []),
	V.

call_script(Bin)->
	Str = binary_to_list(Bin), 
	{ok,Ts,_} = erl_scan:string(Str), 
	Ts1 = case lists:reverse(Ts) of 
		      [{dot,_}|_] -> Ts; 
		      TsR -> lists:reverse([{dot,1} | TsR]) 
	      end, 
	{ok,Expr} = erl_parse:parse_exprs(Ts1), 
	erl_eval:exprs(Expr, []), 
	ok.	

which_class(ClassId) ->
	if
		(ClassId >= 500000000) and (ClassId < 600000000) ->
			skill;
		true ->
			undefined
	end.

get_distance(PosMy,PosEnemy)->
	{Myx,Myy} = PosMy,
	{Enemyx,Enemyy} = PosEnemy,
	erlang:max(erlang:abs(Myx - Enemyx),erlang:abs(Myy - Enemyy)).

get_flytime(SkillSpeed,PosMy,PosEnemy)->
	{Myx,Myy} = PosMy,
	{Enemyx,Enemyy} = PosEnemy,
	if
		SkillSpeed =< 0->
			0;
		Myx=:=Enemyx->
			erlang:trunc(erlang:abs(Myy - Enemyy)*20/SkillSpeed*42);
		Myy=:=Enemyy->
			erlang:trunc(erlang:abs(Myx - Enemyx)*40/SkillSpeed*42);
		true->
			if
				erlang:abs(Myx - Enemyx) >= erlang:abs(Myy - Enemyy)/2->
					erlang:trunc(erlang:abs(Myx - Enemyx)*40/SkillSpeed*42);
				true->
					erlang:trunc(erlang:abs(Myy - Enemyy)*20/SkillSpeed*42)
			end
	end.

is_in_range(PosMy,PosOther,Range) ->
	{Myx,Myy} = PosMy,
	{Enemyx,Enemyy} = PosOther,
	((erlang:abs(Myx - Enemyx) =< Range) and (erlang:abs(Myy - Enemyy) =< Range)).

get_argument(Input) when is_atom(Input)->
	case init:get_argument(Input) of
		error-> [];
		{ok, [ArgString]}-> lists:map(fun(E)-> list_to_atom(E) end, ArgString)
	end;
get_argument(Input) when is_list(Input)->
	case init:get_argument(list_to_atom(Input)) of
		error-> [];
		{ok, [ArgString]}-> lists:map(fun(E)-> list_to_atom(E) end, ArgString)
	end;
get_argument(_Input)->
	[].

json_encode({struct,_MemberList}=Term)->
	try
		Json = json:encode(Term),
		{ok,list_to_binary(Json)}
	catch
		E:R-> 
			logger:warning_msg("json_encode exception ~p:~p",[E,R]),
			{error,"Excption!"}
	end;
json_encode({scribe_valuetolist,_MemberList}=Term)->
	try
		Json = json:encode(Term),
		{ok,list_to_binary(Json)}
	catch
		E:R-> 
			logger:warning_msg("json_encode scribe_valuetolist exception ~p:~p",[E,R]),
			{error,"Excption!"}
	end;
json_encode({scribe_static,_MemberList}=Term)->
	try
		Json = json:encode(Term),
		{ok,list_to_binary(Json)}
	catch
		E:R-> 
			logger:warning_msg("json_encode scribe_static exception ~p:~p ~p",[E,R,Term]),
			{error,"Excption!"}
	end;
json_encode(S) when is_binary(S)->
	try
		Json = json:encode(S),
		{ok,list_to_binary(Json)}
	catch
		E:R-> 
			logger:warning_msg("s_encode exception ~p:~p",[E,R]),
			{error,"Excption!"}
	end;
json_encode(_)->
	{error,"not support!"}.

json_decode(Json) when is_list(Json)->
	try
		Term = json:decode(Json),
		{ok,Term}
	catch
		E:R-> logger:warning_msg("json_decode exception ~p:~p",[E,R])
	end;
json_decode(Json) when is_binary(Json)->
	try
		Term = json:decode(binary_to_list(Json)),
		{ok,Term}
	catch
		E:R-> logger:warning_msg("json_decode exception ~p:~p",[E,R])
	end;
json_decode(_)->
	{error}.

get_json_member(JsonObj,Member) when is_list(Member)->
	get_json_member_pure(JsonObj,list_to_binary(Member));

get_json_member(JsonObj,Member)when is_binary(Member)->
	get_json_member_pure(JsonObj,Member);

get_json_member(_JsonObj,_Member)->
	{error,"bad arguments"}.

get_json_member_pure(JsonObj,Member)->
	case JsonObj of
		{struct,MemberList}-> 
			case lists:keyfind(Member, 1, MemberList) of
				false-> {error,"cannot find"};
				{_,Value}-> 
					if is_binary(Value)->
						   {ok,binary_to_list(Value)};
					   true->
						   {ok,Value}
					end
			end;
		_-> {error,"bad json"}
	end.

string_match(String,MatchList)->
	M = fun(Match,Acc)->
			case Acc of
				true-> true;
				false->
					case Match of
						"*"-> true;
						_->
							case string:right(Match,1) of
								"*"-> MatchStr = string:left(Match, erlang:length(Match)-1),
									  FindIndx = string:str(String, MatchStr),
									  if
										  FindIndx == 0->false;
										  true-> true
									  end;
								_-> Match =:= String
							end
					end
			end
		end,
	lists:foldl(M, false, MatchList).

term_to_record(Term, RecordName) ->
	list_to_tuple([RecordName | tuple_to_list(Term)]).

term_to_record_for_list([], _TableName) ->
	[];
term_to_record_for_list(Term, TableName) when is_list(Term) ->
	[list_to_tuple([TableName | tuple_to_list(Tup)]) ||Tup <- Term].

%% file_read(FunTerm,FunError,FunProc,FunEof,FileName,ProcNum)->
%% 	case file:open(FileName, read) of
%% 		{ok,Fd}->
%% 			do_file_read(Fd,0,ProcNum,FunTerm,FunError,FunProc,FunEof) ;
%% 		{error,Reason}->
%% 			FunError(Reason)
%% 	end.
%% 
%% do_file_read(Fd,Num,ProcNum,FunTerm,FunError,FunProc,FunEof)->
%% 	case io:read(Fd,'') of
%% 		{ok,Term}->
%% 			case ((Num+1) rem ProcNum) of
%% 				0-> FunProc(Num+1);
%% 				_-> nothing
%% 			end,
%% 			FunTerm(Term),
%% 			do_file_read(Fd,Num + 1,ProcNum,FunTerm,FunError,FunProc,FunEof);
%% 		eof ->
%% 			FunEof(); 
%% 		Error-> 
%% 			FunError(Error)
%% 	end.

get_qualty_color(Quality)->
	case Quality of
		0->["#ffffff"];
		1->["#00FF00"];
		2->["#3399ff"];
		3->["#ff00ff"];
		_->["#CD7F32"]
	end.

get_random_list_from_list(List,Count)->
	% RandomFun = fun(dummy,{TempList, OriList})->
	% 					Len = erlang:length(OriList),
	% 					Random = random:uniform(Len),
	% 					Tuple = lists:nth(Random, OriList),
	% 					{TempList ++ [Tuple], lists:delete(Tuple, OriList)}
	% 			end,
	% {Back, _} = lists:foldl(RandomFun, {[], List}, lists:duplicate(Count, dummy)),
	% Back.
    get_random_list_from_list_1(List, Count, []).

get_random_list_from_list_1([], _, Acc) -> Acc;
get_random_list_from_list_1(_, 0, Acc) -> Acc;
get_random_list_from_list_1(List, Num, Acc) ->
    RandNum = rand:uniform(length(List)),
    TakeTuple = lists:nth(RandNum, List),
    get_random_list_from_list_1(lists:delete(TakeTuple, List), Num-1, [TakeTuple | Acc]).

% TODO 
get_server_start_time()->
	PlatForm = env:get(platform,[]),
	if 
		PlatForm =:=[]->
			logger:warning_msg("platform not find in option~n"),
			[];
		true->
			BaseServerId = env:get(baseserverid,0),
			ServerId = env:get(serverid,0),
			ServerNum = ServerId-BaseServerId,
			StartTimeList = env:get2(server_start_time,PlatForm,[]),
			case lists:keyfind(ServerNum, 1, StartTimeList) of
				false->
					logger:warning_msg("not find server start time ~n"),
					[];
				{_,ServerStartTime}->
					ServerStartTime
			end
	end.

sprintf(Format,Data)->
	TempList = io_lib:format(Format,Data),
	lists:flatten(TempList).

format_datetime_to_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  Date = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day])),
  Time = lists:flatten(io_lib:format("~2..0w:~2..0w:~2..0w", [Hour, Minute,Second])),
  Date ++ " " ++ Time.

%% --------------------------------------------------------------------
%% Func: replace_all/3
%% Purpose: Subject,RE,Replacement
%% Returns: List
%% --------------------------------------------------------------------
replace_all(Subject, RE, Replacement) ->
    replace_all(Subject, RE, Replacement, []). 

%% --------------------------------------------------------------------
%% Func: replace_all/4
%% Purpose: Subject,RE,Replacement,Options
%% Returns: List
%% --------------------------------------------------------------------
replace_all(Subject0, RE, Replacement, Options) ->
    Subject = 
        case Subject0 of
            undefined -> [];
            _ -> Subject0
        end,
    ReSubject = re:replace(Subject, RE, Replacement, Options),
    case ReSubject =:= Subject of
        false ->
            replace_all(ReSubject, RE, Replacement, Options);
        _ ->
            ReSubject
    end.

to_list(Msg) when is_list(Msg) -> 
    Msg;
to_list(Msg) when is_atom(Msg) -> 
    atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) -> 
    binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) -> 
    integer_to_list(Msg);
to_list(Msg) when is_float(Msg) -> 
    f2s(Msg);
to_list(Msg) when is_tuple(Msg) ->
    tuple_to_list(Msg);
to_list(_) ->
    throw(other_value).

f2s(N) when is_integer(N) ->
    integer_to_list(N) ++ ".00";
f2s(F) when is_float(F) ->
    [A] = io_lib:format("~.2f", [F]),
    A.

%% 生成md5码
md5(S) ->
    Md5Bin = erlang:md5(S),
    Md5List = binary_to_list(Md5Bin), 
    lists:flatten(list_to_hex(Md5List)).

make_password(Len) ->
    Bin = crypto:rand_bytes(Len),
    List = binary_to_list(Bin), 
    StrList = lists:flatten(list_to_hex(List)),
    {_, StrList1} = lists:foldl(fun(E, {AccNum, AccStr}) -> 
                               if 
                                   AccNum rem 2 == 0 -> 
                                       {AccNum + 1, [E | AccStr]}; 
                                   true -> 
                                       {AccNum + 1, AccStr} 
                               end 
                           end, 
                           {0, []}, 
                           StrList),
    lists:reverse(StrList1).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

%% 十进制转十六进制
int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).

%% 把List随机乱序   by sen
random_order(List) ->
	random_order1(List,[]).

random_order1([], NewList) ->
	NewList;

random_order1(List,NewList) -> 
	Index = random(length(List)),
	E = lists:nth(Index,List),
	NewList1 = [E | NewList],
	List2 = List -- [E],
	random_order1(List2, NewList1).

% 随机整型
random(N) when is_integer(N) -> 
	rand:uniform(N);
% 随机列表
random(List) when is_list(List) ->
    Length = length(List),
    Rand = rand:uniform(Length),
    lists:nth(Rand, List);
random(_) -> ok.


%% 返回列表中包含某个元素的个数
element_num(Elem, List) ->
	Fun = fun(E, Acc) ->
				if
					E == Elem ->
						Acc + 1;
					true ->
						Acc
				end
		  end,
	lists:foldl(Fun, 0, List).

%% 格式化倒计时，n天n小时n分钟n秒
format_second_time(SecondTime) ->
    Days = SecondTime div ?DAY_SECOND,
    StrDay = 
        if
            Days > 0 ->
                io_lib:format("~p天", [Days]);
            true ->
                ""
        end,
    Hours = SecondTime rem ?DAY_SECOND div 3600,
    StrHour = 
        if
            Hours > 0 ->
                io_lib:format("~p小时", [Hours]);
            true ->
                ""
        end,
    Minutes = SecondTime rem 3600 div 60,
    StrMinster = 
        if
            Minutes > 0 ->
                io_lib:format("~p分钟", [Minutes]);
            true ->
                ""
        end,
    Seconds = SecondTime rem 60,
    StrSecond = 
        if
            Seconds > 0 ->
                io_lib:format("~p秒", [Seconds]);
            true ->
                ""
        end,
    lists:append([StrDay, StrHour, StrMinster, StrSecond]).

for(I, N, F) when I >= N -> [F()];
for(I, N, F) -> [F() | for(I + 1, N, F)].

%% while(Fun, V) ->
%%     NewV = Fun(V),
%%     if
%%         NewV == true ->
%%             while(Fun, NewV);
%%         true ->
%%             NewV
%%     end.

%% 根据某个字符串分割大字符串
%% 如 "123 // 456 // 78 / 10" 根据 "//" 分解为 ["123 ", " 456", " 78 / 10"]
string_sep(String, Sep) ->
	string_sep(String, Sep, []).

string_sep([], _, AccListOfList) ->
	lists:reverse(AccListOfList);
string_sep(String, S, AccListOfList) ->
	Interval = (length(S) - 1),
	case string:str(String, S) of
		0 ->
			lists:reverse([String | AccListOfList]);
		Index ->
			GetList = lists:sublist(String, Index-Interval),
			NewAcc = [GetList | AccListOfList],
			LeftString = lists:nthtail(Index+Interval, String),
			string_sep(LeftString, S, NewAcc)
	end.

%% 把erlang的list转成字符串
%% 如：[1,2,3,4,5] -> "1,2,3,4,5"

erlang_list_to_string(ErlangList) ->
    [_ | DeleteRight] = term_to_string(ErlangList),
    [_ | Reverse] = lists:reverse(DeleteRight),
    lists:reverse(Reverse).

%% 寻找元素是列表的第几个元素
get_nth(Element, List) ->
    get_nth(Element, List, 0).

get_nth(Element, [First | _Left], Num) when Element == First -> Num+1;
get_nth(Element, [_First | Left], Num) -> get_nth(Element, Left, Num+1);
get_nth(_Element, [], _Num) -> false.

%% 添加唯一元素
add_element_unique(List, Elem) when is_list(List) ->
    case lists:member(Elem, List) of
        true ->
            List;
        _ ->
            [Elem | List]
    end;
add_element_unique(Tuple, Elem) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    NewList = add_element_unique(List, Elem),
    list_to_tuple(NewList).

%% 列表去重
deduplication(List) ->
    NewList = deduplication1(List, []),
    lists:reverse(NewList).

deduplication1([], ToList) ->
    ToList;
deduplication1([E | L], ToList) ->
    case lists:member(E, ToList) of
        true ->
            deduplication1(L, ToList);
        false ->
            deduplication1(L, [E | ToList])
    end.

%% 字符串转整数
safe_list_to_integer(Args) when is_binary(Args) ->
    safe_list_to_integer(binary_to_list(Args));
safe_list_to_integer(Args) ->
    case string:to_integer(Args) of
        {error,no_integer} ->
            0;
        {I, S} ->
            I
    end.



