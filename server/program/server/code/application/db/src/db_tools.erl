%% 数据库表创建、表结构转换
%% zouv

-module(db_tools).

-include("record.hrl").
-include("table_record.hrl").

-export([
		 create_table/7,
		 wait_tables_in_dbnode/0,
         
         export_db_to_txt/1
		]).

create_table(Tab, Attributes, DefaultRecord, Module, Indices, 'ordered_set' = Type, _Copies) ->
    create_table_disc(Tab, Attributes, DefaultRecord, Module, Indices, Type, 'disc_copies');
create_table(Tab, Attributes, DefaultRecord, Module, Indices, Type, 'disc_copies' = Copies) ->
    create_table_disc(Tab, Attributes, DefaultRecord, Module, Indices, Type, Copies);
create_table(Tab, Attributes, DefaultRecord, Module, Indices, Type, 'disc_only_copies' = Copies) ->
    create_table_disc(Tab, Attributes, DefaultRecord, Module, Indices, Type, Copies);
create_table(Tab, Attributes, DefaultRecord, Module, Indices, Type, 'ram_copies' = Copies) ->
    create_table_disc(Tab, Attributes, DefaultRecord, Module, Indices, Type, Copies).

create_table_disc(Tab, Attributes, DefaultRecord, Module, Indices, Type, Copies) ->
    NeedCreate = 
        try
            OldType = mnesia:table_info(Tab, type),
            OldAttributes = mnesia:table_info(Tab, attributes),
            OldIndexNums = mnesia:table_info(Tab, index),
            NewIndexNums = get_table_index(Attributes, Indices),
            if
                %% attributes有变化
                Attributes /= OldAttributes ->
                    io:format("----- table [~p] attributes is different! ~n", [Tab]),
                    table_transfer(Tab, Module, OldAttributes, Attributes, DefaultRecord, Indices, Type),
                    false;
                %% type有变化
                Type /= OldType ->
                    io:format("----- table [~p] type is different! ~n", [Tab]),
                    table_transfer(Tab, Module, OldAttributes, Attributes, DefaultRecord, Indices, Type),
                    false;
                %% index有变化
                OldIndexNums /= NewIndexNums ->
                    io:format("----- table [~p] index is different! ~n", [Tab]),
                    table_transfer(Tab, Module, OldAttributes, Attributes, DefaultRecord, Indices, Type),
                    false;
                true ->
                    false
            end
        catch
            Error:Reason ->
                case Reason of
                    {aborted, {no_exists, _, _}} ->
                        skip;
                    _ ->
                        logger:msg("create table1 Error! ~p~n", [{?LINE, Tab, Error, Reason, erlang:get_stacktrace()}])
                end,
                true
        end,
    case NeedCreate of
        true ->
            case Indices of
                [] ->
                    R = mnesia:create_table(Tab, [{Copies, [node()]},
                                                  {attributes, Attributes},
                                                  {type,Type}]);
                _ ->
                    R = mnesia:create_table(Tab, [{Copies, [node()]},
                                                  {attributes, Attributes},
                                                  {index,Indices},
                                                  {type,Type}])
            end;
        false ->
            R = ok
    end,
    case R of
        {atomic, ok} ->
            ok;
        ok ->
            ok;
        _ ->
            logger:msg("create table1 Error! ~p~n", [{Tab, R}]),
            error
    end.

wait_tables_in_dbnode()	->
	AllTablesList = mnesia:system_info(tables),        % 获取表
	mnesia:wait_for_tables(AllTablesList, infinity).   % 阻塞调用者直到指定表初始化完成

%% -------------------------
%% mnesia 转换
%% zouv_20140401
%% -------------------------
table_transfer(Tab, Modlue, OldAttributes, Attributes, DefaultRecord, Indices, Type) ->
    % TODO temp_db 续传的问题
    NowSeconds1 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    %% 临时保存
	[Tab | DefaultValues] = erlang:tuple_to_list(DefaultRecord),
    io:format("table transfer___1 back up! [~p]~n", [Tab]),
    TempTab = table_transfer_save(Tab, OldAttributes, Indices, Type),
    %% 删除旧结构，创建新结构
    {atomic, ok} = mnesia:delete_table(Tab),
    [EtsInfo] = ets:lookup(?ETS_BEHAVIOUR_DB_GAME, Modlue),
    #ets_behaviour_db_game{
        module = EModule,
        table_name = ETableName,
        table_fields = ETableFields,
        table_values = ETableValues,
        indices = EIndices,
        semantics = ESemantics,
        copies = ECopies
    } = EtsInfo,
    ok = create_table_disc(ETableName, ETableFields, ETableValues, EModule, EIndices, ESemantics, ECopies),
    io:format("table transfer___2 transfer [~p] to [~p] ~n", [TempTab, Tab]),
    %% 转换
    case db_agent:first(TempTab) of
        {ok, FirstId} when FirstId /= '$end_of_table' ->
            table_transfer1(Tab, TempTab, FirstId, OldAttributes, Attributes, DefaultValues);
        _ ->
            skip
    end,
    io:format("table transfer___3 delete temp db! [~p]~n", [TempTab]),
    {atomic, ok} = mnesia:delete_table(TempTab),
    NowSeconds2 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    io:format("table transfer___4 succeed! seconds: ~p~n", [NowSeconds2 - NowSeconds1]),
    ok.

table_transfer1(Tab, TempTab, Id, OldAttributes, Attributes, DefaultValues) ->
    TabDataList = db_agent:read(TempTab, Id),    
    table_transfer2(TabDataList, Tab, Attributes, OldAttributes, Attributes, DefaultValues),
    case db_agent:next(TempTab, Id) of
        {ok, NextId} when NextId /= '$end_of_table' ->
            table_transfer1(Tab, TempTab, NextId, OldAttributes, Attributes, DefaultValues);
        _ ->
            skip
    end.

table_transfer2([], _Tab, _Attributes, _OldAttributes, _Attributes, _DefaultValues) ->
    skip;
table_transfer2([TabData | L], Tab, Attributes, OldAttributes, Attributes, DefaultValues) ->
    [_TempTab | Values] = tuple_to_list(TabData),
    NewValues = table_transfer3(Attributes, OldAttributes, DefaultValues, Values, 1, []),
    NewTabData = erlang:list_to_tuple([Tab | NewValues]),
    ok = db_agent:write(NewTabData),
    table_transfer2(L, Tab, Attributes, OldAttributes, Attributes, DefaultValues).

table_transfer3([], _OldAttributes, _DefaultValues, _OldValues, _Index, Values) ->
	Values;
table_transfer3([E | List], OldAttributes, DefaultValues, OldValues, Index, Values) ->
	case lists:member(E, OldAttributes) of
		true ->
			AttrIndex = table_transfer_get_attr_pos(OldAttributes, E, 1),
			Value = lists:nth(AttrIndex, OldValues);
		_ ->
			Value = lists:nth(Index, DefaultValues)
	end,
	NewIndex = Index + 1,
	NewValues = Values ++ [Value],
	table_transfer3(List, OldAttributes, DefaultValues, OldValues, NewIndex, NewValues).

table_transfer_get_attr_pos([E | AttributeList], Attribute, Pos) ->
	if
		E == Attribute ->
			Pos;
		true ->
			table_transfer_get_attr_pos(AttributeList, Attribute, Pos + 1)
	end.

table_transfer_save(Tab, Attributes, _Indices, Type) ->
    TempTab = erlang:list_to_atom("temp_db_" ++ atom_to_list(Tab)),
    mnesia:delete_table(TempTab),
    if
        Type == 'set' orelse Type == 'ordered_set' ->
            NewType = 'set';
        Type == 'bag' ->
            NewType = 'bag'
    end,
    ok = create_table(TempTab, Attributes, [], [], [], NewType, 'disc_only_copies'), % 写入磁盘
    case db_agent:first(Tab) of
        {ok, FirstId} when FirstId /= '$end_of_table' ->
            table_transfer_save1(TempTab, Tab, FirstId);
        _ ->
            skip
    end,
    TempTab.

table_transfer_save1(TempTab, Tab, Id) ->
    TabDataList =  db_agent:read(Tab, Id),
    table_transfer_save2(TabDataList, TempTab),
    case db_agent:next(Tab, Id) of
        {ok, NextId} when NextId /= '$end_of_table' ->
            table_transfer_save1(TempTab, Tab, NextId);
        _ ->
            skip
    end.

table_transfer_save2([], _TempTab) ->
    skip;
table_transfer_save2([TabData | L], TempTab) ->
    [_Tab | Values] =  tuple_to_list(TabData),
    NewTabData = erlang:list_to_tuple([TempTab | Values]),
    ok = db_agent:write(NewTabData),
    table_transfer_save2(L, TempTab).

%% 获去索引编号
get_table_index(Attributes, Indices) ->
    lists:map(fun(E) ->
                  get_table_index1(Attributes, E, 2)
              end, 
              Indices).

get_table_index1([], _IndexTerm, Num) ->
    Num;
get_table_index1([E | Attributes], IndexTerm, Num) ->
    if
        E == IndexTerm ->
            Num;
        true ->
            get_table_index1(Attributes, IndexTerm, Num + 1)
    end.

%% -------------------------
%% 在运行终端上手动修改结构信息
%% zouv_
%% -------------------------
%% -record(old, {key, val}).
%% -record(new, {key, val, extra}).
%% Transformer =
%%             fun(X) ->
%%                       #new{key = X#old.key, val = X#old.val, extra = 42}
%%             end,
%% {atomic, ok} = mnesia:transform_table(foo, Transformer, record_info(fields, new)).


%% ==================================================
%% 数据导出
%% ==================================================
%% 导出数据库数据（以数据方式）
%db_tools:export_db_to_txt(player_info).
export_db_to_txt(Tab) ->
    case db_agent:first(Tab) of
        {ok, FirstId} when FirstId /= '$end_of_table' ->
            FileName = lists:concat(["export_db_to_txt_", Tab, ".txt"]),
            {ok, FileDevice} = file:open(FileName, [write]),
            export_db_to_txt1(Tab, FirstId, FileDevice),
            file:close(FileDevice);
        _ ->
            skip
    end.

export_db_to_txt1(Tab, FirstId, FileDevice) ->
    TabDataList =  db_agent:read(Tab, FirstId),
    export_db_to_txt2(TabDataList, FileDevice),
    case db_agent:next(Tab, FirstId) of
        {ok, NextId} when NextId /= '$end_of_table' ->
            export_db_to_txt1(Tab, NextId, FileDevice);
        _ ->
            ok
    end.

export_db_to_txt2([], _FileDevice) ->
    skip;
export_db_to_txt2([TabData | L], FileDevice) ->
    Str = binary_to_list(list_to_binary(io_lib:format("~p", [TabData]))),
    io:fwrite(FileDevice, "~s.~n", [Str]),
    export_db_to_txt2(L, FileDevice).
