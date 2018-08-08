%% Author: Administrator
%% Created: 2012-2-16
%% Description: aStar算法

-module(util_a_star).

-define(LAYER_OBSTACLE,         0).
-define(LAYER_WALK,             1).

-define(FIND_PATH_MAX_COUNT,    1000).

-export([
         test/0,
         pix_to_grid/2,
         grid_to_pix/2,
         find_path/5,
         get_grid_size/0,
         check_can_pass/3,
         check_grid_can_pass/3,

         %buildPath/3,
         test_time/2
        ]).

%-compile(export_all).

%%
%% API Functions
%%
%-define(CHK_COORD_RATE, get_chk_coord_rate()).				% 坐标校验的倍率(1-不校验)
%% Key = {F,{X,Y},{G,H}}..
%% Val = Parent
-define(GRID_LENGTH, 1).                           % X
-define(GRID_HEIGHT, ?GRID_LENGTH).                % Y

%% get_chk_coord_rate() ->
%%     case get(chk_coord_rate) of
%%         undefined -> 1;
%%         0 -> 1;
%%         Num -> Num
%%     end.
%%
%% %% 获取格子的具体像素坐标
%% grid_to_pix(GridX, GridY, PixList) ->
%%     case lists:keyfind([GridX, GridY], 1, PixList) of
%%         {[GridX, GridY], {PixX, PixY}} ->
%%             {PixX, PixY};
%%         _ ->
%%             {GridX * ?GRID_LENGTH + ?GRID_LENGTH div 2, GridY * ?GRID_HEIGHT + ?GRID_HEIGHT div 2}
%%     end.
%%
%% update_pix_path(NewPath, OldPath) ->
%%     Fun =
%%         fun(H, AccList) ->
%%                 case H of
%%                     {[BX, BY],{Pix_X, Pix_Y}} ->
%%                         case lists:keyfind([BX, BY], 1, AccList) of
%%                             false ->
%%                                 [{[BX, BY],{Pix_X, Pix_Y}}|AccList];
%%                             _ ->
%%                                 lists:keyreplace([BX, BY], 1, AccList, {[BX, BY],{Pix_X, Pix_Y}})
%%                         end;
%%                     _ ->
%%                         AccList
%%                 end
%%         end,
%%     lists:foldr(Fun, OldPath, NewPath).

filt_re(Path) ->
    Fun =
        fun(H, {AccGirdL, AccList}) ->
            case H of
                [LX, LY]->
                    Pix_X = LX * ?GRID_LENGTH + ?GRID_LENGTH div 2,
                    Pix_Y = LY * ?GRID_HEIGHT + ?GRID_HEIGHT div 2,
                    case lists:keyfind([LX, LY], 1, AccList) of
                        false ->
                            {[[LX, LY] | AccGirdL], [{[LX, LY], [Pix_X, Pix_Y]} | AccList]};
                        _ ->
                            {AccGirdL, lists:keyreplace([LX, LY], 1, AccList, {[LX, LY],[Pix_X, Pix_Y]})}
                    end;
                _ ->
                    {AccGirdL, AccList}
            end
        end,
    {GirdList, PixList} = lists:foldl(Fun, {[],[]}, Path),
    {lists:reverse(GirdList), lists:reverse(PixList)}.

%% %% 小格子坐标转大格子坐标
%% coord_lit_to_big(X, Y) ->
%%     CHK_COORD_RATE = ?CHK_COORD_RATE,
%%     {X div CHK_COORD_RATE, Y div CHK_COORD_RATE}.

%% %% 像素坐标转小格子坐标
%% coord_pix_to_lit(X, Y) ->
%%     CHK_COORD_RATE = ?CHK_COORD_RATE,
%%     {X div (?GRID_LENGTH div CHK_COORD_RATE), Y div (?GRID_HEIGHT div CHK_COORD_RATE)}.

pix_to_grid(X, Y) ->
    if 
        X =< 0 ->
            GridX = 1;
        X rem ?GRID_LENGTH == 0 ->
            GridX = X div ?GRID_LENGTH;
        true ->
            GridX = X div ?GRID_LENGTH + 1
    end,
    if 
        Y =< 0 ->
            GridY = 1;
        Y rem ?GRID_LENGTH == 0 ->
            GridY = Y div ?GRID_HEIGHT;
        true ->
            GridY = Y div ?GRID_HEIGHT + 1
    end,
    {GridX, GridY}.

grid_to_pix(X, Y) ->
    FunCalc =
        fun(E, W) ->
            if 
                W < 2 ->
                    E;
                true ->
                    trunc((E - 1) * W + W / 2)
            end
        end,
    if 
        X =< 0 ->
            PixX = FunCalc(1, ?GRID_LENGTH);
        true ->
            PixX = FunCalc(X, ?GRID_LENGTH)
    end,
    if 
        Y =< 0 ->
            PixY = FunCalc(1, ?GRID_HEIGHT);
        true ->
            PixY = FunCalc(Y, ?GRID_HEIGHT)
    end,
    {PixX, PixY}.

%% 像素坐标转化为大格子坐标
% coord_pix_to_big(X, Y) ->
%     {X div (?GRID_LENGTH), Y div (?GRID_HEIGHT)}.

test_time(MapId,Count)->
    {MapPid, _LineId} = mod_map:get_scene_pid(MapId),
    gen_server:cast(MapPid, {test,Count}).

find_pos(_Member, [], _Pos)->
    false;
find_pos(Member, [H|T], Pos)->
    if Member =:= H ->
           Pos;
       true->
           find_pos(Member, T, Pos + 1)
    end.

%util_a_star:test().
test() ->
    a_star_find_path(52101, 110, 31, 11, 165, 1, false).

find_path(MapId, X, Y, TargetX, TargetY) ->
    {X1, Y1} = pix_to_grid(X, Y),
    {TargetX1, TargetY1} = pix_to_grid(TargetX, TargetY),
    if
        X1 == TargetX1 andalso Y1 == TargetY1 ->
            {[], []};
        true ->
            {GirdList, PixList} = a_star_find_path(MapId, X1, Y1, TargetX1, TargetY1, 1, false),
            {GirdList, PixList}
    end.

%%Flag(true-需要判定路径中的怪物为阻挡物，false-不需要判定路径中的怪物为阻挡物) 现在这里修改为了像素坐标
a_star_find_path(MapId, X, Y, TargetX, TargetY, Coord_Rate, Flag) ->
    put(chk_coord_rate, Coord_Rate),
    Check_Pos = {},
    Path1 = a_star_find_path1(MapId, TargetX, TargetY, init_open_list(X, Y), [], Flag, ?FIND_PATH_MAX_COUNT, 0),
    if
        length(Path1) == 1 ->
            filt_re(Path1);
        true ->
            Path2 = optimize_path(Path1, [], MapId, Flag, Check_Pos),
            Pos = find_pos([TargetX, TargetY], Path2, 1),
            if
                Pos =/= false ->
                    {Path3, _} = lists:split(Pos, Path2),
                    filt_re(Path3);
                true ->
                    filt_re(Path2)
            end
    end.

%% %%Flag(true-需要判定路径中的怪物为阻挡物，false-不需要判定路径中的怪物为阻挡物)
%% a_star_find_path(MapId,X1,Y1,TargetX1,TargetY1,MaxCount,Optimize, Coord_Rate, Flag)->
%%     put(chk_coord_rate, Coord_Rate),
%%     put(count,0),
%%     {X,Y} = coord_pix_to_lit(X1,Y1),
%%     {Begin_Big_X,Begin_Big_Y} = coord_pix_to_big(X1, Y1),
%%     {Target_Big_X,Target_Big_Y} = coord_pix_to_big(TargetX1,TargetY1),
%%     Check_Pos = {Begin_Big_X,Begin_Big_Y, Target_Big_X,Target_Big_Y},
%%     {TargetX,TargetY} = coord_pix_to_lit(TargetX1,TargetY1),
%%     Path = a_star_find_path1(lib_map:get_map_template_id(MapId),TargetX,TargetY,init_open_list(X,Y),[],MaxCount*Coord_Rate, Flag, Check_Pos),
%%     NewPath =
%%         if Optimize ->
%%                NewPath1 = optimize_path(Path, [],MapId,Flag, Check_Pos),
%%                filt_re(NewPath1, Begin_Big_X,Begin_Big_Y,Target_Big_X,Target_Big_Y);
%%            true->
%%                filt_re(Path, Begin_Big_X,Begin_Big_Y,Target_Big_X,Target_Big_Y)
%%         end,
%%     NewPath.

a_star_find_path1(MapId, TargetX, TargetY, OpenList, CloseList, Flag, MaxCount, Count)->
    NewCount = Count + 1,
    if
        NewCount > MaxCount ->
            buildPath(CloseList,{},[]);
        true->
            {Min, NewOpenList} = removeMin(OpenList),
            if
                %% 没有路，找出离终点最近的路径过去
                Min =:= 0 ->
                    buildPath(CloseList,{},[]);
                true->
                    {Key, Parent} = Min,
                    {_F, {X, Y}, _Val} = Key,
                    if
                        %% 到达终点
                        abs(X - TargetX) == 0 andalso abs(Y - TargetY) == 0 ->
                            buildPath([{{X, Y}, Parent} | CloseList], {}, []);
                        true ->
                            NewCloseList = [{{X, Y}, Parent} | CloseList],
                            NewOpenList1 = add_local(NewOpenList, NewCloseList, MapId, Key, TargetX, TargetY, Flag), % 添加到OpenList
                            a_star_find_path1(MapId, TargetX, TargetY, NewOpenList1, NewCloseList, Flag, MaxCount, NewCount)
                    end

            end
    end.

%%
%% Local Functions
%%

%%优化寻路结果
optimize_path([],NewPath,_MapId,_Flag, _Check_Pos)->
    NewPath;
optimize_path([CheckPoint|LeftPath],NewPath,MapId,Flag, Check_Pos)->
    case LeftPath of
        [CurrentPoint|Path]->
            case optimize_path_1(CheckPoint,CurrentPoint,Path,[],MapId,Flag, Check_Pos) of
                {stop,PathLast}->
                    NewPath ++ PathLast;
                {NextCurrentPointPath,PathLast}->
                    optimize_path(NextCurrentPointPath,NewPath++PathLast,MapId,Flag, Check_Pos)
            end;
        _->
            NewPath ++ LeftPath
    end.

%% 找出CheckPoint在接下来的路径中能走到最远的直线路径，不含起始点
optimize_path_1(_CheckPoint,CurrentPoint,[],Path,_MapId,_Flag, _Check_Pos)->
    case Path of
        []->
            {stop,[CurrentPoint]};
        _->
            {stop,Path}
    end;

optimize_path_1(CheckPoint,CurrentPoint,[H|T],Path,MapId,Flag, Check_Pos)->
    NewPath1 = make_straight_path(CheckPoint,H),
    NewPath = case check_path_moveable(NewPath1,MapId,Flag, Check_Pos) of
                  true->
                      NewPath1;
                  _->
                      []
              end,
    if NewPath =:= []->
           {[CurrentPoint|[H|T]],Path};
       true->
           optimize_path_1(CheckPoint,H,T,NewPath,MapId,Flag, Check_Pos)
    end.

%%查找两点连成的直线所经过的点，并检查这些点是否可以通过，不可以返回[]
check_path_moveable([], _MapId, _Flag, _Check_Pos)->
    true;
check_path_moveable([H|T], MapId, _Flag, Check_Pos)->
    %%加入可通行判断
    [X,Y] = H,
    case check_grid_can_pass(MapId, X, Y) of
        true->
            check_path_moveable(T, MapId, _Flag, Check_Pos);
        _->
            false
    end.

%%
make_straight_path(FirstPoint,SecondPoint)->
    [FirstPointX,FirstPointY] = FirstPoint,
    [SecondPointX,SecondPointY] = SecondPoint,
    if (SecondPointX - FirstPointX) =/= 0->
           K = abs(SecondPointY - FirstPointY) / abs(SecondPointX - FirstPointX),
           K2 = (SecondPointY - FirstPointY) / (SecondPointX - FirstPointX);
       true->
           K = 0,K2 = 0
    end,
    if (SecondPointY - FirstPointY =:= 0) andalso (SecondPointX - FirstPointX =:= 0)->
           [];
       (SecondPointY - FirstPointY =:= 0)->
           lists:map(fun(X)->[X,FirstPointY] end, seq(FirstPointX,SecondPointX,FirstPointX,[]));
       (SecondPointX - FirstPointX =:= 0)->
           lists:map(fun(Y)->[FirstPointX,Y] end, seq(FirstPointY,SecondPointY,FirstPointY,[]));
       true->
           make_path_point(FirstPointX,FirstPointY,SecondPointX,SecondPointY,K,[],FirstPointX,FirstPointY,K2)
    end.

seq(Start,End,Now,List)->
    Shift =
        if End >Start->
               1;
           true->
               -1
        end,
    Next = Now + Shift,
    if (End - Next)/Shift>=0 ->
           seq(Start,End,Next,[Next|List]);
       true->
           lists:reverse(List)
    end.

make_path_point(TempPointX,TempPointY,SecondPointX,SecondPointY,K,Line,FirstPointX,FirstPointY,K2) ->
    if K =<1->
           Shift =
               if SecondPointX >FirstPointX->
                      1;
                  true->
                      -1
               end,
           NextPointX = TempPointX+Shift,
           if (SecondPointX - NextPointX)/Shift>= 0->
                  NextPointY = trunc((NextPointX - FirstPointX)*K2 + FirstPointY),
                  make_path_point(NextPointX,0,SecondPointX,SecondPointY,K,[[NextPointX,NextPointY]|Line],FirstPointX,FirstPointY,K2);
              true->
                  lists:reverse(Line)
           end;
       true->
           Shift =
               if SecondPointY >FirstPointY->
                      1;
                  true->
                      -1
               end,

           NextPointY = TempPointY+Shift,
           if (SecondPointY - NextPointY )/Shift >=0 ->
                  NextPointX = trunc((NextPointY - FirstPointY)/K2 + FirstPointX),
                  make_path_point(0,NextPointY,SecondPointX,SecondPointY,K,[[NextPointX,NextPointY]|Line],FirstPointX,FirstPointY,K2);
              true->
                  lists:reverse(Line)
           end
    end.

buildPath([], _Parent, Path) ->
    Path;
buildPath([H | T], _Parent, []) ->
    {{X, Y}, Parent} = H,
    buildPath(T, Parent, [[X, Y]]);
buildPath(List, Parent, Path) ->
    if
        Parent =:= {} ->
            Path;
        true ->
            case lists:keyfind(Parent, 1, List) of
                false->
                    Path;
                Element->
                    {{X, Y}, NewParent} = Element,
                    buildPath(List, NewParent, [[X, Y] | Path])
            end
    end.

add_local(OpenList, CloseList, MapId, Key, TargetX, TargetY, Flag)->
    {_F, {X, Y}, {G, _H}} = Key,
    NewG = G + 1,
    Parent = {X, Y},
    NewOpenList1 = calc(OpenList,CloseList,NewG,X-1,Y,TargetX,TargetY,MapId,Parent, Flag),
    NewOpenList2 = calc(NewOpenList1,CloseList,NewG,X,Y-1,TargetX,TargetY,MapId,Parent, Flag),
    NewOpenList3 = calc(NewOpenList2,CloseList,NewG,X+1,Y+1,TargetX,TargetY,MapId,Parent, Flag),
    NewOpenList4 = calc(NewOpenList3,CloseList,NewG,X,Y+1,TargetX,TargetY,MapId,Parent, Flag),
    NewOpenList5 = calc(NewOpenList4,CloseList,	NewG,X-1,Y-1,TargetX,TargetY,MapId,Parent, Flag),
    NewOpenList6 = calc(NewOpenList5,CloseList,NewG,X-1,Y+1,TargetX,TargetY,MapId,Parent, Flag),
    NewOpenList7 = calc(NewOpenList6,CloseList,NewG,X+1,Y+1,TargetX,TargetY,MapId,Parent, Flag),
    calc(NewOpenList7,CloseList,NewG,X+1,Y-1,TargetX,TargetY,MapId,Parent, Flag).

calc(OpenList, CloseList, G, X, Y, TargetX, TargetY, MapId, Parent, _Flag)->
    case check_grid_can_pass(MapId, X, Y) of
        true ->
            case lists:keyfind({X, Y}, 1, CloseList) of
                false ->
                    case findElement(OpenList, X, Y) of
                        false ->
                            H = evaluateH(X, Y, TargetX, TargetY),
                            insertElement(OpenList, X, Y, Parent, G, H);
                        Key->
                            case Key of
                                {_F, {X, Y},{OldG, H}}->
                                    if
                                        G < OldG ->
                                            NewOpenList = delete(OpenList, Key),
                                            insert({G + H,{X, Y},{G, H}}, Parent, NewOpenList);
                                        true->
                                            OpenList
                                    end;
                                _->
                                    OpenList
                            end
                    end;
                _->
                    OpenList
            end;
        _->
            OpenList
    end.

%% 获取网格大小
get_grid_size() ->
    ?GRID_LENGTH.

%% 查找X,Y格子能不能站立
%% 查找X,Y格子能不能站立
check_can_pass(MapId, PixX, PixY) ->
    {GridX, GridY} = pix_to_grid(PixX, PixY),
    check_grid_can_pass(MapId, GridX, GridY).

check_grid_can_pass(MapId, GridX, GridY) ->
    {IsSupportDoor, DoorList, Layer} = lib_map:get_pos_layer(MapId, GridX, GridY),
    if 
        IsSupportDoor ->
            Isvalid = lists:member(Layer, DoorList),
            Layer == ?LAYER_WALK orelse Layer < 0 orelse Isvalid;
        true ->
            Layer /= ?LAYER_OBSTACLE
    end.

%% 计算估价,F = G + H
evaluateH(X, Y, TargetX, TargetY)->
    MovX = abs(TargetX - X),
    MovY = abs(TargetY - Y),
    min(MovX , MovY) * 1.4 + abs(MovX - MovY).

%%用gb_trees做的假堆
findElement(Heap,X,Y)->
    case get({f,X,Y}) of
        undefined->
            false;
        {F,G,H}->
            Key = {F,{X,Y},{G,H}},
            case gb_trees:lookup(Key, Heap) of
                none->
                    false;
                _->
                    Key
            end;
        _->
            false
    end.

delete(Heap,Key)->
    gb_trees:delete(Key, Heap).
insert(Key,Parent,Heap)->
    case Key of
        {_,{X,Y},{G,H}}->
            try (gb_trees:insert(Key, Parent, Heap)) of
                NewHeap->
                    put({f,X,Y},{G+H,G,H}),
                    NewHeap
            catch
                _:_->
                    io:format("Key exsit ~p~n", [{Key,Heap}]),
                    Heap
            end;
        _->
            io:format("wrong key~p~n",[Key]),
            Heap
    end.

removeMin(Heap)->
    case gb_trees:is_empty(Heap) of
        true->
            {0, Heap};
        _ ->
            {Key, Val, NewHeap} = gb_trees:take_smallest(Heap), % 去除F值最小的
            Min = {Key, Val},
            {Min, NewHeap}
    end.

%% 起始点首先放入OpenList
init_open_list(X, Y) ->
    insertElement([], X, Y, {}, 0, 0).

insertElement([], X, Y, Parent, G, H)->
    insertElement({0, nil}, X, Y, Parent, G, H);
insertElement(Heap, X, Y, Parent, G, H)->
    Key = {G + H, {X, Y}, {G, H}},
    case gb_trees:lookup(Key, Heap) of
        none ->
            try (gb_trees:insert(Key, Parent, Heap)) of
                NewHeap ->
                    put({f, X, Y},{G + H, G, H}),
                    NewHeap
            catch
                _:_->
                    Heap
            end;
        _ ->
            Heap
    end.

%%draw_path(Path,MapId)->
%%	case ets:lookup(ets_map_size, MapId) of
%%		[{MapId,{MaxX,MaxY}}]->
%%			NewPath = lists:sort(Path),
%%			draw_path_1(1,1,NewPath,MaxX,MaxY,MapId);
%%		_->
%%			skip
%%	end.
%%draw_path_1(X,Y,[],MaxX,MaxY,MapId)->
%%	lists:foreach(fun(_N)->io:format("-") end, lists:seq(1, MaxX));
%%draw_path_1(X,Y,[H|T],MaxX,MaxY,MapId)->
%%	if [X,Y] =:= H->
%%			NextPath = T;
%%		true->
%%			NextPath = [H|T]
%%	end,
%%	if (Y+1) >=MaxY->
%%			NextX = X+1,
%%			NextY = 1,
%%			draw_path_1(NextX,NextY,NextPath,MaxX,MaxY,MapId);
%%		X > MaxX->
%%			ok;
%%		true->
%%			NextX = X,
%%			NextY = Y+1,
%%			draw_path_1(NextX,NextY,NextPath,MaxX,MaxY,MapId)
%%	end.


%% %%大地图导航
%% %%扫描所有传送门，填写传送表
%% %%效率太慢，不用了
%% make_transfer_table()->
%% 	DoorList = ets:tab2list(?ETS_DOOR_TEMPLATE),
%% 	MS = ets:fun2ms(fun(Template) when Template#ets_map_template.type =:= 1-> Template#ets_map_template.map_id end),
%% 	MapList = ets:select(?ETS_MAP_TEMPLATE, MS),
%% 	ets:new(door_table, [{keypos,1}, named_table, public ,set]),
%% 	make_door_table_1(MapList,MapList),
%% 	make_door_table(DoorList),
%% 	make_door_table_2(MapList,MapList).

% get_time()->
%     case get(autoid) of
%         undefined->
%             put(autoid,1),
%             0;
%         Num->
%             put(autoid,Num+1),
%             Num
%     end.


% make_door_table_2([],_MapList)->
%     ok;
% make_door_table_2([H|T],MapList)->
%     Fun = fun(Id)->
%                   case ets:lookup(door_table, {H,Id}) of
%                       [{{H,Id},undefined}]->		%%未赋值
%                           %?DEBUG("~p ~p~n",[get_time(),length(MapList)]),
%                           find_door_path([H],Id,0,0);
%                       _->
%                           skip
%                   end
%           end,
%     lists:foreach(Fun, MapList),
%     make_door_table_2(T, MapList).

%%找出从一个门到另一个门的路径
% find_door_path([],_Id,_Parent,_AutoTime)->
%     false;
% find_door_path([H|T],Id,Parent,AutoTime)->
%     %%先找到该地图能到达的所有地图
%     if H =:= Id->
%            true;
%        AutoTime>10->
%            false;
%        true->
%            MS = ets:fun2ms(fun({{FirstId,NextId},Value}) when FirstId =:= H andalso Value =/= undefined->
%                                    NextId end),
%            MapList = ets:select(door_table, MS),
%            Res = find_door_path(lists:delete(Parent, MapList), Id,H,AutoTime+1),
%            case Res of
%                _ when Parent =:= 0->
%                    skip;
%                true->		%%直接到达
%                    ets:insert(door_table, {{Parent,Id},H}),
%                    H;
%                MapId when is_integer(MapId)->			%%子节点可以到达
%                    ets:insert(door_table, {{Parent,Id},H});
%                false->
%                    find_door_path(T,Id,Parent,AutoTime+1)
%            end
%     end.

% make_door_table_1([],_MapList)->
%     ok;
% make_door_table_1([H|T],MapList)->
%     Fun = fun(Id)->
%                   ets:insert(door_table, {{H,Id},undefined})
%           end,
%     lists:foreach(Fun, MapList),
%     make_door_table_1(T, MapList).


%% make_door_table([])->
%% 	ok;
%% make_door_table([H|T])->
%% %% 	?DEBUG("~p~n",[H]),
%% 	Enter = H#ets_door_template.current_map_id,
%% 	Next = H#ets_door_template.next_map_id,
%% 	if Next =:= -1->
%% 			skip;
%% 		true->
%% 			NextDoor = data_agent:door_template_get(H#ets_door_template.next_door_id),
%% 			if is_record(NextDoor,ets_door_template)->
%% 						ets:insert(door_table, {{Enter,Next},{H#ets_door_template.pos_x,H#ets_door_template.pos_y,NextDoor#ets_door_template.pos_x,NextDoor#ets_door_template.pos_y}});
%% 			true->
%% 				skip
%% 			end
%% 	end,
%% 	make_door_table(T).

%%test() ->
%%	H = #ets_users{id = 3, career = 1},
%%	case H of
%%		#ets_users{_ = '_'} ->
%%			1;
%%		_ ->
%%			#ets_users{_ = '_'}
%%	end.
%%test1(Id, LoopNum) ->
%%	BeginTime = misc_timer:now_milseconds(),
%% 	io:format("\n test a* begin loop_num[~p] \n",[LoopNum]),
%%	User = lib_player:get_online_player_info_by_id(Id),
%%%% 	io:format("~p\n", [User#ets_users.id]),
%%	Res = test1_1(User, LoopNum),
%%	EndTime = misc_timer:now_milseconds(),
%% 	io:format("\n test a* end loop_num[~p] cost_time [~p] milsec res:[~p]\n",[LoopNum, EndTime - BeginTime, Res]).
%%
%%test1_1(_User, 0) ->
%%	true;
%%
%%test1_1(User, Num) ->
%%	if is_record(User, ets_users) ->
%%			test1_1(User, Num - 1);
%%		true ->
%%			test1_1(User, Num - 1)
%%%% 			io:format("111~p\n", [Num]),
%%%% 			false
%%	end.
