%% zouv
%% 数学类接口

-module(util_math).

-export([
         ceil/1,
         floor/1,
         keep_point_ceil/2,
         keep_point_floor/2,
         keep_point_round/2,
         abs/1,
         angle_to_radian/1,
         radian_to_angle/1,
         radian_minus/2
        ]).

%% 向上取整
ceil(X)  ->
    T = erlang:trunc(X),
    case X - T of
        P when P <0 ->  T;
        P when P >0 ->  T + 1;
        _   ->  T
    end.

%% 向下取整
floor(X)    ->
    T = erlang:trunc(X),
    case X - T of
        P when P < 0 -> T - 1;
        P when P > 0 -> T;
        _   ->  T
    end.

%% 保留n位小数（小数向上取整）
keep_point_ceil(Float, N) ->
    ceil(Float * math:pow(10, N)) / math:pow(10,N).
%% 保留n位小数（小数向下取整）
keep_point_floor(Float, N) ->
    floor(Float * math:pow(10, N)) / math:pow(10,N).
%% 保留n位小数（小数四舍五入）
keep_point_round(Float, N) ->
    round(Float * math:pow(10, N)) / math:pow(10,N).


%% 取绝对值
abs(N) ->
    if
        N >= 0 ->
            N;
        true ->
            -N
    end.


%% 角度转弧度
angle_to_radian(Angle) ->
    Angle/180*math:pi().

radian_to_angle(Radian) ->
    Radian/math:pi()*180.

%% 弧度计算
radian_minus(Radian1, Radian2) when Radian1 < Radian2 ->
    New1 = Radian1 + 2*math:pi(),
    radian_minus(New1, Radian2);
radian_minus(Radian1, Radian2) ->
    Radian1 - Radian2.

