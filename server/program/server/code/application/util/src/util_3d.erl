%% @author zouv
%% @doc @todo 3D运算

-module(util_3d).

-export([
		distance/4,
		check_distance/5,

		radian_normalize/2,
		radian_vector_x/1,
		radian_vector_z/1
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================
%% 计算距离
distance(MX, MZ, TX, TZ) ->
    math:sqrt(math:pow(MX - TX, 2) + math:pow(MZ - TZ, 2)).

%% 检测距离
check_distance(MX, MZ, TX, TZ, Distance) ->
    math:pow(MX - TX, 2) + math:pow(MZ - TZ, 2) =< math:pow(Distance, 2).

%% 将角度缩小到范围0 ~ 2Pi
radian_normalize(Radian, Pi) when Radian < 0 ->
    Div = trunc(abs(Radian)*10000) div trunc(2*Pi*10000),
    Radian+(1-Div)*2*Pi;
radian_normalize(Radian, Pi) ->
    Div = trunc(abs(Radian)*10000) div trunc(2*Pi*10000),
    Radian-Div*2*Pi.


%% 获取和x轴夹角为Raidian的向量
radian_vector_x(Radian) ->
	Pi = math:pi(),
	Radian1 = radian_normalize(Radian, Pi),
	{AbsX, AbsZ} =
		if
			abs(Radian1 - Pi/2) =< 0.01 orelse
			abs(Radian1 - 3*Pi/2) =< 0.01 ->
				%% 与X轴夹角为90°
				{0, 1};
			true ->
				%%
				TanR = math:tan(Radian1),
				Sqrt = math:sqrt(TanR*TanR+1),
				X =
					if
						1/Sqrt < 0.0001 -> 0;
						true -> 1/Sqrt
					end,
				Z =
				    if
				    	abs(TanR / Sqrt) < 0.0001 -> 0;
				    	true -> abs(TanR / Sqrt)
				    end,
				{X, Z}
		end,
	radian_vector_x_1(Radian1, Pi, AbsX, AbsZ).

%% 根据角度大小，获取点的对应象限
%% @arg Radian的大小需要在 0 ~ 2Pi
radian_vector_x_1(Radian, Pi, AbsX, AbsZ) when Radian >= 3*Pi/2 -> {AbsX, -AbsZ};
radian_vector_x_1(Radian, Pi, AbsX, AbsZ) when Radian >= Pi -> {-AbsX, -AbsZ};
radian_vector_x_1(Radian, Pi, AbsX, AbsZ) when Radian >= Pi/2 -> {-AbsX, AbsZ};
radian_vector_x_1(_, _, AbsX, AbsZ) -> {AbsX, AbsZ}.


%% 获取和z轴夹角为Raidian的向量（此接口返回绝对值）（第一象限）
radian_vector_z(Radian) ->
	Pi = math:pi(),
	Radian1 = radian_normalize(Radian, Pi),
	{AbsX, AbsZ} =
		if
			abs(Radian1 - Pi/2) =< 0.01 orelse
			abs(Radian1 - 3*Pi/2) =< 0.01 ->
				{1, 0};
			true ->
				TanR = math:tan(Radian1),
				Sqrt = math:sqrt(TanR*TanR+1),
				X =
					if
						abs(TanR / Sqrt) < 0.0001 -> 0;
						true -> abs(TanR / Sqrt)
					end,
				Z =
					if
						1/Sqrt < 0.0001 -> 0;
						true -> 1/Sqrt
					end,
				{X, Z}
		end,
	radian_vector_z_1(Radian1, Pi, AbsX, AbsZ).


radian_vector_z_1(R, Pi, AbsX, AbsZ) when R>=0 andalso R<Pi/2 -> {AbsX, AbsZ};
radian_vector_z_1(R, Pi, AbsX, AbsZ) when R>=Pi/2 andalso R<Pi -> {AbsX, -AbsZ};
radian_vector_z_1(R, Pi, AbsX, AbsZ) when R>=Pi andalso R<3*Pi/2 -> {-AbsX, -AbsZ};
radian_vector_z_1(R, Pi, AbsX, AbsZ) when R>= 3*Pi/2 andalso R=<2*Pi -> {-AbsX, AbsZ}.
