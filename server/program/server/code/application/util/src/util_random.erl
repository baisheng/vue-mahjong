-module(util_random).

-include_lib("eunit/include/eunit.hrl").

-define(NOTEST, true).

-export([
         get_random_weight/2,
         round_desk/3
         
         %get_random_rewards/1
        ]).

%% 按权重获取
get_random_weight(KeyList, WeightList) ->
    {Acc, _AccSum} = lists:mapfoldl(fun(W, Sum) -> {W + Sum, W + Sum} end, 0, WeightList),
    RandPos = util:rand(1, lists:last(Acc)),
    [{_, Key} | _] = lists:dropwhile(fun({EPos, _EKey}) -> EPos < RandPos end, lists:zip(Acc, KeyList)),
    [Key].

%% %% 按权重获取 
%% get_random_rewards({random_weight, L_terms, L_weights}) ->
%%     {Acc, _AccSum} = lists:mapfoldl(fun(X, Sum) -> {X+Sum, X+Sum} end, 0, L_weights),
%%     RandPos = util:uniform() * lists:last(Acc),
%%     [{_, Term} | _] = lists:dropwhile(fun({N, _}) -> N < RandPos end, lists:zip(Acc, L_terms)),
%%     [Term];
%% 
%% %% 按概率获取
%% get_random_rewards({random_simple, L_terms, L_rates}) ->
%%     get_random_rewards1(L_terms, L_rates, []).
%% 
%% get_random_rewards1([], [], Result) ->
%%     Result;
%% get_random_rewards1([Term | T_terms], [Rate | T_rates], Result) ->
%%     Random = util:uniform(),
%%     if
%%         Random < Rate ->
%%             get_random_rewards1(T_terms, T_rates, [Term | Result]);
%%         true ->
%%             get_random_rewards1(T_terms, T_rates, Result)
%%     end.


%% 圆桌概率
round_desk([], _Rand, Default) -> Default;
round_desk([{Tag, Min, Max} | Left], Rand, Default) ->
    if
        Rand >= Min andalso Rand < Max ->
            Tag;
        true ->
            round_desk(Left, Rand, Default)
    end.