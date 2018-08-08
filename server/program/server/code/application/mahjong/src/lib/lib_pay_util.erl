%% 后台验证

-module(lib_pay_util).

-export([check/1]).
-compile(export_all).

%% 已经自带默认参数
-define(URL,   "http://s.h5taotao.com/index2.php?g=Wap&m=Jiekouall&a=orderinfo").
-define(KEY,    "67541626855bbf83b816c5a45d895f59").
%% url = http://s.h5taotao.com/index2.php
%% url_args = ?g=Wap&m=jiekouall&a=orderinfo&order_id=&openid=&itemid=&sign=1
%% secretKey = "67541626855bbf83b816c5a45d895f59"
%% 格式：json 
%% 请求方式：post
test() ->
    OrderId = "6cc19b50a0fbfa503b71dbe80605b683",
    Price = 10,
    WechatId = "oM2JhwWWUPSAGJByYWY3Tem1n5fY",
    % ParaList = ["06aa909d918976431fb6e3a31bff33b5", 10,"oM2JhwWWUPSAGJByYWY3Tem1n5fY"],
    ParaList = [OrderId, Price, WechatId],
    Sign = sign(ParaList),
    io:format("check~p~n",[{WechatId,OrderId, Price, Sign}]),
    % PropList= lists:map(fun({K, V})  -> 
    %                         string:join([K, V], "=")
    %                     end, [  
    %                             {"order_id", OrderId},
    %                             {"openid", WechatId},
    %                             {"itemid", integer_to_list(Price)},
    %                             {"sign", Sign}
    %                           ]), 
    % io:format("check1~p~n",[PropList]),
    % PropStr = string:join(PropList, "&"),  
    % Url = lists:concat([?URL, "&", PropStr]),
    % io:format("check2~p~n",[{Url, PropList}]),
    % Body =   {obj, [ 
    %         {order_id, OrderId},
    %         {itemid, Price},
    %         {openid, WechatId},
    %         {sign, Sign}
    %       ]},
    % BodyStr = rfc4627:encode(Body),
    %  io:format("~p~n",[BodyStr]),
    BodyStr = lists:concat(["order_id=", OrderId, "&itemid=", Price, "&openid=", WechatId, "&sign=", Sign]),
    http_confirm_delivery(?URL, WechatId, BodyStr),
    ok.
check([WechatId, PlayerId, OrderId, Price|_]) ->
    ParaList = [OrderId, Price, WechatId],
    Sign = sign(ParaList),
    io:format("check~p~n",[{WechatId,OrderId, Price, Sign}]),
    % PropList= lists:map(fun({K, V})  -> 
    %                         string:join([K, V], "=")
    %                     end, [  
    %                             {"order_id", OrderId},
    %                             {"openid", WechatId},
    %                             {"itemid", integer_to_list(Price)},
    %                             {"sign", Sign}
    %                           ]), 
    % io:format("check1~p~n",[PropList]),
    % PropStr = string:join(PropList, "&"),  
    % Url = lists:concat([?URL, "&", PropStr]),
    % io:format("check2~p~n",[{Url, PropList}]),
    BodyStr = lists:concat(["order_id=", OrderId, "&itemid=", Price, "&openid=", WechatId, "&sign=", Sign]),
    http_confirm_delivery(?URL, WechatId, BodyStr).

sign(ParaList)  ->
    StrPara = lists:append(lists:concat(ParaList),?KEY),
    io:format("sign~p~n",[StrPara]),
    util:md5(StrPara).

http_confirm_delivery(Url, WechatId, BodyStr)  ->
    http_confirm_delivery(Url, WechatId, BodyStr, 2).
    
http_confirm_delivery(_Url, _WechatId, _BodyStr, 0)   ->
    {false, "服务器无法达到，请重新登录"};
http_confirm_delivery(Url, WechatId, BodyStr, Count)   ->
    Headers = [ 
                    % {"Content-Type", "application/json"},
                    % {"Content-Length",erlang:integer_to_list(length(BodyStr))}
              ],
    Res = httpc:request(post, {Url, Headers, "application/x-www-form-urlencoded", BodyStr}, [{timeout, 500*1000}], []), 
    io:format("http_confirm_delivery1~p~n",[Res]),
    case  Res of
        {ok, {{_, 200, _}, _, Content}}  ->  
            {ok, JsonData, _} = rfc4627:decode(lists:concat([Content])),
            io:format("http_confirm_delivery2~p~n",[{JsonData, Content}]),
            case rfc4627:get_field(JsonData, "status") of
                {ok, 1} ->
                    {ok, JsonData1} = rfc4627:get_field(JsonData, "data"),
                    % {ok,NewWechatId} = rfc4627:get_field(JsonData1, "wecha_id"),
                    {ok,Price} = rfc4627:get_field(JsonData1, "price"),
                    {ok, PayTime} = rfc4627:get_field(JsonData1, "add_time"),
                    NewPrice = util:safe_list_to_integer(Price),
                    NewPayTime = list_to_integer(binary_to_list(PayTime)),
                    io:format("http_confirm_delivery3~p~n",[{WechatId, NewPrice, NewPayTime}]),
                    {true, [WechatId, NewPrice, NewPayTime]};
                _ ->
                    {ok, Msg} = rfc4627:get_field(JsonData, "msg"),
                    {false, binary_to_list(Msg)}
            end;
        _   ->
            http_confirm_delivery(Url, WechatId, BodyStr, Count-1)
    end.
