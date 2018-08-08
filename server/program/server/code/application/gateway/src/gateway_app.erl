
-module(gateway_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
               % {"/", cowboy_static, {priv_file, gateway, "index.html"}},
               % {"/static/[...]", cowboy_static, {priv_dir, gateway, "static"}},
               {"/", ws_handler, []}
              ]}
        ]),
    Port = config:get_tcp_port(),
    {ok, _} = cowboy:start_clear(http, 100, [{port, Port}], #{ env => #{dispatch => Dispatch} }),
    gateway_sup:start_link().

stop(_State) ->
    ok.
