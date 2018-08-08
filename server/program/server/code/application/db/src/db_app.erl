-module(db_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = db_sup:start_link(),
    behaviour_db_game:init_table_auto_increment(),
    {ok, Pid}.

stop(_State) ->
    ok.
