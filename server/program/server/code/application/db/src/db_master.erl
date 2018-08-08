
-module(db_master).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

start_link()->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
init([]) ->
	db_init(),
	db_pick_table(),
	db_init_table(),
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    mnesia:dump_log(),
	logger:msg("dbmaster terminate Reason ~p ~n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
db_init() ->
    behaviour_db_game:init(),
    case mnesia:system_info(is_running) of
        yes -> mnesia:stop();
        no -> skip;
        starting -> mnesia:stop()
    end,
    %mnesia:create_schema([node(), 'test@127.0.0.1']).
    mnesia:create_schema([node()]).

db_pick_table() ->
    behaviour_db_game:pick_table([]).

db_init_table()->
    %rpc:call(node(), mnesia, change_config, [extra_db_nodes, ['test@127.0.0.1']]),
    mnesia:start(),
    db_tools:wait_tables_in_dbnode(),
    behaviour_db_game:init_table(),
    % db_tools:wait_tables_in_dbnode().
    ok.
