%% @author zouv
%% @doc 玩家表自增Id

-module(db_auto_increment).
-behaviour(gen_server).

-define(INDEX_ID_BASE,              100000000).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([
         init_table_auto_increment/1,
         get_table_auto_increment/1
        ]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {
                table_auto_increment_dict
               }).

start_link()->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch
        Error:Reason ->
            logger:warning_msg("mod call Error! info = ~p~n, stack = ~p~n", [{Error, Reason, Request}, erlang:get_stacktrace()]),
            {reply, ok, State}
    end.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(Msg, State) ->
    try
        do_cast(Msg, State)
    catch
        Error:Reason ->
            logger:warning_msg("mod cast Error! info = ~p~n, stack = ~p~n", [{Error, Reason, Msg}, erlang:get_stacktrace()]),
            {noreply, State}
    end.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch
        Error:Reason ->
            logger:warning_msg("mod cast Error! info = ~p~n, stack = ~p~n", [{Error, Reason, Info}, erlang:get_stacktrace()]),
            {noreply, State}
    end.

%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    logger:warning_msg("~p terminated ~n", [?MODULE]),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
do_call({'get_table_auto_increment', Flag}, _From, State) ->
    Dict = State#state.table_auto_increment_dict,
    {ok, Index} = dict:find(Flag, Dict),
    NewIndex = Index + 1,
    NewDict = dict:store(Flag, NewIndex, Dict),
    db_agent_common_auto_id:update(Flag, NewIndex),
    NewState = State#state{table_auto_increment_dict = NewDict},
    ServerId = config:get_server_id(),
    ReturnId = ServerId * ?INDEX_ID_BASE + NewIndex,
    {reply, ReturnId, NewState};

do_call(Request, From, State) ->
    logger:warning_msg("~p call no match! info = ~p~n", [?MODULE, {Request, From}]),
    {reply, ok, State}.

%% ====================================================================
do_cast({'init_table_auto_increment', DbList}, State) ->
    Dict = 
        lists:foldl(fun(EFlag, Acc) ->
            EIndex = db_agent_common_auto_id:get(EFlag),
            io:format("~n init table auto increment: ~p~n", [{EFlag, EIndex}]),
            dict:store(EFlag, EIndex, Acc)
        end,
        dict:new(),
        DbList),
    NewState = State#state{table_auto_increment_dict = Dict},
    {noreply, NewState};

do_cast(Msg, State) ->
    logger:warning_msg("~p cast no match! info = ~p~n", [?MODULE, Msg]),
    {noreply, State}.

%% ====================================================================
do_info(Info, State) ->
    logger:warning_msg("~p info no match! info = ~p~n", [?MODULE, Info]),
    {noreply, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
init_table_auto_increment(DbList) ->
    gen_server:cast(?MODULE, {'init_table_auto_increment', DbList}).

get_table_auto_increment(Info) ->
    Flag = erlang:element(1, Info),
    gen_server:call(?MODULE, {'get_table_auto_increment', Flag}).
