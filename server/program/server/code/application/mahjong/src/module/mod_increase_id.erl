%% @author zouv
%% @doc 临时自增Id

-module(mod_increase_id).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         start_link/1,
         get_mod_pid/0,
         
         get_unique_id/0,
         get_room_unique_id/0
        ]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {
                unique_id = 0,                      % 自增Id
                room_unique_id = 0                  % 房间自增Id
               }).

start_link(ModProcessName) ->
    gen_server:start_link(?MODULE, [ModProcessName], []).

get_mod_pid() ->
    misc:get_mod_pid(?MODULE).

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
init([ModProcessName]) ->
    process_flag(trap_exit, true),
    util_process:register_local(ModProcessName, self()),
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
do_call({'get_unique_id'}, _From, State) ->
    #state{
           unique_id = Id
          } = State,
    NewId = Id + 1,
    NewState = State#state{unique_id = NewId},
    {reply, NewId, NewState};

do_call({'get_room_unique_id'}, _From, State) ->
    #state{
           room_unique_id = Id
          } = State,
    NewId = Id + 1,
    NewState = State#state{room_unique_id = NewId},
    {reply, NewId, NewState};

do_call(Request, From, State) ->
    logger:warning_msg("~p call no match! info = ~p~n", [?MODULE, {Request, From}]),
    {reply, ok, State}.

%% ====================================================================
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
get_unique_id() ->
    gen_server:call(get_mod_pid(), {'get_unique_id'}).

get_room_unique_id() ->
    gen_server:call(get_mod_pid(), {'get_room_unique_id'}).
