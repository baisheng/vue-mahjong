%%%--------------------------------------
%% Created: 2016-03-25 
%% Description: 动态编译game配置
%%%--------------------------------------

-module(dynamic_config).

-define(CONFIG_MODULE,  config_app).

-export([
         start/0
        ]).

%%
%% API Functions
%%
start() ->
    [FileName, Src] = dynamic_src(),
    dynamic_module(FileName, Src).

dynamic_module(FileName, Src) ->
    try
        {Mod, Code} = dynamic_compile:from_string(Src),
        code:load_binary(Mod, FileName ++ ".erl", Code),
        io:format("~n    ~p OK!", [?MODULE])
    catch
        Type:Error -> io:format("Error compiling mode: ~p, info: ~p ~p~n ~p~n", [FileName, Type, Error, erlang:get_stacktrace()])
    end.

dynamic_src() ->
    Fun = get_funs(),	
    ["dynamic_config",
     "-module(config).\t\n-compile(export_all).\t\n"
         ++ Fun ++ "\t\n"].

get_funs() ->
    Config_funs = [
                   {get_server_id, []},
                   {get_tcp_port, []},
                   {get_server_type, []},
                   {get_gm_cmd_on, []},
                   {get_node_id, []},
                   {is_server_type_dev, []},
                   {is_server_type_release, []},
                   {get_online_limit, []},
                   {get_platform_type, []},
                   {get_logfile_path, []},
                   {get_start_time, []},
                   {get_cross_server_id, []}
                  ],
    Funs = 
        lists:map(fun(Cfg) ->
            % io:format("dynamic config___ : ~p~n", [Cfg]),
            try
                {F, P, End} =
                    case Cfg of
                        {F0, P0} -> {F0, P0, "."};
                        {F0, P0, End0} -> {F0, P0, End0}
                    end,
                case P of 
                    [] ->
                        Config_value = util:term_to_string(erlang:apply(?CONFIG_MODULE, F, [])),
                        lists:concat([F, "()->", Config_value, End, "\t\n"]);
                    [P1] ->
                        Config_value = util:term_to_string(erlang:apply(?CONFIG_MODULE, F, [P1])),
                        lists:concat([F, "(", P1 , ")->", Config_value, End, "\t\n"])                            
                end
            catch
                _:_ -> io:format("Error____/~p/~n",[Cfg])
            end
        end,
        Config_funs),
    lists:concat(Funs).
