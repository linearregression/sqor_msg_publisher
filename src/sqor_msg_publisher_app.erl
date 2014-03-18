-module(sqor_msg_publisher_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("sqor_erl_common/include/app_logger.hrl").
-include_lib("sqor_msg_publisher_config.hrl").

-record(app_state, {saved_handlers}).
-spec start() -> {ok, pid(), #app_state{}}.
-spec start(any(), any()) -> {ok, pid(), #app_state{}}.
-spec stop(#app_state{}) -> ok.

-define(DEF_CONF_NAME, ?MODULE).


%% ===================================================================
%% Application callbacks
%% ===================================================================
start()-> 
    ConfigFile = app_config_helper:get_config_path(sqor_msg_publisher_conf), 
    ConfigFile = app_config_helper:get_config_file("sqor_msg_publisher_config.config",
                 sqor_msg_publisher_conf),
    start(normal, [{sqor_msg_publisher_conf, ConfigFile}]).


start(_StartType, [{sqor_msg_publisher_conf, ConfigFile}]) ->
    ?INFO_MSG("Starting ~s: start/2~n", [?MODULE]),    
    ok = create_config_table(),
    ok  = load_into_mnesia(ConfigFile)
    sqor_msg_publisher_sup:start_link().

stop(_State) ->
    ?INFO_MSG("Stopping ~s: stop/1~n", [?MODULE]),    
    ?INFO_MSG("Stopped ~s: stop/1~n", [?MODULE]),    
    ok.


create_config_table()->
    ?INFO_MSG("[~s]: Trying to initialize mnesia schema...",[?MODULE]),
    ok =  app_helper:stop_app(mnesia),
    ok = mnesia:start(), 
    create_config_table([sqor_msg_publisher_config, sqor_msg_pipeline_config]),    
    stopped = mnesia:stop(),
    ok.

create_config_table([]) -> ok;
create_config_table([Name|T]) when is_atom(Name) ->
    mnesia:delete_table(Name),
    mnesia:create_table(Name, [
            {type, set},
            {attributes, record_info(fields, Name)},
            {disc_copies, [node()]}
            ]),
    create_config_table([Name|T]). 

load_into_mnesia(ConfigFile) ->
    [Config] = sqor_msg_consumer_config:get_config_values(ConfilgFile), 
    [AmqpParams]= sqor_msg_consumer_config:get_connection_setting(Config),
    [Event_Mods] = ?supported_events, Config, []),
    
    mnesia:transaction(app_config_helper:insert(sqor_msg_publisher_config, AmqpParams)),
   % mnesia:transaction(sqor_msg_pipeline_config, message_processor, Values),


