-module(sqor_msg_publisher_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([create_config_table/0, load_into_mnesia/1]).

-include_lib("sqor_constant.hrl").
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
    ok  = load_into_mnesia(ConfigFile),
    sqor_msg_publisher_sup:start_link().

stop(_State) ->
    ?INFO_MSG("Stopping ~s: stop/1~n", [?MODULE]),    
    ?INFO_MSG("Stopped ~s: stop/1~n", [?MODULE]),    
    ok.


create_config_table()->
    ?INFO_MSG("[~s]: Trying to initialize mnesia schema...",[?MODULE]),
    
    stopped = mnesia:stop(),
    ok = mnesia:start(), 
    mnesia:delete_table(sqor_supported_events_config),
    mnesia:delete_table(sqor_msg_publisher_config),
    mnesia:create_table(sqor_supported_events_config, [
            {type, set},
            {attributes, record_info(fields, sqor_supported_events_config)},
            {disc_copies, [node()]}
            ]),
    mnesia:create_table(sqor_msg_publisher_config, [
            {type, set},
            {attributes, record_info(fields, sqor_msg_publisher_config)},
            {disc_copies, [node()]}
            ]),

    ok = mnesia:start(),
    ?INFO_MSG("[~s]: Success to initialized mnesia schema...",[?MODULE]),

    ok.

load_into_mnesia(ConfigFile) ->
    [Config] = app_config_helper:get_config_values(ConfigFile), 
    [AmqpParams]= sqor_msg_consumer_config:get_connection_setting(Config),
    [Event_Mods] = ?parse(supported_events, Config, []),
    
    mnesia:transaction(app_config_helper:insert(sqor_msg_publisher_config, AmqpParams)).
   % mnesia:transaction(sqor_msg_pipeline_config, message_processor, Values),


