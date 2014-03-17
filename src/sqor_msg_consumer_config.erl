-module(sqor_msg_consumer_config).


%-export([start/0]).

-export([
        get_config_values/0, get_config_values/1,
        get_connection_setting/0, get_connection_setting/1, get_amqp_setting/1,
        get_exchange_setting/0, get_exchange_setting/1,
        get_queue_setting/0, get_queue_setting/1,
        get_sqor_consumer_setting/0, get_sqor_consumer_setting/1,
        extract_exchange_name/1,
        extract_queue_name/1,   
        extract_routing_key/1     
        ]).

% TODO store confing into mnesia
-export([start/0]).

% -include_lib("rabbit_common/include/rabbit_framing.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("sqor_consumer_constant.hrl").
-include_lib("sqor_consumer.hrl").
-include("sqor_consumer_config.hrl").
-include_lib("sqor_erl_common/include/app_logger.hrl").

-define(SQOR_CONSUMER_CONFIG, "sqor_msg_consumer.config").

start()->
    mnesia:create_table(config,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, config)}]),
    mnesia:add_table_copy(config, node(), ram_copies),
    mnesia:create_table(local_config,
                        [{ram_copies, [node()]},
                         {local_content, true},
                         {attributes, record_info(fields, local_config)}]),
    mnesia:add_table_copy(local_config, node(), ram_copies)
    
    .


%% @doc Get the filename of the application configuration file.
%% The filename can be specified with: erl -config "/path/to/sqor_msg_consumer.conf".
%% It can also be specified with the environtment variable SQOR_CONFIG_PATH.
%% If not specified, the default value 'sqor_msg_consumer.conf' is assumed.
-spec get_config_path() -> string().
get_config_path() ->
    case application:get_env(config) of
	{ok, Path} -> Path;
	undefined ->
	    case os:getenv(?SQOR_CONFIG_PATH_ENV) of
		false ->
		    ?CONFIG_PATH;
		Path ->
		    Path
	    end
    end.

%% @doc
%% Read config files that is parsed in.
%% @return  
%% @end
-spec get_config_path(filename()) -> string().
get_config_path(File) ->
   get_absolute_path(File).

%% @doc Read the contents of the config files and passed 
-spec get_config_values() -> list().
get_config_values()->
    Config = get_config_file(),
    ?INFO_MSG("[~s]: Reading config from ~s ~n",[?MODULE, Config]),
    app_helper:try_consult(Config).

%% @doc Read the contents of the config files and passed 
-spec get_config_values(filename()) -> list().
get_config_values(File)->
    Config = get_config_file(File),
    ?INFO_MSG("[~s]: Reading config from ~s ~n",[?MODULE, Config]),
    app_helper:try_consult(Config).

-spec get_config_file() -> filename().
get_config_file()-> 
    Path = get_config_path(), 
    get_config_file(Path).

-spec get_config_file(filename()) -> filename().
get_config_file(File) -> 
    Path = get_config_path(File),
    Ret = case filelib:is_dir(Path) of
        true -> filename:join(Path, ?SQOR_CONSUMER_CONFIG); 
        false -> Path;
        _ -> Path
    end,
    ?INFO_MSG("[~s]: Config file: ~s ~n",[?MODULE, Ret]),   
    Ret.

-spec get_connection_setting()-> #'amqp_params_network'{} | #'amqp_params_direct'{}.
get_connection_setting()->
    ConfList = get_config_values(), 
    get_connection_setting(ConfList).
 
-spec get_connection_setting(list())-> #'amqp_params_network'{} | #'amqp_params_direct'{}.
get_connection_setting(ConfList)->
    [sqor_amqp_connection:get_connection_setting(ConfList)].

-spec get_amqp_setting(list())-> #'amqp_params_network'{} | #'amqp_params_direct'{}.
get_amqp_setting(ConFlist) ->
    [sqor_amqp_connection:get_amqp_params(ConFlist)].

-spec get_exchange_setting()-> [#'exchange.declare'{}].
get_exchange_setting()->
    ConfList = get_config_values(), 
    get_exchange_setting(ConfList).

-spec get_exchange_setting(list())-> [#'exchange.declare'{}].
get_exchange_setting(ConfList)->
    [sqor_amqp_connection:get_exchange_setting(ConfList)].

-spec get_queue_setting()-> #'queue.declare'{}.
get_queue_setting()->
    ConfList = get_config_values(), 
    get_queue_setting(ConfList).

-spec get_queue_setting(list()) -> [#'queue.declare'{}].
get_queue_setting(ConfList)->
    [sqor_amqp_connection:get_queue_setting(ConfList)].

%-spec get_sqor_consumer_setting() -> [{amqp_connection, amqp_param()},                  {exchange_setting, #'exchange.declare'{}}, 
%{queue_setting, #'queue.declare'{}}].

-spec get_sqor_consumer_setting() -> [{amqp_connection, amqp_param()}]. 
get_sqor_consumer_setting() -> 
    get_sqor_consumer_setting([]).  

-spec get_sqor_consumer_setting(filename()) -> [{amqp_connection, amqp_param()}]. 
get_sqor_consumer_setting(File)->
    [ConfList] = get_config_values(File), 
    ConnectionSetting = get_connection_setting(ConfList),
    ExchangeSetting = get_exchange_setting(ConfList),
    QueueSetting = get_queue_setting(ConfList),    
    [{amqp_connection, ConnectionSetting}, 
     {exchange_setting, ExchangeSetting}, 
     {queue_setting, QueueSetting}].

-spec extract_exchange_name(#'exchange.declare'{}) -> binary().
extract_exchange_name(#'exchange.declare'{} = Exchange) ->
    #'exchange.declare'{exchange = ExchangeName} = Exchange,
    ExchangeName.

-spec extract_queue_name(#'queue.declare'{}) -> binary().
extract_queue_name(#'queue.declare'{} = Queue) ->
    #'queue.declare'{queue = QueueName} = Queue,
    QueueName. 

-spec extract_routing_key(#'queue.bind'{}) -> binary().
extract_routing_key(#'queue.bind'{} = QueueBind) ->
    #'queue.bind'{routing_key = RoutingKey} = QueueBind,
    RoutingKey.

%% @doc Convert configuration filename to absolute path.
%% Input is an absolute or relative path to an sqor_msg_consumer configuration file.
%% And returns an absolute path to the configuration file.
-spec get_absolute_path(string()) -> string().
get_absolute_path(File) ->
    case filename:pathtype(File) of
	absolute ->
	    File;
	relative ->
	    Config_path = get_config_path(),
	    Config_dir = filename:dirname(Config_path),
	    filename:absname_join(Config_dir, File)
    end.




   
