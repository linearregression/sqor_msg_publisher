-module(sqor_msg_publisher_config).


%-export([start/0]).

-export([
        get_connection_setting/0, get_connection_setting/1, get_amqp_setting/1,
        get_exchange_setting/0, get_exchange_setting/1,
        get_queue_setting/0, get_queue_setting/1,
        get_sqor_publisher_setting/0, get_sqor_publisher_setting/1,
        extract_exchange_name/1,
        extract_queue_name/1,   
        extract_routing_key/1     
        ]).

% -include_lib("rabbit_common/include/rabbit_framing.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("kernel/include/file.hrl").
% -include_lib("sqor_publisher_constant.hrl").
%-include_lib("sqor_publisher.hrl").
%-include("sqor_publisher_config.hrl").
-include_lib("sqor_erl_common/include/app_logger.hrl").

-define(SQOR_publisher_CONFIG, "sqor_msg_publisher.config").
-type amqp_param() :: #'amqp_params_network'{} | #'amqp_params_direct'{}.    
 

-spec get_connection_setting()-> #'amqp_params_network'{} | #'amqp_params_direct'{}.
get_connection_setting()->
    ConfList = app_config_helper:get_config_values(), 
    get_connection_setting(ConfList).

-spec get_connection_setting(list())-> #'amqp_params_network'{} | #'amqp_params_direct'{}.
get_connection_setting(ConfList)->
    [sqor_amqp_connection:get_connection_setting(ConfList)].

-spec get_amqp_setting(list())-> #'amqp_params_network'{} | #'amqp_params_direct'{}.
get_amqp_setting(ConFlist) ->
    [sqor_amqp_connection:get_amqp_params(ConFlist)].

-spec get_exchange_setting()-> [#'exchange.declare'{}].
get_exchange_setting()->
    ConfList = app_config_helper:get_config_values(), 
    get_exchange_setting(ConfList).

-spec get_exchange_setting(list())-> [#'exchange.declare'{}].
get_exchange_setting(ConfList)->
    [sqor_amqp_connection:get_exchange_setting(ConfList)].

-spec get_queue_setting()-> #'queue.declare'{}.
get_queue_setting()->
    ConfList = app_config_helper:get_config_values(), 
    get_queue_setting(ConfList).

-spec get_queue_setting(list()) -> [#'queue.declare'{}].
get_queue_setting(ConfList)->
    [sqor_amqp_connection:get_queue_setting(ConfList)].

%-spec get_sqor_publisher_setting() -> [{amqp_connection, amqp_param()},                  {exchange_setting, #'exchange.declare'{}}, 
%{queue_setting, #'queue.declare'{}}].

-spec get_sqor_publisher_setting() -> [{amqp_connection, amqp_param()}]. 
get_sqor_publisher_setting() -> 
    get_sqor_publisher_setting([]).  

-spec get_sqor_publisher_setting(file:filename()) -> [{amqp_connection, amqp_param()}]. 
get_sqor_publisher_setting(File)->
    [ConfList] = app_helper:get_config_values(File), 
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
%% Input is an absolute or relative path to an sqor_msg_publisher configuration file.
%% And returns an absolute path to the configuration file.
-spec get_absolute_path(string()) -> string().
get_absolute_path(File) ->
    case filename:pathtype(File) of
	absolute ->
	    File;
	relative ->
	    Config_path = app_config_helper:get_config_path(),
	    Config_dir = filename:dirname(Config_path),
	    filename:absname_join(Config_dir, File)
    end.




   
