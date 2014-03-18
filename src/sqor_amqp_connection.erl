-module(sqor_amqp_connection).

%-include("sqor_consumer_constant.hrl").

-export([get_connection_setting/1, get_amqp_params/1,
         get_exchange_setting/1,
         get_queue_setting/1,
         get_clone_queue_setting/2,
         get_routing_key/1,
         get_queue_bind/3,
         get_consumer_setting/1, 
         channel_setup/1, exchange_setup/2, queue_setup/2, queue_bind/4
]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("sqor_erl_common/include/app_logger.hrl").
-include_lib("sqor_erl_common/include/sqor_constant.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(to_string(A), type_utils:to_string(A)).

%% @doc Read connection settings to rabbitmq
%% @end
-spec get_connection_setting(list()) ->#'amqp_params_network'{} | #'amqp_params_direct'{}.
get_connection_setting(ConfList) ->
    ?INFO_MSG("[~s]: Get Connection String ~n",[?MODULE]),
    {ok, AmqpConfList} = ?parse(amqp_connection, ConfList, []),
    {ok, ConnectionString} = ?parse(connection_string, AmqpConfList, <<"">>),
    get_amqp_params(ConnectionString).

%    ConnectionString0 = type_utils:to_string(ConnectionString),
%    ?INFO_MSG("[~s]: Read Connection String: ~s ~n",[?MODULE, ConnectionString0]),
%    {ok, Settings} = amqp_uri:parse(ConnectionString0), 
%    Settings.

get_amqp_params(AmqpString) ->
    AmqpString0 = type_utils:to_string(AmqpString),
    ?INFO_MSG("[~s]: Get AMQP Connection String: ~s ~n",[?MODULE, AmqpString0]),
    {ok, Settings} = amqp_uri:parse(AmqpString0), 
    Settings.


-spec get_exchange_setting(list())-> #'exchange.declare'{}.
get_exchange_setting(ConfList)->
    ?INFO_MSG("[~s]: Get Exchange Setting: ~s ~n",[?MODULE, ?to_string(ConfList)]),
    {ok, ExchangeConfList} = ?parse(amqp_exchange, ConfList, []),
    ?INFO_MSG("[~s]: Read Exchange Setting: ~s ~n",[?MODULE, ?to_string(ExchangeConfList)]),
    {ok, Ticket}       = ?parse(ticket,ExchangeConfList,0),
    {ok, Exchange}     = ?parse(exchange,ExchangeConfList, <<"">>),
    {ok, Type}         = ?parse(type,ExchangeConfList,<<"direct">>),
    {ok, Passive}      = ?parse(passive,ExchangeConfList,false),
    {ok, Durable}      = ?parse(durable,ExchangeConfList,false),
    {ok, AutoDelete}   = ?parse(auto_delete,ExchangeConfList,false),
    {ok, Internal}     = ?parse(internal,ExchangeConfList,false),
    {ok, NoWait}       = ?parse(nowait,ExchangeConfList,false),
    {ok, Arguments}    = ?parse(arguments,ExchangeConfList,[]),
    #'exchange.declare'{
                ticket      = Ticket,
                exchange    = type_utils:to_binary(Exchange),
                type        = type_utils:to_binary(Type),
                passive     = Passive,
                durable     = Durable,
                auto_delete = AutoDelete,
                internal    = Internal,
                nowait      = NoWait,
                arguments   = Arguments
                }.

-spec get_queue_setting(list())-> #'queue.declare'{}.
get_queue_setting(ConfList)->
    {ok, AmqpQueueConfList} = ?parse(amqp_queue, ConfList, []),
    {ok, QTicket} = ?parse(qticket, AmqpQueueConfList, 0),
    {ok, Queue} = ?parse(queue, AmqpQueueConfList, <<"">>),
    {ok, QPassive} = ?parse(qpassive, AmqpQueueConfList, false),
    {ok, QDurable} = ?parse(qdurable, AmqpQueueConfList, false),
    {ok, QExclusive} = ?parse(qexclusive, AmqpQueueConfList, false),
    {ok, QAutoDelete} = ?parse(qauto_delete, AmqpQueueConfList, false),
    {ok, QNoWait} = ?parse(qnowait, AmqpQueueConfList, false),
    {ok, QArguments} = ?parse(qarguments, AmqpQueueConfList, []),

    #'queue.declare'{
               ticket = QTicket,
               queue = Queue,
               passive     = QPassive,
               durable     = QDurable,
               auto_delete = QAutoDelete,
               exclusive   = QExclusive,
               nowait      = QNoWait,
               arguments = QArguments}.

-spec get_clone_queue_setting(list(), integer())-> [#'queue.declare'{}].
get_clone_queue_setting(ConfList, HowMany) when is_integer(HowMany), HowMany >=0 ->
   BaseSetting = get_queue_setting(ConfList),   
   get_clone_queue_setting(BaseSetting, [BaseSetting], HowMany).

-spec get_clone_queue_setting(list(), list(), integer())-> [#'queue.declare'{}].
get_clone_queue_setting(BaseSetting, [H|_], I) ->
   NewName = index_queue_name(BaseSetting#'queue.declare'.queue, I),
   NewQueue = BaseSetting#'queue.declare'{queue = NewName},
   get_clone_queue_setting(BaseSetting, [H|NewQueue], I-1);


%-spec clone_queue_setting(list(), list(), integer())-> [#'queue.declare'{}].
get_clone_queue_setting(_, _, 0)-> [#'queue.declare'{}].  

-spec index_queue_name(binary(), binary())-> binary().
index_queue_name(Name, I) when is_binary(Name) ->
   <<Name/binary,I/binary>>.

-spec get_routing_key(list()) -> binary().
get_routing_key(ConfList)->
   {ok, QueueConfList} = ?parse(amqp_queue, ConfList, []),
   ?parse(routing_key,QueueConfList, <<"">>). 
  
-spec get_queue_bind(binary(), binary(), binary())->#'queue.bind'{}.
get_queue_bind(Queue, Exchange, RoutingKey)->
   #'queue.bind'{
        queue = Queue,
        exchange = Exchange,
        routing_key = RoutingKey
        }.

-spec get_consumer_setting(list())-> #'basic.consume'{}.
get_consumer_setting(ConfList) ->
   {ok, ConsumerConfList} = ?parse(consumer_queue, ConfList, []),
   {ok, Consumer_tag} = ?parse(consumer_queue, ConsumerConfList, <<"">>),
   {ok, Queue} = ?parse(queue, ConsumerConfList, <<"">>),
   {ok, Ticket} = ?parse(ticket, ConsumerConfList, 0),
   {ok, NoLocal} = ?parse(no_local, ConsumerConfList, false),
   {ok, No_ack} = ?parse(no_ack, ConsumerConfList, false),
   {ok, Exclusive} = ?parse(exclusive, ConsumerConfList, false),
   {ok, Nowait} = ?parse(nowait, ConsumerConfList,false),
   {ok, Arguments} = ?parse(arguments, ConsumerConfList,[]),
   #'basic.consume'{
        ticket = Ticket,
        queue = Queue,
        no_local = NoLocal,
        no_ack = No_ack,
        exclusive = Exclusive,
        nowait = Nowait,
        consumer_tag = Consumer_tag,
        arguments= Arguments
   }.

-spec channel_setup(list()) -> {ok, pid()} | {error, term()}.
channel_setup(ConfList)->
    ?INFO_MSG("Setting up communication: ~s",[ConfList]),
    {ok, AmqpParams} = sqor_consumer_connection:get_connection_setting(ConfList), 
    {ok, Connection} = amqp_connection:start(AmqpParams),
    ?INFO_MSG("AMQP connection established with pid: ~s",[Connection]),
    ?INFO_MSG("Setting up channel: ~s",[AmqpParams]), 
    {ok, Channel} = amqp_connection:open_channel(Connection),
    ?INFO_MSG("AMQP channel established with pid: ~s",[Channel]),
    {ok, Channel, AmqpParams}.

-spec exchange_setup(pid(), list()) -> {ok, pid()} | {error, term()}.
exchange_setup(Channel, ConfList)->
    ?INFO_MSG("Setting up Exchange if not exist, reuse if exist: ~s", [ConfList]),
    ExchangeDeclare = sqor_consumer_connection:get_exchange_setting(ConfList),
    {'exchange.declare_ok'}  = amqp_channel:call(Channel, ExchangeDeclare), 
    ?INFO_MSG("Exchange declared: ~s",
  			['exchange_declare_ok']),  
    ExchangeDeclare.

-spec queue_setup(pid(), list()) -> {ok, pid()} | {error, term()}.  
queue_setup(Channel, ConfList)->
    ?INFO_MSG("Setting up Queue if not exist, reuse if exist: ~s", [ConfList]),  
    QueueDeclare = sqor_consumer_connection:get_queue_setting(ConfList),
    {'queue.declare_ok', _, _, _} = amqp_channel:call(Channel, QueueDeclare),
    ?INFO_MSG("Queue declared: ~s",
  			['queue_declare_ok']),
    QueueDeclare.

-spec queue_bind(pid(), binary(), binary(), binary()) -> {ok, pid()} | {error, term()}.
queue_bind(Channel, Queue, Exchange, RoutingKey) ->
    QueueBind = sqor_consumer_connection:get_queue_bind(Queue, Exchange, RoutingKey),
    {'queue.bind_ok'}  = amqp_channel:call(Channel, QueueBind),
    QueueBind.




