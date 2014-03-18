-ifndef(SQOR_MSG_PUBLISHER_CONFIG_HRL).
-define(SQOR_MSG_PUBLISHER_CONFIG_HRL, true).
-include_lib("amqp_client/include/amqp_client.hrl").

-record(sqor_supported_events_config, {
    event_modules = [] 
}).


-record(sqor_msg_publisher_config, {
   amqp_connection = <<"">>,
   exchange = #exchange{},
   queue = #amqqueue{}

}).

-endif.
