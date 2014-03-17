-ifndef(SQOR_MSG_PUBLISHER_CONFIG_HRL).
-define(SQOR_MSG_PUBLISHER_CONFIG_HRL, true).
-include_lib("amqp_client/include/amqp_client.hrl").

-record(sqor_message_processor, {
  name = undefined,
  opts = []     
}).


-record(soqr_publisher_config, {
   amqp_connection = <<"">>,
   exchange = #exchange{},
   queue = #amqqueue{},
   message_processor = [#pre_publish_processor{}]
}).

-endif.
