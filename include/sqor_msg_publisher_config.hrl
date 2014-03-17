-ifndef(SQOR_MSG_PUBLISHER_CONFIG_HRL).
-define(SQOR_MSG_PUBLISHER_CONFIG_HRL, true).
-include_lib("amqp_client/include/amqp_client.hrl").

-record(sqor_message_processor_config, {
   enrich_payload = undefined,
   enrich_payload_opts = [],
   transform_payload = undefined,
   transform_payload_opts = [],
   filter_payload = undefined,
   filter_payload_opts = []
 
}).

-record(sqor_msg_pipeline_config, {
    message_processor = [#sqor_message_processor{}]    
        
}).

-record(sqor_msg_publisher_config, {
   amqp_connection = <<"">>,
   exchange = #exchange{},
   queue = #amqqueue{}

}).

-endif.
