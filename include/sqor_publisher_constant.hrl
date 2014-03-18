-ifndef(SQOR_PUBLISHER_CONST_HRL).
-define(SQOR_PUBLISHER_CONST_HRL, true).

-include_lib("sqor_erl_common/include/sqor_constant.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(CONFIG_PATH, <<"sqor_msg_publisher.config">>).

-define(LOG_PATH, <<"sqor_msg_publisher.log">>).

-define(APP_NAME, "sqor_msg_publisher").

-define(to_string(A), type_utils:to_string(A)).

-type amqp_param() :: #'amqp_params_network'{} | #'amqp_params_direct'{}.

-endif.
