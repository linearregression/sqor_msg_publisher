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


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?INFO_MSG("Starting ~s: start/2~n", [?MODULE]),    
    sqor_msg_publisher_sup:start_link().

stop(_State) ->
    ?INFO_MSG("Stopping ~s: stop/1~n", [?MODULE]),    
    ?INFO_MSG("Stopped ~s: stop/1~n", [?MODULE]),    
    ok.


