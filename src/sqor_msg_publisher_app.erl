-module(sqor_msg_publisher_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    sqor_msg_publisher_sup:start_link().

stop(_State) ->
    ok.
