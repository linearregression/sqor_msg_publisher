-ifndef(SQOR_CONST_HRL).
-define(SQOR_CONST_HRL, true).

%% Name of the environment variables, may change per application
-define(SQOR_CONFIG_PATH_ENV, "SQOR_CONFIG_PATH").
-define(parse(Key, Opts, DEFAULT), app_helper:get_prop_or_default(Key, Opts, DEFAULT)).

-type error():: tuple().
-type ok_or_error() :: {atomic, ok} | ok | {error, any()}.
-type filename():: file:name_all().

-endif.


