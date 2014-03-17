 -define(PRINT(Format, Args), io:format(Format, Args)).
-define(LAGER, true).

-ifdef(LAGER).
-compile([{parse_transform, lager_transform}]).

-define(DEBUG_MSG(Format, Args),
	lager:debug(Format, Args)).

-define(INFO_MSG(Format, Args),
	lager:info(Format, Args)).

-define(WARNING_MSG(Format, Args),
	lager:warning(Format, Args)).

-define(ERROR_MSG(Format, Args),
	lager:error(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
	lager:critical(Format, Args)).

-else.

-define(DEBUG_MSG(Format, Args),
	error_logger:debug_msg(Format, Args)).

-define(INFO_MSG(Format, Args),
	error_logger:info_msg(Format, Args)).

-define(WARNING_MSG(Format, Args),
	error_logger:warning_msg(Format, Args)).

-define(ERROR_MSG(Format, Args),
	error_logger:error_msg(Format, Args)).

% sasl error_logger has not critical level
-define(CRITICAL_MSG(Format, Args),
	error_logger:error_msg(Format, Args)).
-endif.
