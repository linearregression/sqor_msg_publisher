-ifndef(APP_CONFIG_TEMPLATE_HRL).
-define(APP_CONFIG_TEMPLATE_HRL, true).
-record(sample_app, {sample_key, sample_val}).

-export_records([sample_app]).
-define(APPCONF_REC, 'sample_app').

-endif.
