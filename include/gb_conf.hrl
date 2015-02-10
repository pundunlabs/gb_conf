%% gb_conf application macros and records.

-record(gb_conf_appconf,{name       :: string(),
                         appname    :: atom(),
                         file       :: string(),
                         version    :: pos_integer(),
                         active     :: boolean(),
                         tag        :: string(),
                         conf       :: [{atom, term()}]
                         }).

