%% gb_conf application macros and records.

-record(gb_conf_appconf,{name       :: string(),
                         appname    :: atom(),
                         file       :: string(),
                         version    :: pos_integer(),
                         active     :: boolean(),
                         tag        :: string(),
                         conf       :: [{atom, term()}]
                         }).


-define(gb_conf_get(Name), gb_conf:get_param(?gb_conf_default, Name)).
-define(gb_conf_get(Name, Default), gb_conf:get_param(?gb_conf_default, Name, Default)).
