-module(gb_conf_env).

-export([proddir/0,
         logdir/0,
	 bindir/0]).

proddir() ->
    case os:getenv("PRODDIR") of 
	false ->
	    error:logger("no PRODDIR exported; bad prod configuration");
	DIR ->
	    DIR
    end.

logdir() ->
    filename:join(proddir(), "log").

bindir() ->
    filename:join(proddir(), "bin").
