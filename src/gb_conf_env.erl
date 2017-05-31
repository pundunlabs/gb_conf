%%%-------------------------------------------------------------------
%%% @author jonas falkevik
%%% @copyright (C) 2015, Mobile Arts AB
%%% @doc
%%% Growbeard Configuration Management module.
%%% @end
%%% Created :  26 Feb 2015 by jonas falkevik
%%%-------------------------------------------------------------------

-module(gb_conf_env).

-export([proddir/0,
         logdir/0,
	 bindir/0]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Get production root directory.
-spec proddir() -> list().
proddir() ->
    Env = gb_conf:get_param("gb_conf.yaml", prod_dir_env, "PRODDIR"),
    case os:getenv(Env) of
	false ->
	    throw({no_such_env, Env});
	DIR ->
	    DIR
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get production log directory.
-spec logdir() -> list().
logdir() ->
    filename:join(proddir(), "log").

%%--------------------------------------------------------------------
%% @doc
%% Get production bin directory.
-spec bindir() -> list().
bindir() ->
    filename:join(proddir(), "bin").
