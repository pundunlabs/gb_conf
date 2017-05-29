%%%-------------------------------------------------------------------
%%% @author erdem <erdem@sitting>
%%% @copyright (C) 2015, erdem
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2015 by erdem <erdem@sitting>
%%%-------------------------------------------------------------------
-module(gb_conf_db).

%% API
-export([create_schema/1,
         create_tables/1]).

-export([transaction/1]).

-export([db_exists/0]).

-include("gb_conf.hrl").

%%%===================================================================
%%% API
%%%===================================================================

db_exists() ->
    case catch mnesia:system_info(tables) of
	Tables when is_list(Tables) ->
	    lists:member(gb_conf_appconf, Tables);
	_ ->
	    false
    end.
%%--------------------------------------------------------------------
%% @doc
%% Create Mnesia schema on given Nodes.
%% @end
%%--------------------------------------------------------------------
-spec create_schema(Nodes :: [node()]) -> ok | {error, Reason :: any()}.
create_schema(Nodes) ->
    mnesia:create_schema(Nodes).

%%--------------------------------------------------------------------
%% @doc
%% Create tables on given Nodes.
%% @end
%%--------------------------------------------------------------------
-spec create_tables(Nodes :: [node()]) -> ok | {error, Reason :: any()}.
create_tables(Nodes) ->
    [create_table(Nodes, T) || T <- [gb_conf_appconf]].

%%--------------------------------------------------------------------
%% @doc
%% Run mnesia activity with access context transaction with given fun
%% @end
%%--------------------------------------------------------------------
-spec transaction(Fun :: fun()) -> {aborted, Reason::term()} | {atomic, ResultOfFun::term()}.
transaction(Fun) ->
    case catch mnesia:activity(transaction, Fun) of
    {'EXIT', Reason} ->
        {error, Reason};
    Result ->
        {atomic, Result}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec create_table(Nodes::[node()], Name::atom()) -> ok | {error, Reason::term()}.
create_table(Nodes, Name) when Name == gb_conf_appconf->
    TabDef = [{access_mode, read_write},
              {attributes, record_info(fields, gb_conf_appconf)},
              {disc_copies, Nodes},
              {load_order, 99},
              {record_name, Name},
              {type, bag}
              ],
    mnesia:create_table(Name, TabDef);
create_table(_, _) ->
    {error, "Unknown table definition"}.


