%%%-------------------------------------------------------------------
%%% @author erdem aksu <erdem@sitting>
%%% @copyright (C) 2015, Mobile Arts AB
%%% @doc
%%% Growbeard Configuration Management module.
%%% @end
%%% Created :  2 Feb 2015 by erdem <erdem@sitting>
%%%-------------------------------------------------------------------
-module(gb_conf).

%% API
-export([db_init/0]).

-export([get_param/2,
         get_param/3]).

-export([list/0,
	     versions/1,
	     show/1, show/2,
	     load/1,
	     activate/1, activate/2,
         tag/3
	    ]).

-export([notify/0]).

-include("gb_conf.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Initialize the Mnesia database for the node. This function is intended
%% to be used once at configuration phase of the system.
%%
%%--------------------------------------------------------------------
-spec db_init() -> ok | {error, Reason :: any()}.
db_init()->
    %% Read the configuration to get mnesia topology
    PrivDir = code:priv_dir(gb_conf),
    Filename = filename:join(PrivDir, "gb_conf.json"),
    error_logger:info_msg("GB Configuration: Read  ~p~n", [Filename]),
    case read_config(Filename) of
        {ok, Conf} ->
            error_logger:info_msg("read_config(~p) -> ~p.~n", [Filename, {ok, Conf}]),
            MnesiaNodes = [erlang:list_to_atom(N) || N <- find_param(mnesia_nodes, Conf)],
            DbMods = [erlang:list_to_atom(M) || M <- find_param(db_mods, Conf)],
            CS_Result = gb_conf_db:create_schema(MnesiaNodes),
            error_logger:info_msg("gb_conf_db:create_schema(~p) -> ~p~n",
                [MnesiaNodes, CS_Result]),
            application:start(mnesia),
            CT_Result = [Mod:create_tables(MnesiaNodes)|| Mod <- DbMods],
            error_logger:info_msg("Create Tables: ~p~n", [CT_Result]),
            AppConf = #gb_conf_appconf{name = "gb_conf.json",
                                       appname = gb_conf,
                                       file = Filename,
                                       version = undefined,
                                       active = false,
                                       conf = Conf},
            MaxKeeps = find_param(num_of_versions_to_keep, Conf),
            {ok, Version} = store(AppConf, MaxKeeps),
            error_logger:info_msg("Stored configuration of gb_conf app, Version: ~p~n",[Version]),
            do_activate("gb_conf.json", Version),
            Configurations = find_param(configurations, Conf),
            ok = init_load(Configurations, MaxKeeps);
        {error, Reason} ->
            {error, Reason}
    end.
    
%%--------------------------------------------------------------------
%% @doc
%% Notification function to be called when a gb_conf.json document
%% version is activated.
%%--------------------------------------------------------------------
-spec notify() -> ok | {error, Reason::term()}.
notify()->
    error_logger:info_msg("~p:notify/0 called..~n",[?MODULE]),
    Transaction =
        fun()->
            case get_active_conf("gb_conf.json") of
                #gb_conf_appconf{conf = Conf} ->
                    MaxKeep = find_param(num_of_versions_to_keep, Conf),
                    Configurations = find_param(configurations, Conf),
                    ok = check_configurations(Configurations, MaxKeep),
                    ok = check_num_of_versions_to_keep(MaxKeep);
                _ ->    
                    {error, no_active_conf}
            end
        end,

    case gb_conf_db:transaction(Transaction) of
        {atomic, ok} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get param for given JSON file basename and key. If not found, return undefined
%%
%%--------------------------------------------------------------------
-spec get_param(JSON :: string(), Key :: atom())-> Value :: term() | undefined.
get_param(JSON, Key) ->
    get_param(JSON, Key, undefined). 

%%--------------------------------------------------------------------
%% @doc
%% Get param for given JSON file basename and key. If not found, return Default
%%
%%--------------------------------------------------------------------
-spec get_param(JSON :: string(), Key :: atom(), Default :: term())-> Value :: term().
get_param(JSON, Key, Default) ->
    case get_active_conf(JSON) of
        #gb_conf_appconf{conf = Conf} ->
            case find_param(Key, Conf) of
                undefined -> Default;
                Value -> Value
            end;
        _ ->    
            Default
    end.

%%--------------------------------------------------------------------
%% @doc
%% List the configuration files that are loaded on the system
%% @end
%%--------------------------------------------------------------------
-spec list()-> {ok, atom()} | {error, Error :: term()}.
list()->
    case gb_conf_db:transaction(fun()-> mnesia:all_keys(gb_conf_appconf) end) of
        {atomic, AllKeys} ->
            AllKeys;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% List the versions of a configuration file that is loaded on the system.
%% @end
%%--------------------------------------------------------------------
-spec versions(JSON :: string())-> {ok, atom()} | {error, Error :: term()}.
versions(JSON)->
    case gb_conf_db:transaction(fun()-> mnesia:read(gb_conf_appconf, JSON) end) of
        {atomic, AppConfList} ->
            [begin
                #gb_conf_appconf{name = JSON, version = V, active = A} = C,
                {V,A}
             end || C <- AppConfList];
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Show contents of active version of the provided configuration file.
%% @end
%%--------------------------------------------------------------------
-spec show(JSON :: string())-> {ok, atom()} | {error, Error :: term()}.
show(JSON) ->
    get_active_conf(JSON).

%%--------------------------------------------------------------------
%% @doc
%% Show contents of the provided version of the provided configuration file.
%% @end
%%--------------------------------------------------------------------
-spec show(JSON :: string(), Version :: pos_integer()) -> {ok, atom()} |
							  {error, Error :: term()}.
show(JSON, Version) ->
    case gb_conf_db:transaction(fun()-> mnesia:read(gb_conf_appconf, JSON) end) of
        {atomic, AppConfList} ->
            V = 
                case Version of
                    latest -> find_top_version(AppConfList);
                _ -> Version
                end,
            case lists:keyfind(V, #gb_conf_appconf.version, AppConfList) of
                false -> undefined;
                AppConf -> AppConf
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Load the provided configuration file.
%% @end
%%--------------------------------------------------------------------
-spec load(JSON :: string())-> {ok, Version::pos_integer()} | {error, Error :: term()}.
load("gb_conf.json")->
    Transaction =
        fun() ->
            MaxKeeps = get_param("gb_conf.json", num_of_versions_to_keep),
            do_load(gb_conf, "gb_conf.json", MaxKeeps)
        end,
    case gb_conf_db:transaction(Transaction) of
        {atomic, {ok, Version}} ->
            {ok, Version};
        {error, Reason} ->
            {error, Reason}
    end;
load(JSON) ->
    Transaction =
        fun() ->
            Configurations = get_param(configurations, "gb_conf.json"),
            case lists:keyfind(JSON, 2, Configurations) of
                false ->
                    {error, unknown_conf};
                {AppName, JSON} ->
                    MaxKeeps = get_param("gb_conf.json", num_of_versions_to_keep),
                    do_load(AppName, JSON, MaxKeeps)
            end
        end,
    case gb_conf_db:transaction(Transaction) of
        {atomic, {ok, Version}} ->
            {ok, Version};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Activate the latest loaded version of the provided configuration file.
%% @end
%%--------------------------------------------------------------------
-spec activate(JSON :: string())-> ok | {error, Error :: term()}.
activate(JSON)->
    activate(JSON, latest).

%%--------------------------------------------------------------------
%% @doc
%% Activate the provided version of the provided configuration file.
%% @end
%%--------------------------------------------------------------------
-spec activate(JSON :: string(), Version :: pos_integer() | latest)->
                                 ok |
							     {error, Error :: term()}.
activate(JSON, Version)->
    Transaction =
        fun()->
            V = case Version of
                latest -> get_latest_conf_version(JSON);
                _ -> Version
                end,
            case do_activate(JSON, V) of
                ok ->
                    notify_cb(JSON);
                Else ->
                    Else
            end
        end,
    case gb_conf_db:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Add a string tag to the provided JSON Document's provided Version.
%% @end
%%--------------------------------------------------------------------
-spec tag(JSON :: string(), Version :: pos_integer(), Tag :: string())->
                                 ok |
							     {error, Error :: term()}.
tag(JSON, Version, Tag)->
    Transaction =
        fun()->
            do_tag(JSON, Version, Tag)
        end,
    case gb_conf_db:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {error, Reason} ->
            {error, Reason}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Call notify callback module:function if configured in given configuration
%% @end
%%--------------------------------------------------------------------
-spec notify_cb(JSON :: string()) -> ok | {error, Reason::term()}.
notify_cb(JSON) ->
    case get_active_conf(JSON) of
        #gb_conf_appconf{conf = Conf} ->
            case find_value(notify_cb, Conf) of
                undefined -> ok;
                [ModStr, FunStr] ->
                    Mod = erlang:list_to_atom(ModStr),
                    Fun = erlang:list_to_atom(FunStr),
                    erlang:apply(Mod, Fun, [])
            end;
        _ ->    
            {error, no_active_conf}
    end.
%%--------------------------------------------------------------------
%% @doc
%% Read JSON configuration files and return a proplist representation of
%% given configuration.
%% @end
%%--------------------------------------------------------------------
-spec read_config(Filename :: string()) -> {ok, Conf :: term()} | {error, Reason :: any()}.
read_config(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            case catch mochijson2:decode(Binary, [{format, proplist}]) of
                {'EXIT', Reason} ->
                    error_logger:error_msg("file:read_file(~p) -> ~p.~n", [Filename, {'EXIT', Reason}]),
                    {error, {'EXIT', Reason}};
                JsonTypes ->
                    case json_to_term(JsonTypes) of
                        {ok, Conf} ->
                            {ok, Conf};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        {error, Reason} ->
            error_logger:error_msg("file:read_file(~p) -> ~p.~n", [Filename, {error, Reason}])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Convert mochijson2 decoded binaries to strings
%%
%% @end
%%--------------------------------------------------------------------
-spec json_to_term(JsonTypes :: term()) -> {ok, Conf :: [{atom(), term()}]} | {error, Reason :: any()}.
json_to_term(JsonTypes)->
    {ok, json_to_term(JsonTypes, [])}.

json_to_term([], Acc) ->
    lists:reverse(Acc);
json_to_term([{JK, JV} | Rest], Acc)->
    K = erlang:binary_to_atom(JK, latin1),
    V = case JV of
            _ when is_list(JV) ->
                json_to_term(JV, []);
            _ when is_binary(JV) ->
                erlang:binary_to_list(JV);
            _ ->
                JV
         end,
    json_to_term(Rest, [{K,V} | Acc]);
json_to_term([JV | Rest], Acc) when is_binary(JV) ->
    V = erlang:binary_to_list(JV),
    json_to_term(Rest, [V | Acc]);
json_to_term([V | Rest], Acc) when is_integer(V) ->
    json_to_term(Rest, [V | Acc]);
json_to_term([V | Rest], Acc) when is_float(V) ->
    json_to_term(Rest, [V | Acc]).

%%--------------------------------------------------------------------
%% @doc
%% Load given configuration files for application AppName.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_load(AppName::atom(), File::string(), MaxKeeps::pos_integer()) -> ok | {error, Reason::term()}.
do_load(AppName, File, MaxKeeps)->
    case code:priv_dir(AppName) of
        {error, Reason} ->
            error_logger:error_msg("Cannot get priv_dir for app: ~p, Reason: ~p~n",
                                    [AppName, Reason]);
        PrivDir ->
            Filename = filename:join(PrivDir, File),
            case read_config(Filename) of
                {ok, Conf} ->
                    AppConf = #gb_conf_appconf{name = File,
                                               appname = AppName,
                                               file = Filename,
                                               version = undefined,
                                               active = false,
                                               conf = Conf},
                    case store(AppConf, MaxKeeps) of
                        {ok, Version} ->
                            error_logger:info_msg("Stored configuration of gb_conf app, Version: ~p~n",[Version]),
                            {ok, Version};
                        Else ->
                            Else
                    end;
                {error, Reason} ->
                    error_logger:error_msg("Cannot read configuration for app: ~p file: ~p", [AppName, File]),
                    {error, Reason}
            end
    end.
%%--------------------------------------------------------------------
%% @doc
%% Initial load for declared applications and configuration files.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_load(Configurations::[{AppName::atom(), File::string()}], MaxKeeps::pos_integer()) -> ok.
init_load([], _) ->
    ok;
init_load([{AppName, File} | Rest], MaxKeeps) ->
    case code:priv_dir(AppName) of
        {error, Reason} ->
            error_logger:error_msg("Cannot get priv_dir for app: ~p, Reason: ~p~n",
                                    [AppName, Reason]);
        PrivDir ->
            Filename = filename:join(PrivDir, File),
            case read_config(Filename) of
                {ok, Conf} ->
                    AppConf = #gb_conf_appconf{name = File,
                                               appname = AppName,
                                               file = Filename,
                                               version = undefined,
                                               active = false,
                                               conf = Conf},
                    {ok, Version} = store(AppConf, MaxKeeps),
                    error_logger:info_msg("Stored configuration of gb_conf app, Version: ~p~n",[Version]),
                    do_activate(File, Version);
                {error, Reason} ->
                    error_logger:error_msg("Cannot read configuration for app: ~p file: ~p, error: ~p~n", [AppName, File, Reason])
            end
        end,
        init_load(Rest, MaxKeeps).

%%--------------------------------------------------------------------
%% @doc
%% Find the value for the given Key in application configuration term Conf.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_value(Key::atom(), Conf::[{atom(), term()}]) -> Value::term() | undefined.
find_value(Key, Conf)->
    proplists:get_value(Key, Conf).

%%--------------------------------------------------------------------
%% @doc
%% Find the parameter value for the given Key in application configuration term Conf.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_param(Key::atom(), Conf::[{atom(), term()}]) -> Value::term() | undefined.
find_param(Key, Conf)->
    case proplists:get_value(params, Conf) of
        undefined ->
            proplists:get_value(Key, Conf);
        PList when is_list(PList) ->
            proplists:get_value(Key, PList)
    end.

-spec store(AppConf :: #gb_conf_appconf{}, MaxKeep::pos_integer()) -> {ok, Version :: pos_integer()} | {error, Reason::term()}.
store(#gb_conf_appconf{name = Name} = AppConf, MaxKeep) when is_integer(MaxKeep)->
    Transaction =
    fun () ->
        AppConfList = mnesia:read(gb_conf_appconf, Name),
        TopVersion = find_top_version(AppConfList),
        BottomVersion = find_bottom_version(AppConfList),
        Version = TopVersion+1,
        StoreAppConf =  AppConf#gb_conf_appconf{version = Version},
        TotalKeptVersions = Version-BottomVersion,

        OldestConf = find_version_conf(AppConfList, BottomVersion),
        case TotalKeptVersions of
            MaxKeep ->
                case OldestConf of
                    #gb_conf_appconf{active = true} ->
                        mnesia:abort("Cannot delete oldest configuration that is active!");
                    _ ->
                        mnesia:delete_object(OldestConf),
                        mnesia:write(StoreAppConf)
                end;
            _ when TotalKeptVersions < MaxKeep ->
                mnesia:write(StoreAppConf)
        end,
        {ok, Version}
    end,
    case gb_conf_db:transaction(Transaction) of
        {atomic, {ok, V}} ->
            {ok, V};
        {error, Reason} ->
            {error, Reason}
    end.

-spec do_activate(Name:: string(), Version::pos_integer()) -> ok | {error, Reason::term()}.
do_activate(Name, Version) when is_integer(Version)->
    Transaction =
        fun() ->
            AppConfList = mnesia:read(gb_conf_appconf, Name),
            case find_version_conf(AppConfList, Version) of
                undefined ->
                    {error, invalid_version};
                InactiveConf ->
                    case find_active_conf(AppConfList) of
                            undefined -> ok;
                            ActiveConf -> 
                                mnesia:delete_object(ActiveConf),
                                mnesia:write(ActiveConf#gb_conf_appconf{active = false})
                    end,
                mnesia:delete_object(InactiveConf),
                mnesia:write(InactiveConf#gb_conf_appconf{active = true})
            end
        end,
    case gb_conf_db:transaction(Transaction) of
        {atomic, ok} -> ok;
        Else -> Else
    end. 

-spec do_tag(Name:: string(), Version::pos_integer(), Tag::string()) -> ok | {error, Reason::term()}.
do_tag(Name, Version, Tag) when is_integer(Version),
                                is_list(Tag)->
    AppConfList = mnesia:read(gb_conf_appconf, Name),
    case find_version_conf(AppConfList, Version) of
        undefined ->
            {error, invalid_version};
        Conf ->
            mnesia:delete_object(Conf),
            mnesia:write(Conf#gb_conf_appconf{tag = Tag})
    end. 

-spec find_bottom_version(AppConfList :: [#gb_conf_appconf{}]) -> Version :: integer().
find_bottom_version(AppConfList)->
    find_bottom_version(AppConfList, undefined).

find_bottom_version([], undefined)->
    0;
find_bottom_version([], Version)->
    Version;
find_bottom_version([#gb_conf_appconf{version = V} | Rest], BV)->
    NBV =
        case BV of
            undefined -> V;
            _ ->  erlang:min(V,BV)
        end,
    find_bottom_version(Rest, NBV).

-spec find_top_version(AppConfList :: [#gb_conf_appconf{}]) -> Version :: integer().
find_top_version(AppConfList)->
    find_top_version(AppConfList, 0).

find_top_version([], Version)->
    Version;
find_top_version([#gb_conf_appconf{version = V} | Rest], HV)->
    NHV = erlang:max(V,HV),
    find_top_version(Rest, NHV).

-spec find_active_conf(AppConfList :: [#gb_conf_appconf{}]) -> #gb_conf_appconf{} | undefined.
find_active_conf([])->
    undefined;
find_active_conf([#gb_conf_appconf{active = true} = Active| _])->
    Active;
find_active_conf([#gb_conf_appconf{active = false}| Rest])->
    find_active_conf(Rest).

-spec find_version_conf(AppConfList :: [#gb_conf_appconf{}], Version :: pos_integer) -> #gb_conf_appconf{} | undefined.
find_version_conf([], _)->
    undefined;
find_version_conf([#gb_conf_appconf{version = V} = Active| _], V)->
    Active;
find_version_conf([_| Rest], V)->
    find_version_conf(Rest, V).

-spec get_active_conf(JSON::string())-> #gb_conf_appconf{} | undefined | {error, Reason::term()}.
get_active_conf(JSON) ->
    case gb_conf_db:transaction(fun()-> mnesia:read(gb_conf_appconf, JSON) end) of
        {atomic, AppConfList} ->
            case find_active_conf(AppConfList) of
                undefined ->
                    undefined;
                AppConf ->
                    AppConf
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_latest_conf_version(JSON::string())-> #gb_conf_appconf{} | undefined | {error, Reason::term()}.
get_latest_conf_version(JSON) ->
    case gb_conf_db:transaction(fun()-> mnesia:read(gb_conf_appconf, JSON) end) of
        {atomic, AppConfList} ->
            find_top_version(AppConfList);
        {error, Reason} ->
            {error, Reason}
    end.

-spec check_configurations(Configurations::[{AppName::atom(), File::string()}],
        MaxKeeps::pos_integer()) -> ok.
check_configurations(Configurations, MaxKeeps) ->
    AllConf = mnesia:all_keys(gb_conf_appconf),
    ConfsToRemove = find_removed_configurations(AllConf, Configurations, []),
    ConfsToAdd = find_added_configurations(Configurations, AllConf, []),
    [mnesia:delete({gb_conf_appconf, Conf}) || Conf <- ConfsToRemove],
    ok = init_load(ConfsToAdd, MaxKeeps).

-spec find_removed_configurations(AllConf::[string()],
        Configurations::[{AppName::atom(), File::string()}],
        Acc::[string()])-> ok.
find_removed_configurations([], _, Acc) ->
    Acc;
find_removed_configurations(["gb_conf.json" | Rest], Configurations, Acc) ->
    find_removed_configurations(Rest, Configurations, Acc);
find_removed_configurations([AppConf | Rest], Configurations, Acc) ->
    case lists:keymember(AppConf, 2, Configurations) of
        true ->
            find_removed_configurations(Rest, Configurations, Acc);
        false ->
            find_removed_configurations(Rest, Configurations, [AppConf | Acc])
    end.

-spec find_added_configurations(Configurations::[{AppName::atom(), File::string()}],
        AllConf::[string()],
        Acc::[{AppName::atom(), File::string()}])-> ok.
find_added_configurations([], _, Acc) ->
    Acc;
find_added_configurations([{AppName, File} | Rest], AllConf, Acc) ->
    case lists:member(File, AllConf) of
        true ->
            find_added_configurations(Rest, AllConf, Acc);
        false ->
            find_added_configurations(Rest, AllConf, [{AppName, File} | Acc])
    end.

-spec check_num_of_versions_to_keep(MaxKeep::pos_integer()) -> ok.
check_num_of_versions_to_keep(MaxKeep) -> 
    AppConfList = mnesia:read(gb_conf_appconf, "gb_conf.json"),
    Length = length(AppConfList),
    if Length =< MaxKeep ->
            ok;
        true ->
            delete_versions(MaxKeep-Length)
    end.

-spec delete_versions(Num::pos_integer()) -> ok.
delete_versions(Num) ->
   AllConf = mnesia:all_keys(gb_conf_appconf),
   delete_versions(Num, AllConf).

-spec delete_versions(Num::pos_integer(), AllConf::[string()]) -> ok.
delete_versions(_Num, []) ->
    ok;
delete_versions(Num, [JSON | Rest]) ->
    delete_oldest_n_versions(Num, JSON),
    delete_versions(Num, Rest).

-spec delete_oldest_n_versions(Num::pos_integer(), JSON::string()) -> ok.
delete_oldest_n_versions(Num, JSON) ->
    AppConfList = mnesia:read(gb_conf_appconf, JSON),
    Sorted = lists:keysort(#gb_conf_appconf.version, AppConfList),
    delete_n_if_inactive(Num, Sorted).

-spec delete_n_if_inactive(Num::non_neg_integer(), AppConfList::[#gb_conf_appconf{}]) -> ok.
delete_n_if_inactive(0, _) ->
    ok;
delete_n_if_inactive(N, [#gb_conf_appconf{active = true} | Rest]) ->
    delete_n_if_inactive(N, Rest);
delete_n_if_inactive(N, [Del = #gb_conf_appconf{active = false} | Rest]) ->
    mnesia:delete_object(Del),
    delete_n_if_inactive(N-1, Rest).
    
