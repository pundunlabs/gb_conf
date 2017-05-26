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
         get_param/3,
	 get_all_params/1]).

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
    Filename = filename:join(PrivDir, "gb_conf.yaml"),
    error_logger:info_msg("GB Configuration: Read  ~p~n", [Filename]),
    case read_config(Filename) of
        {ok, [Conf|_]} ->
            error_logger:info_msg("read_config(~p) -> ~p.~n", [Filename, {ok, Conf}]),
            MnesiaNodes = case find_param("mnesia_nodes", Conf) of
			    [] ->
				[node()];
			    Nodes ->
				[erlang:list_to_atom(N) || N <- Nodes]
			  end,
            DbMods = [erlang:list_to_atom(M) || M <- find_param("db_mods", Conf)],
            CS_Result = gb_conf_db:create_schema(MnesiaNodes),
            error_logger:info_msg("gb_conf_db:create_schema(~p) -> ~p~n",
                [MnesiaNodes, CS_Result]),
            application:start(mnesia),
	    db_init_(CS_Result, MnesiaNodes, DbMods, Filename, Conf);
	{ok, []} ->
            error_logger:error_msg("Empty configuration file: ~p", [Filename]),
	    {error, "no_yaml_doc"};
        {error, Reason} ->
            {error, Reason}
    end.
-spec db_init_(CS_Result :: ok | {error, Reason :: term()},
	       MnesiaNodes :: [node()], DbMods :: [atom()],
	       Filename :: string(),
	       Conf :: term()) ->
    ok | {error, Reason :: any()}.

db_init_({error,{_N,{already_exists,_Node}}}, _, _, _, _) ->
    ok;
db_init_(ok, MnesiaNodes, DbMods, Filename, Conf) ->
    CT_Result = [Mod:create_tables(MnesiaNodes)|| Mod <- DbMods],
    error_logger:info_msg("Create Tables: ~p~n", [CT_Result]),
    AppConf = #gb_conf_appconf{name = "gb_conf.yaml",
                               appname = "gb_conf",
                               file = Filename,
                               version = undefined,
                               active = false,
                               conf = Conf},
    MaxKeeps = find_param("num_of_versions_to_keep", Conf),
    {ok, Version} = store(AppConf, MaxKeeps),
    error_logger:info_msg("Stored configuration of gb_conf app, Version: ~p~n",
			  [Version]),
    do_activate("gb_conf.yaml", Version),
    Configurations = find_param("configurations", Conf),
    ok = init_load(Configurations, MaxKeeps).

%%--------------------------------------------------------------------
%% @doc
%% Notification function to be called when a gb_conf.yaml document
%% version is activated.
%%--------------------------------------------------------------------
-spec notify() -> ok | {error, Reason::term()}.
notify()->
    error_logger:info_msg("~p:notify/0 called..~n",[?MODULE]),
    Transaction =
        fun()->
            case get_active_conf("gb_conf.yaml") of
                #gb_conf_appconf{conf = Conf} ->
                    MaxKeep = find_param("num_of_versions_to_keep", Conf),
                    Configurations = find_param("configurations", Conf),
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
%% Get param for given YAML file basename and key. If not found, return undefined
%%
%%--------------------------------------------------------------------
-spec get_param(File :: string(), Key :: string() | atom())->
    Value :: term() | undefined.
get_param(File, Key) when is_atom(Key) ->
    get_param(File, atom_to_list(Key));
get_param(File, Key) ->
    get_param(File, Key, undefined). 

%%--------------------------------------------------------------------
%% @doc
%% Get param for given YAML file basename and key. If not found, return Default
%%
%%--------------------------------------------------------------------
-spec get_param(File :: string(),
		Key :: string() | atom(),
		Default :: term()) -> Value :: term().
get_param(File, Key, Default) when is_atom(Key) ->
    get_param(File, atom_to_list(Key), Default);
get_param(File, Key, Default) ->
    case get_active_conf(File) of
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
%% Get all params for given YAML file basename. If not found, return []
%%
%%--------------------------------------------------------------------

get_all_params(File) ->
    case get_active_conf(File) of
        #gb_conf_appconf{conf = Conf} ->
            Conf;
        _ ->
            []
    end.
%%--------------------------------------------------------------------
%% @doc
%% List the configuration files that are loaded on the system
%% @end
%%--------------------------------------------------------------------
-spec list()-> {ok, string()} | {error, Error :: term()}.
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
-spec versions(File :: string())->
    [{string(), term()}] | {error, Error :: term()}.
versions(File)->
    case gb_conf_db:transaction(fun()-> mnesia:read(gb_conf_appconf, File) end) of
        {atomic, AppConfList} ->
            [begin
                #gb_conf_appconf{name = File, version = V, active = A} = C,
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
-spec show(File :: string())-> {ok, atom()} | {error, Error :: term()}.
show(File) ->
    get_active_conf(File).

%%--------------------------------------------------------------------
%% @doc
%% Show contents of the provided version of the provided configuration file.
%% @end
%%--------------------------------------------------------------------
-spec show(File :: string(), Version :: pos_integer()) ->
    {ok, string()} | {error, Error :: term()}.
show(File, Version) ->
    case gb_conf_db:transaction(fun()-> mnesia:read(gb_conf_appconf, File) end) of
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
-spec load(File :: string())->
    {ok, Version::pos_integer()} | {error, Error :: term()}.
load("gb_conf.yaml")->
    Transaction =
        fun() ->
            MaxKeeps = get_param("gb_conf.yaml", "num_of_versions_to_keep"),
            do_load("gb_conf", "gb_conf.yaml", MaxKeeps)
        end,
    case gb_conf_db:transaction(Transaction) of
        {atomic, {ok, Version}} ->
            {ok, Version};
        {error, Reason} ->
            {error, Reason}
    end;
load(File) ->
    Transaction =
        fun() ->
            Configurations = get_param("gb_conf.yaml", "configurations"),
            case lists:keyfind(File, 2, Configurations) of
                false ->
                    {error, unknown_conf};
                {AppName, File} ->
                    MaxKeeps = get_param("gb_conf.yaml", "num_of_versions_to_keep"),
                    do_load(AppName, File, MaxKeeps)
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
-spec activate(File :: string())-> ok | {error, Error :: term()}.
activate(File)->
    activate(File, latest).

%%--------------------------------------------------------------------
%% @doc
%% Activate the provided version of the provided configuration file.
%% @end
%%--------------------------------------------------------------------
-spec activate(File :: string(), Version :: pos_integer() | latest) ->
    ok | {error, Error :: term()}.
activate(File, Version)->
    Transaction =
        fun()->
            V = case Version of
                latest -> get_latest_conf_version(File);
                _ -> Version
                end,
            case do_activate(File, V) of
                ok ->
                    notify_cb(File);
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
%% Add a string tag to the provided YAML File's provided Version.
%% @end
%%--------------------------------------------------------------------
-spec tag(File :: string(), Version :: pos_integer(), Tag :: string())->
    ok | {error, Error :: term()}.
tag(File, Version, Tag)->
    Transaction =
        fun()->
            do_tag(File, Version, Tag)
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
-spec notify_cb(File :: string()) -> ok | {error, Reason::term()}.
notify_cb(File) ->
    case get_active_conf(File) of
        #gb_conf_appconf{conf = Conf} ->
            case find_value("notify_cb", Conf) of
                undefined -> ok;
		[] -> ok;
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
%% Read YAML configuration files and return a proplist representation
%% of given configuration.
%% @end
%%--------------------------------------------------------------------
-spec read_config(Filename :: string()) -> {ok, Conf :: term()} | {error, Reason :: any()}.
read_config(Filename) ->
    case catch yamerl_constr:file(Filename) of
	{'EXIT', Reason} ->
	    error_logger:error_msg("file:read_file(~p) -> ~p.~n", [Filename, {'EXIT', Reason}]),
	    {error, {'EXIT', Reason}};
        YamlDocs ->
		{ok, YamlDocs}
    end.

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
                {ok, [Conf|_]} ->
                    AppConf = #gb_conf_appconf{name = File,
                                               appname = AppName,
                                               file = Filename,
                                               version = undefined,
                                               active = false,
                                               conf = Conf},
                    case store(AppConf, MaxKeeps) of
                        {ok, Version} ->
                            error_logger:info_msg("Stored configuration of ~p app, Version: ~p~n",[File, Version]),
                            {ok, Version};
                        Else ->
                            Else
                    end;
		{ok, []} ->
                    error_logger:error_msg("Empty configuration file for app: ~p file: ~p", [AppName, File]),
		    {error, "no_yaml_doc"};
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
                {ok, [Conf|_]} ->
                    AppConf = #gb_conf_appconf{name = File,
                                               appname = AppName,
                                               file = Filename,
                                               version = undefined,
                                               active = false,
                                               conf = Conf},
                    {ok, Version} = store(AppConf, MaxKeeps),
                    error_logger:info_msg("Stored configuration of ~p app, Version: ~p~n",[File, Version]),
                    do_activate(File, Version);
		{ok, []} ->
                    error_logger:error_msg("Empty configuration file for app: ~p file: ~p", [AppName, File]);
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
    case proplists:get_value("params", Conf) of
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

-spec get_active_conf(File::string())-> #gb_conf_appconf{} | undefined | {error, Reason::term()}.
get_active_conf(File) ->
    case gb_conf_db:transaction(fun()-> mnesia:read(gb_conf_appconf, File) end) of
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

-spec get_latest_conf_version(File::string())-> #gb_conf_appconf{} | undefined | {error, Reason::term()}.
get_latest_conf_version(File) ->
    case gb_conf_db:transaction(fun()-> mnesia:read(gb_conf_appconf, File) end) of
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
find_removed_configurations(["gb_conf.yaml" | Rest], Configurations, Acc) ->
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
    AppConfList = mnesia:read(gb_conf_appconf, "gb_conf.yaml"),
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
delete_versions(Num, [File | Rest]) ->
    delete_oldest_n_versions(Num, File),
    delete_versions(Num, Rest).

-spec delete_oldest_n_versions(Num::pos_integer(), File::string()) -> ok.
delete_oldest_n_versions(Num, File) ->
    AppConfList = mnesia:read(gb_conf_appconf, File),
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

