%%%-------------------------------------------------------------------
%%% @author erdem <erdem@sitting>
%%% @copyright (C) 2015, erdem
%%% @doc
%%%
%%% @end
%%% Created :  2 Feb 2015 by erdem <erdem@sitting>
%%%-------------------------------------------------------------------
-module(gb_conf).

-behaviour(gen_server).

%% API
-export([db_init/0]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([get_param/2]).

-export([list/0,
	     versions/1,
	     show/1, show/2,
	     load/1,
	     activate/1, activate/2
	    ]).

-define(SERVER, ?MODULE).

-include("gb_conf.hrl").

-record(state, {}).

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
            AppConf = #gb_conf_appconf{appname = gb_conf,
                                       file = Filename,
                                       version = undefined,
                                       active = false,
                                       conf = Conf},
            MaxKeeps = find_param(num_of_versions_to_keep, Conf),
            {ok, Version} = store(AppConf, MaxKeeps),
            error_logger:info_msg("Stored configuration of gb_conf app, Version: ~p~n",[Version]),
            do_activate(gb_conf, Version);
        {error, Reason} ->
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Get param for given application and key.
%%
%%--------------------------------------------------------------------
-spec get_param(Application :: atom(), Key :: atom()) -> Value :: term() | undefined.
get_param(_,_) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% Starts the gb_conf server with given path to ect folder where gb_conf.cfg is placed.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
					  ignore |
					  {error, Error :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% List the configuration files that are loaded on the system
%% @end
%%--------------------------------------------------------------------
-spec list()-> {ok, atom()} | {error, Error :: term()}.
list()->
    gen_server:call(?SERVER, list).

%%--------------------------------------------------------------------
%% @doc
%% List the versions of a configuration file that is loaded on the system.
%% @end
%%--------------------------------------------------------------------
-spec versions(JSON :: string())-> {ok, atom()} | {error, Error :: term()}.
versions(JSON)->
    gen_server:call(?SERVER, {versions, JSON}).

%%--------------------------------------------------------------------
%% @doc
%% Show contents of the provided configuration file.
%% @end
%%--------------------------------------------------------------------
-spec show(JSON :: string())-> {ok, atom()} | {error, Error :: term()}.
show(JSON) ->
        gen_server:call(?SERVER, {show, JSON, latest}).

%%--------------------------------------------------------------------
%% @doc
%% Show contents of the provided version of the provided configuration file.
%% @end
%%--------------------------------------------------------------------
-spec show(JSON :: string(), Version :: pos_integer()) -> {ok, atom()} |
							  {error, Error :: term()}.
show(JSON, Version) ->
        gen_server:call(?SERVER, {versions, JSON, Version}).

%%--------------------------------------------------------------------
%% @doc
%% Load the provided configuration file.
%% @end
%%--------------------------------------------------------------------
-spec load(JSON :: string())-> {ok, atom()} | {error, Error :: term()}.
load(JSON)->
    gen_server:call(?SERVER, {load, JSON}).

%%--------------------------------------------------------------------
%% @doc
%% Activate the latest loaded version of the provided configuration file.
%% @end
%%--------------------------------------------------------------------
-spec activate(JSON :: string())-> {ok, atom()} | {error, Error :: term()}.
activate(JSON)->
    gen_server:call(?SERVER, {activate, JSON, latest}).

%%--------------------------------------------------------------------
%% @doc
%% Activate the provided version of the provided configuration file.
%% @end
%%--------------------------------------------------------------------
-spec activate(JSON :: string(), Version :: pos_integer())-> {ok, atom()} |
							     {error, Error :: term()}.
activate(JSON, Version)->
        gen_server:call(?SERVER, {versions, JSON, Version}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Filename = filename:join(code:priv_dir(gb_conf), "gb_conf.json"),
    error_logger:info_msg("~p:init/0 read configuration: ~p~n", [?MODULE, Filename]),
    case file:read_file(Filename) of
	{ok, Binary} ->	    
	    {ok, #state{}};
	{error, Reason} ->
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(list, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
handle_call({versions, JSON}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
handle_call({show, JSON, Version}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
handle_call({load, JSON}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
handle_call({activate, JSON, Version}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
            JsonTypes = mochijson2:decode(Binary, [{format, proplist}]),
            case json_to_term(JsonTypes) of
                {ok, Conf} ->
                    {ok, Conf};
                {error, Reason} ->
                    {error, Reason}
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
%% Find the given Key in application configuration term Conf.
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
store(#gb_conf_appconf{appname = Name} = AppConf, MaxKeep) when is_integer(MaxKeep)->
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

-spec do_activate(AppName:: atom(), Version::pos_integer()) -> ok | {error, Reason::term()}.
do_activate(AppName, Version)->
    Transaction =
        fun() ->
            AppConfList = mnesia:read(gb_conf_appconf, AppName),
            ActiveConf = find_active_conf(AppConfList),
            InactiveConf = find_version_conf(AppConfList, Version),
    
            case ActiveConf of
                    undefined -> ok;
                    _ -> 
                        mnesia:delete_object(ActiveConf),
                        mnesia:write(ActiveConf#gb_conf_appconf{active = false})
            end,
            mnesia:delete_object(InactiveConf),
            mnesia:write(InactiveConf#gb_conf_appconf{active = true})
        end,
    gb_conf_db:transaction(Transaction). 

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


