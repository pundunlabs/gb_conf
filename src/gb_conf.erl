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

-export([list/0,
	     versions/1,
	     show/1, show/2,
	     load/1,
	     activate/1, activate/2
	    ]).

-define(SERVER, ?MODULE).

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
-spec db_init() -> ok.
db_init()->
    %% Read the configuration to get mnesia topology
    PrivDir = code:priv_dir(gb_conf),
    Filename = filename:join(PrivDir, "gb_conf.json"),
    error_logger:info_msg("GB Configuration: Read  ~p~n", [Filename]),
    case file:read_file(Filename) of
        {ok, Binary} ->
            %% Process binary and
            %%gb_conf_db:create_tables(Conf),
            ok;
        {error, Reason} ->
            error_logger:error_msg("file:read_file(~p) -> ~p.~n", [Filename, {error, Reason}])
    end.
    
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
%% List the configuration files tahat are loaded on the system
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
