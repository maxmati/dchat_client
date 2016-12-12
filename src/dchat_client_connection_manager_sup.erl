%%%-------------------------------------------------------------------
%%% @author maxmati
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2016 1:27 AM
%%%-------------------------------------------------------------------
-module(dchat_client_connection_manager_sup).
-author("maxmati").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  ConnectionManager = {connection_manager, {dchat_client_connection_manager, start_link, []},
    Restart, Shutdown, Type, [dchat_client_connection_manager]},

  ConnectionPool = {connection_pool, {dchat_client_connection_pool, start_link, []},
    Restart, Shutdown, Type, [dchat_client_connection_pool]},

  {ok, {SupFlags, [ConnectionManager, ConnectionPool]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
