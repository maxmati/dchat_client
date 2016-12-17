%%%-------------------------------------------------------------------
%%% @author maxmati
%%% @doc
%%%
%%% @end
%%% Created : 04. Dec 2016 11:36 PM
%%%-------------------------------------------------------------------
-module(dchat_client_connection_pool).
-author("maxmati").

-behaviour(supervisor).

%% API
-export([start_link/0, connect/2, get_any_connection/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

connect(Hostname, Port) ->
  supervisor:start_child(?MODULE, [Hostname, Port]).

get_any_connection() ->
  [{_, Pid, _, _}| _] = supervisor:which_children(?MODULE),
  Pid.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = transient,
  Shutdown = 2000,
  Type = worker,

  ConnectionServer = {connection, {dchat_client_connection, start_link, []},
    Restart, Shutdown, Type, [dchat_client_connection]},

  {ok, {SupFlags, [ConnectionServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
