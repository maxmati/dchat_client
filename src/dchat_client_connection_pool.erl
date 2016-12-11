%%%-------------------------------------------------------------------
%%% @author maxmati
%%% @copyright (C) 2016, <COMPANY>
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

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

connect(Hostname, Port) ->
  supervisor:start_child(?SERVER, [Hostname, Port]).

get_any_connection() ->
  [{_, Pid, _, _}| _] = supervisor:which_children(?SERVER),
  Pid.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  ConnectionServer = {connection, {dchat_client_connection, start_link, []},
    Restart, Shutdown, Type, [dchat_client_connection]},

  {ok, {SupFlags, [ConnectionServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
