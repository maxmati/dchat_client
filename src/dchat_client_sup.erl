%%%-------------------------------------------------------------------
%% @doc dchat_client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dchat_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartStrategy = {one_for_all, 0, 1},
    ConnectionManagerSupSpec = {connection_manager_sup, {dchat_client_connection_manager_sup, start_link, []},
        permanent, brutal_kill, worker, [dchat_client_connection_manager_sup]},
    Children = [ConnectionManagerSupSpec],
    {ok, { RestartStrategy, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
