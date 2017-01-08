%%%-------------------------------------------------------------------
%% @doc dchat_client public API
%% @end
%%%-------------------------------------------------------------------

-module(dchat_client_app).

-behaviour(application).

%% Application callbacks
-export([connect/2, send/2, disconnect/0, start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
connect(Servers, Nickname) ->
    application:start(dchat_client),
    lists:foreach(fun dchat_client_connection_manager:connect/1, Servers),
    dchat_client_connection_manager:send({login, [Nickname]}).

send(Recipient, Message) ->
    dchat_client_connection_manager:send({message, [Recipient, Message]}).

disconnect() ->
    dchat_client_connection_manager:send({logout, []}),
    application:stop(dchat_client).

start(_StartType, _StartArgs) ->
    dchat_client_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================