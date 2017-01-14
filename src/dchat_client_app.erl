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
connect(Server, Nickname) ->
    dchat_client_connection_manager:connect(Server, Nickname).

send(Recipient, Message) ->
    dchat_client_connection_manager:send({message, [Recipient, Message]}).

disconnect() ->
    dchat_client_connection_manager:send({logout, []}).

start(_StartType, _StartArgs) ->
    dchat_client_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================