%%%-------------------------------------------------------------------
%%% @author maxmati
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2016 9:56 PM
%%%-------------------------------------------------------------------
-module(dchat_client_connection_manager).
-author("maxmati").

-behaviour(gen_server).

%% API
-export([start_link/0,
  connect/1,
  send/1,
  dispatch/3,
  register_handler/2]).


%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(state, {designated, handlers}).

%%%===================================================================
%%% API
%%%===================================================================
connect(Server) ->
  gen_server:cast(?MODULE, {connect, Server}).

send(Message) ->
  gen_server:cast(?MODULE, {send, Message}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

dispatch(Connection, Command, Params) ->
  gen_server:cast(?MODULE, {dispatch,Connection, Command, Params}).

register_handler(Command, Handler) ->
  gen_server:cast(?MODULE, {register, Command, Handler}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{handlers = dict:new() }}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({connect, Server}, State) ->
  connect_to(Server),
  {noreply, update_designated(State)};
handle_cast({send, Message}, State) ->
  Server = get_designated(State#state.designated),
  dchat_client_connection:send(Server, Message),
  {noreply, State};
handle_cast({dispatch, Connection, Command, Params}, State) ->
  Func = dict:fetch(Command, State#state.handlers),
  Func(Connection, Params),
  {noreply, State};
handle_cast({register, Command, Handler}, State) ->
  NewHandlers = dict:append(Command, Handler, State#state.handlers),
  NewState = State#state{handlers = NewHandlers},
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_designated(State) ->
  Designated = State#state.designated,
  State#state{designated = get_designated(Designated)}.

get_designated(undefined) ->
  dchat_client_connection_pool:get_any_connection();
get_designated(Designated) ->
  Designated.

connect_to({Hostname, Port}) ->
  {ok, Server} = dchat_client_connection_pool:connect(Hostname, Port),
  Server.
