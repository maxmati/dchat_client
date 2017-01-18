%%%-------------------------------------------------------------------
%%% @author maxmati
%%% @doc
%%%
%%% @end
%%% Created : 04. Dec 2016 11:30 PM
%%%-------------------------------------------------------------------
-module(dchat_client_connection).
-author("maxmati").

-behaviour(gen_server).

%% API
-export([start_link/0,
  send/2,
  connect/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(state, {socket}).

%%%===================================================================
%%% API
%%%===================================================================

send(Server, Message) ->
  gen_server:cast(Server, {send, Message}).

connect(Server, Hostname, Port) ->
  gen_server:call(Server, {connect, Hostname, Port}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call({connect, Hostname, Port}, _From, State) ->
  case internal_connect(Hostname, Port) of
    {ok, Socket} -> {reply, ok, State#state{socket = Socket}};
    {error, _Reason} -> {reply, error, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({send, Message}, State) ->
  Socket = State#state.socket,
  ok = gen_tcp:send(Socket, term_to_binary(Message)),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({tcp_closed,_}, State) ->
  {stop, normal, State};
handle_info({tcp,_, Data}, State) ->
  {Command, Params} = binary_to_term(Data),
  dchat_client_connection_manager:dispatch(self(),Command,Params),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

internal_connect(Hostname, Port) ->
  gen_tcp:connect(Hostname, Port, [binary, {active,true}, {packet,4}]).
