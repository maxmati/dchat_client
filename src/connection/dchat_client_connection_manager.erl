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
  connect/2,
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

-record(state, {designated, handlers, alternative, nickname}).

%%%===================================================================
%%% API
%%%===================================================================
connect(Server, Nickname) ->
  gen_server:cast(?MODULE, {connect, Server, Nickname}).

send(Message) ->
  gen_server:cast(?MODULE, {send, Message}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

dispatch(Connection, Command, Params) ->
  gen_server:cast(?MODULE, {dispatch,Connection, Command, Params}).

register_handler(Command, Handler) ->
  gen_server:cast(?MODULE, {register, Command, Handler}).

add_alternative([Server]) ->
  gen_server:cast(?MODULE, {add_alternative, Server}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{alternative = queue:new(), handlers = dict:from_list([
    {add_server, fun (_Connection, Params) -> add_alternative(Params) end}
  ])}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({connect, Server, Nickname}, State) ->
  Socket = connect_to(Server, Nickname),
  {noreply, State#state{designated = Socket, nickname = Nickname}};
handle_cast({send, Message}, State) ->
  Server = get_designated(State#state.designated),
  dchat_client_connection:send(Server, Message),
  {noreply, State};
handle_cast({dispatch, Connection, Command, Params}, State) ->
  case dict:find(Command, State#state.handlers) of
    {ok, Func} -> Func(Connection, Params);
    error -> io:format("unsupported command ~p(~p)~n", [Command, Params])
  end,
  {noreply, State};
handle_cast({register, Command, Handler}, State) ->
  NewHandlers = dict:append(Command, Handler, State#state.handlers),
  NewState = State#state{handlers = NewHandlers},
  {noreply, NewState};
handle_cast({add_alternative, Server}, State) ->
  case queue:member(Server, State#state.alternative) of
    true  -> {noreply, State};
    false -> {noreply, State#state{alternative = queue:in(Server, State#state.alternative)}}
  end;
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN',_,_,_,_}, State) ->
  {{value, ServerInfo}, Queue} = queue:out(State#state.alternative),
  Server = connect_to(ServerInfo, State#state.nickname),
  {noreply, State#state{designated = Server, alternative = Queue}};
handle_info(Info, State) ->
  io:format("unhandled info ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_designated(undefined) ->
  dchat_client_connection_pool:get_any_connection();
get_designated(Designated) ->
  Designated.

connect_to({Hostname, Port}, Nickname) ->
  {ok, Server} = dchat_client_connection_pool:connect(Hostname, Port),
  monitor(process, Server),
  dchat_client_connection:send(Server, {login, [Nickname]}),
  Server.