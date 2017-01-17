%%%-------------------------------------------------------------------
%%% @author maxmati
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2017 4:59 PM
%%%-------------------------------------------------------------------
-module(dchat_ui).
-author("maxmati").

%% API
-export([run/0]).

run() ->
  application:start(dchat_client),
  dchat_client_connection_manager:register_handler(message, fun (_, Param) -> receive_message(Param) end),
  dchat_client_connection_manager:register_handler(nick_taken, fun (_, [Nick]) -> nick_taken(Nick) end),
  parse_line().

parse_line() ->
  Line = lists:droplast(io:get_line("> ")),
  {Command, Param} = split_line(Line),
  case execute_command(Command, Param)  of
      true -> parse_line();
      false -> ok
  end.

split_line(Line) ->
  Space = find_space(Line),
  Command = string:sub_string(Line, 1, Space-1),
  Param = case length(Line) of
    Space -> [];
    _ -> string:sub_string(Line, Space+1, length(Line))
  end,
  {Command, Param}.

find_space(Line) ->
  find_space(Line, 1).

find_space([32|_T], N) ->
  N;
find_space([_H|T], N) ->
  find_space(T, N+1);
find_space([], N) ->
  N-1.

execute_command(Command, Param) ->
  case Command of
    "quit" -> quit();
    "connect" -> connect(Param);
    "msg" -> message(Param);
    _ -> wrong_command(Command)
  end.

quit() ->
  dchat_client_app:disconnect(),
  application:stop(dchat_client),
  false.

connect(Param) ->
  [Host, Port, Nickname] = string:tokens(Param, " "),
  {IntPort, _} = string:to_integer(Port),
  dchat_client_app:connect({Host, IntPort}, Nickname),
  true.

message(Param) ->
  {Recipient, Message} = split_line(Param),
  dchat_client_app:send(Recipient, Message),
  true.

wrong_command(Command) ->
  io:format("Unrecognized command: ~s. Type `quit` to exit~n", [Command]),
  true.

receive_message(Params) ->
  io:format("\r~s: ~s~n> ", Params).

nick_taken(Nick) ->
  io:format("\rNickname ~s is already taken~n> ", [Nick]).