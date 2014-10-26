%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%% Created : 26. Oct 2014 12:43
%%%-------------------------------------------------------------------
-module(dcmd).
-author("evangelosp").

%% API
-export([run/1, run/2, run/3]).

run(Cmd) ->
  Timeout = 3000,
  run(Cmd, Timeout).

run(Cmd, Timeout) ->
  Opt = [stream, exit_status, use_stdio, stderr_to_stdout, in, eof],
  run(Cmd, Timeout, Opt).

run(Cmd, Timeout, Opt) ->
  Port = erlang:open_port({spawn, Cmd}, Opt),
  loop(Port, [], Timeout).

loop(Port, Data, Timeout) ->
  receive
    {Port, {data, NewData}} -> loop(Port, lists:concat([Data, NewData]), Timeout);
    {Port, {exit_status, 0}} -> Data;
    {Port, {exit_status, S}} -> throw({commandfailed, S})
  after Timeout ->
    throw(timeout)
  end.
