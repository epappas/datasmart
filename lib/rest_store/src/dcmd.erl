%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% Copyright 2015, evalonlabs
%%%
%%% Licensed under the Apache License, Version 2.0 (the 'License');
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an 'AS IS' BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
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
    {Port, {exit_status, 0}} -> {ok, Data};
    {Port, {exit_status, S}} -> {error, S}
  after Timeout ->
    throw(timeout)
  end.
