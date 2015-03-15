%%% -*- erlang -*-
%%%-------------------------------------------------------------------
%%% @author Evangelos Pappas <epappas@evalonlabs.com>
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
%%%-------------------------------------------------------------------
-module(qredis).
-author("epappas").

-behaviour(gen_server).

%% API
-export([
  q/1,
  start_link/0,
  start_link/2,
  start_link/3,
  start_link/4
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, ["127.0.0.1", 6379, 0, ""], []).

start_link(Host, Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port, 0, ""], []).

start_link(Host, Port, Database) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port, Database, ""], []).

start_link(Host, Port, Database, Password) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port, Database, Password], []).

q(Req) ->
  gen_server:call(?MODULE, Req).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Port, Database, Password]) ->
  {ok, Conn} = eredis:start_link(Host, Port, Database, Password),
  {ok, Conn}.

handle_call(Request, _From, Conn) ->
  {reply, eredis:q(Conn, Request), Conn}.

handle_cast(_Request, Conn) ->
  {noreply, Conn}.

handle_info(_Info, Conn) ->
  {noreply, Conn}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
