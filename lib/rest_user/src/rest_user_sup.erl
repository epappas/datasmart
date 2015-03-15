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
-module(rest_user_sup).
-author("epappas").

-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, {{one_for_one, 10, 10}, [
    {user_server,
      {user_server, start_link, []},
      permanent, 1000, worker,
      [user_server]},
    {srp_server,
      {srp_server, start_link, []},
      permanent, 1000, worker,
      [srp_server]},
    {ukey_server,
      {ukey_server, start_link, []},
      permanent, 1000, worker,
      [ukey_server]},
    {aukey_server,
      {aukey_server, start_link, []},
      permanent, 1000, worker,
      [aukey_server]},
    {oukey_server,
      {oukey_server, start_link, []},
      permanent, 1000, worker,
      [oukey_server]},
    {atoken_server,
      {atoken_server, start_link, []},
      permanent, 1000, worker,
      [atoken_server]},
    {sign_server,
      {sign_server, start_link, []},
      permanent, 1000, worker,
      [sign_server]}
  ]}}.


