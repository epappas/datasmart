%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_user_sup).

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
    {qredis,
      {qredis, start_link, []},
      permanent, 1000, worker,
      [qredis]},
    {couch,
      {couch, start_link, []},
      permanent, 1000, worker,
      [couch]},
    {user_server,
      {user_server, start_link, []},
      permanent, 1000, worker,
      [user_server]},
    {srp_server,
      {srp_server, start_link, []},
      permanent, 1000, worker,
      [srp_server]},
    {sign_server,
      {sign_server, start_link, []},
      permanent, 1000, worker,
      [sign_server]}
  ]}}.

