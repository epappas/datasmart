%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_files_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  rest_files_sup:start_link().

start(_StartType, _StartArgs) ->
  start().

stop(_State) ->
  ok.
