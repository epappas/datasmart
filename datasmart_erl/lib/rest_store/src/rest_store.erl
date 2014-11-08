%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_store).
-author("evangelosp").

%% API.
-export([start/0]).

%% API.
start() ->
  application:start(crypto),
  application:start(asn1),
  application:start(public_key),
  application:start(ssl),
  application:start(bcrypt),
  application:start(ranch),
  application:start(hackney),
  couchbeam:start(), %% application:start(couchbeam),
  application:start(cowlib),
  application:start(cowboy),
  application:start(rest_store),
  application:start(rest_user),
  application:start(rest_files).
