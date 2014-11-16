%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ds_util).
-author("evangelosp").

%% API
-export([
  list_to_keyval/1,
  list_to_keyval_rev/1,
  timestamp/0,
  timestamp/1,
  now/0,
  now/1
]).

timestamp() -> timestamp(os:timestamp()).

timestamp({MegaSecs, Secs, MicroSecs}) ->
  (MegaSecs * 1000000000000) + (Secs * 1000000) + MicroSecs.

now() -> now(os:timestamp()).

now({MegaSecs, Secs, _MicroSecs}) ->
  (MegaSecs * 1000000) + Secs.

list_to_keyval([]) -> [];

list_to_keyval([K, V | T]) -> [{K, V} | list_to_keyval(T)].

list_to_keyval_rev([]) -> [];

list_to_keyval_rev([K, V | T]) -> [{V, K} | list_to_keyval(T)].
