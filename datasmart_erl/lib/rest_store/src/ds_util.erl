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
%%%-------------------------------------------------------------------
-module(ds_util).
-author("evangelosp").

%% API
-export([
  uuid/0,
  list_to_keyval/1,
  list_to_keyval_rev/1,
  timestamp/0,
  timestamp/1,
  now/0,
  now/1,
  hashPass/3
]).

uuid() ->
  uuid:to_string(uuid:uuid3(uuid:uuid4(), uuid:to_string(uuid:uuid1()))).

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

hashPass(Password, Salt, 0) ->
  hash_md5:build(lists:concat([Password, Salt]));

hashPass(Password, Salt, Factor) when (Factor rem 2) > 0 ->
  hashPass(hash_md5:build(lists:concat([Password, Salt])), Salt, Factor - 1);

hashPass(Password, Salt, Factor) ->
  hashPass(hash_md5:build(lists:concat([Salt, Password])), Salt, Factor - 1).
