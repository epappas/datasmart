#!/usr/bin/env escript
%%% -*- erlang -*-
%%! -pa ./tests -smp enable -boot start_sasl -config datasmart.config
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
-author("epappas").

-include("../lib/rest_user/src/user_records.hrl").

main(_) ->
  code:add_paths(filelib:wildcard("./lib/*/ebin", "./")),
  code:add_paths(filelib:wildcard("./deps/*/ebin", "./")),

  rest_store:start(),

  etap:plan(unknown),

  RegisterKVList = test_register(),
  _ProfileKVList = test_user_profile(RegisterKVList),

  etap:end_tests(),
  ok.

test_register() ->
  Email = string:concat(
    hash_md5:build(binary_to_list(uuid:uuid1())),
    ".test@test.domain.com"
  ),
  {ok, ResultList} = user_server:register(Email),

  etap:is(lists:keymember(email, 1, ResultList), true, "Should have return an Email"),
  etap:is(lists:keyfind(email, 1, ResultList), {email, list_to_binary(Email)}, "Should return the same Email"),
  etap:is(lists:keymember(oukey, 1, ResultList), true, "Should have return an oukey"),
  etap:is(lists:keymember(secret, 1, ResultList), true, "Should have return a secret"),
  etap:is(lists:keymember(aukey, 1, ResultList), true, "Should have return an aukey"),
  etap:is(lists:keymember(asecret, 1, ResultList), true, "Should have return an asecret"),
  ResultList.

test_user_profile(ResultList) ->
  Email = proplists:get_value(email, ResultList),
  OUkey = proplists:get_value(oukey, ResultList),
%%   Secret = proplists:get_value(secret, ResultList),
%%   AUkey = proplists:get_value(aukey, ResultList),
%%   ASecret = proplists:get_value(asecret, ResultList),

  {ok, UKey} = oukey_server:get_ukey({oukey, OUkey}),
  {ok, UserKVList} = user_server:getuser(UKey),

  Emaill2 = proplists:get_value(<<"email">>, UserKVList),

  etap:is(Emaill2, Email, "Email should be fetched"),
  UserKVList.

