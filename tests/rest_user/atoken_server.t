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

-include("../../lib/rest_user/src/user_records.hrl").

main(_) ->
  code:add_paths(filelib:wildcard("./lib/*/ebin", "./")),
  code:add_paths(filelib:wildcard("./deps/*/ebin", "./")),

  rest_store:start(),

  etap:plan(unknown),

  {AUKey, ResultKVList} = test_generate(),

  AToken = proplists:get_value(atoken, ResultKVList),

  test_get_aukey(AUKey, AToken),

  test_check(AUKey, AToken),

  etap:end_tests(),
  ok.

test_generate() ->
  Version = ?User_SRP_Version, %% get User's Version
  UserRSABits = ?User_RSA_Bits, %% Define RSA bits
  UserPrimeBytes = ?User_Prime_Bytes, %% get Prime of user
  UserGenerator = ?User_Generator, %% get generator of user
  Factor = ?Factor,

  Email = string:concat(
    hash_md5:build(binary_to_list(uuid:uuid1())),
    ".test@test.domain.com"
  ),

  {ok, #ukey_generate_rsp{ukey = UKey}} =
    ukey_server:generate(#ukey_generate{
      email = Email, userPrimeBytes = UserPrimeBytes,
      userGenerator = UserGenerator,
      factor = Factor, version = Version,
      userRSABits = UserRSABits
    }),

  {ok, #oukey_generate_rsp{oukey = OUKey, secret = Secret}} =
    oukey_server:generate(#oukey_generate{
      ukey = UKey, email = Email,
      userPrimeBytes = UserPrimeBytes,
      userGenerator = UserGenerator,
      factor = Factor, version = Version,
      userRSABits = UserRSABits
    }),

  {ok, #aukey_generate_rsp{aukey = AUKey, asecret = ASecret}} =
    aukey_server:generate(#aukey_generate{
      oukey = OUKey, userPrimeBytes = UserPrimeBytes,
      userGenerator = UserGenerator,
      factor = Factor, version = Version,
      userRSABits = UserRSABits
    }),

  {ok, ResultKVList} = atoken_server:generate(#atoken_generate{
    aukey = AUKey, scope = ?Scope_all,
    expires = integer_to_list(list_to_integer(?Default_Atoken_Expiration, 10) + ds_util:timestamp())
  }),

  etap:is(is_binary(proplists:get_value(atoken, ResultKVList)), true, "Should have generate an atoken"),
  etap:is(is_binary(proplists:get_value(opensalt, ResultKVList)), true, "Should have generate an opensalt"),
  etap:is(is_list(proplists:get_value(scope, ResultKVList)), true, "Should have generate an scope"),

  {AUKey, ResultKVList}.

test_get_aukey(AUKey, AToken) ->
  {ok, AUKey1} = atoken_server:get_aukey({atoken, AToken}),

  etap:is(AUKey, AUKey1, "Should have matched the AUKey"),

  AToken.

test_check(AUKey, AToken) ->
  {ok, CheckKVList} = atoken_server:check(AToken),

  etap:is(proplists:get_value(aukey, CheckKVList), AUKey, "Should have matched AUKey"),
  etap:is(is_binary(proplists:get_value(opensalt, CheckKVList)), true, "Should have get Salt"),
  etap:is(lists:keymember(scope, 1, CheckKVList), true, "Should have get Scope"),
  etap:is(lists:keymember(expires, 1, CheckKVList), true, "Should have get Expires"),

  AToken.
