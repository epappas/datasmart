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

  {OUKey, #aukey_generate_rsp{aukey = AUKey}} = test_generate(),

  test_get_aukey(OUKey, AUKey),

  test_srp_essentials(OUKey),

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

  {ok, #aukey_generate_rsp{aukey = AUKey, asecret = ASecret} = AUKeyGenerateRSP} =
    aukey_server:generate(#aukey_generate{
      oukey = OUKey, userPrimeBytes = UserPrimeBytes,
      userGenerator = UserGenerator,
      factor = Factor, version = Version,
      userRSABits = UserRSABits
    }),

  etap:is(is_binary(AUKey), true, "Should have generate an AUKey"),
  etap:is(is_binary(ASecret), true, "Should have generate a ASecret"),

  {OUKey, AUKeyGenerateRSP}.

test_get_aukey(OUKey, AUKey) ->
  {ok, [
    {aukey, AUKey1},
    {oukey, OUKey1},
    {scope, Scope}
  ]} = aukey_server:get_aukey({aukey, AUKey}),

  etap:is(AUKey, AUKey1, "Should have matched the AUKey"),
  etap:is(OUKey, OUKey1, "Should have matched the OUKey"),
  etap:is(is_binary(Scope), true, "Should Scope be a list"),

  AUKey.

test_srp_essentials(AUKey) ->
  {ok, EssentialsKVList} = aukey_server:srp_essentials(AUKey),

  etap:is(is_list(EssentialsKVList), true, "Should have fetched the EssentialsKVList"),
  etap:is(lists:keyfind(<<"key">>, 1, EssentialsKVList), {<<"key">>, AUKey}, "Should the AUKey match"),
  EssentialsKVList.
