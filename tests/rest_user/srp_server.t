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

  #ukey_generate_rsp{
    email = Email, ukey = UKey
  } = test_generate(),

  Factor= ?Factor,
  Salt = srp_server:new_salt(),
  Password = srp_server:new_salt(),

  ClientSRPKVList = test_begin_srp(Email),
  ServerSRPKVList = test_begin_srp(Email),

  {pubKey, ServerPubKey} = lists:keyfind(pubKey, 1, ServerSRPKVList),

  ClientSharedKey = test_compute_key(client, ServerPubKey, [
    {<<"privKey">>, proplists:get_value(privKey, ClientSRPKVList)},
    {<<"pubKey">>, proplists:get_value(pubKey, ClientSRPKVList)},
    {<<"derivedKey">>, srp_server:derived_key(Salt, binary_to_list(Email), Password, Factor)},
    {<<"prime">>, proplists:get_value(prime, ClientSRPKVList)},
    {<<"generator">>, proplists:get_value(generator, ClientSRPKVList)},
    {<<"version">>, proplists:get_value(version, ClientSRPKVList)}
  ]),

  test_compute_key(server, ClientSharedKey, [
    {<<"privKey">>, proplists:get_value(privKey, ServerSRPKVList)},
    {<<"pubKey">>, proplists:get_value(pubKey, ServerSRPKVList)},
    {<<"prime">>, proplists:get_value(prime, ServerSRPKVList)},
    {<<"generator">>, proplists:get_value(generator, ServerSRPKVList)},
    {<<"version">>, proplists:get_value(version, ServerSRPKVList)},
    {<<"verifier">>, proplists:get_value(verifier, ServerSRPKVList)}
  ]),

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

  {ok, #ukey_generate_rsp{ukey = UKey} = UKeyGenerateRSP} =
    ukey_server:generate(#ukey_generate{
      email = Email, userPrimeBytes = UserPrimeBytes,
      userGenerator = UserGenerator,
      factor = Factor, version = Version,
      userRSABits = UserRSABits
    }),

  UKeyGenerateRSP.

test_begin_srp(Email) ->
  {ok, SRPKVList} = srp_server:begin_srp({email, Email}),

  etap:is(lists:keymember(sesRef, 1, SRPKVList), true, "Should begin_srp returned sesRef as part of the response"),
  etap:is(lists:keymember(salt, 1, SRPKVList), true, "Should begin_srp returned salt as part of the response"),
  etap:is(lists:keymember(privKey, 1, SRPKVList), true, "Should begin_srp returned privKey as part of the response"),
  etap:is(lists:keymember(pubKey, 1, SRPKVList), true, "Should begin_srp returned pubKey as part of the response"),
  etap:is(lists:keymember(prime, 1, SRPKVList), true, "Should begin_srp returned prime as part of the response"),
  etap:is(lists:keymember(generator, 1, SRPKVList), true, "Should begin_srp returned generator as part of the response"),
  etap:is(lists:keymember(version, 1, SRPKVList), true, "Should begin_srp returned version as part of the response"),
  etap:is(lists:keymember(verifier, 1, SRPKVList), true, "Should begin_srp returned verifier as part of the response"),

  SRPKVList.

test_compute_key(server, ClientSharedKey, SRPKVList) ->

  {ok, ServerPubKey} = srp_server:compute_key(server, {clientPub, ClientSharedKey}, SRPKVList),

  etap:is(is_binary(ClientSharedKey), true, "Should compute_key server return a binary public key"),

  ServerPubKey;

test_compute_key(client, ServerPub, SRPKVList) ->

  {ok, ClientSharedKey} = srp_server:compute_key(client, {serverPub, ServerPub}, SRPKVList),

  etap:is(is_binary(ClientSharedKey), true, "Should compute_key client return a binary shared key"),

  ClientSharedKey.
