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

  Text = <<"Foo Bar Foo">>,

  RSAKeyBin = test_generate_rsa(private),

  {key, RSAKey} = sign_server:decode_rsa(RSAKeyBin),

  RSAPubKeyBin = test_generate_rsa(public, RSAKeyBin),

  {key, RSAPubKey} = sign_server:decode_rsa(RSAPubKeyBin),

  Signature = test_sign(Text, RSAKey),

  test_verify(Text, Signature, RSAPubKey),

  etap:end_tests(),
  ok.

test_generate_rsa(private) ->
  {privKey, RSAKey} = sign_server:generate_rsa(private),

  etap:is(is_binary(RSAKey), true, "Should generate_rsa private return a binary RSAKey key"),

  RSAKey.

test_generate_rsa(public, RSAKey) ->
  {pubKey, RSAPubKey} = sign_server:generate_rsa(public, RSAKey),

  etap:is(is_binary(RSAPubKey), true, "Should generate_rsa private return a binary RSAPubKey key"),

  RSAPubKey.

test_sign(Text, RSAKey) ->
  {signature, Signature} = sign_server:sign(Text, RSAKey),

  etap:is(is_binary(Signature), true, "Should sign return a binary Signature"),

  Signature.

test_verify(Text, Signature, RSAPubKey) ->
  etap:is(sign_server:verify(Text, Signature, RSAPubKey), true, "Should verify the signed content").
