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

  test_rsa(),

  etap:end_tests(),
  ok.

test_rsa() ->

  {privKey, RSAKey} = sign_server:generate_rsa(private),

  etap:is({privKey, is_binary(RSAKey)}, {privKey, true}, "RSAKey is binary"),

  <<FirstPart:32/binary, _/binary>> = RSAKey,

  etap:is(FirstPart, <<"-----BEGIN RSA PRIVATE KEY-----\n">>, "Header of RSA Private Key"),

  {pubKey, PubKey} = sign_server:generate_rsa(public, RSAKey),

  <<FP:27/binary, _/binary>> = PubKey,

  etap:is(FP, <<"-----BEGIN PUBLIC KEY-----\n">>, "Header of RSA Private Key").
