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
-module(sign_server).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0,
  generate_rsa/1,
  generate_rsa/2,
  decode_rsa/1,
  decode_rsa/2,
  sign/2,
  sign/3,
  verify/3,
  verify/4,
  encrypt/2,
  decrypt/2
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%% {privKey, RSA/Binary}
generate_rsa(private) -> generate_rsa(private, 2048).

generate_rsa(private, Bits) -> gen_server:call(?MODULE, {generate_rsa, private, Bits});
generate_rsa(public, RSAPrivateKey) -> gen_server:call(?MODULE, {generate_rsa, public, RSAPrivateKey}).

decode_rsa(PemBin) -> gen_server:call(?MODULE, {decode_rsa, PemBin}).
decode_rsa(PemBin, Password) -> gen_server:call(?MODULE, {decode_rsa, PemBin, Password}).

sign(Msg, RSAPrivateKey) -> {signature, public_key:sign(Msg, sha, RSAPrivateKey)}.
sign(Msg, Type, RSAPrivateKey) -> {signature, public_key:sign(Msg, Type, RSAPrivateKey)}.

verify(Msg, Signature, RSAPublicKey) -> public_key:verify(Msg, sha, Signature, RSAPublicKey).
verify(Msg, Type, Signature, RSAPublicKey) -> public_key:verify(Msg, Type, Signature, RSAPublicKey).

encrypt(Msg, {privKey, RSAPrivateKey}) -> public_key:encrypt_private(Msg, RSAPrivateKey);
encrypt(Msg, {public, RSAPublicKey}) -> public_key:encrypt_public(Msg, RSAPublicKey).

decrypt(Encrypted, {privKey, RSAPrivateKey}) -> public_key:decrypt_private(Encrypted, RSAPrivateKey);
decrypt(Encrypted, {public, RSAPublicKey}) -> public_key:decrypt_public(Encrypted, RSAPublicKey).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) -> {ok, #state{}}.

handle_call({generate_rsa, private, Bits}, _From, State) ->
  {ok, Out} = dcmd:run(io_lib:format("openssl genrsa ~p", [Bits])),
  case extract_PRIVRSA(Out) of
    [] -> {reply, {error, empty}, State};
    Priv_RSA -> {reply, {privKey, iolist_to_binary(Priv_RSA)}, State}
  end;

handle_call({generate_rsa, public, RSAPrivateKey}, _From, State) ->
  {ok, Out} = dcmd:run(io_lib:format("echo \"~s\" | openssl rsa -pubout", [binary_to_list(RSAPrivateKey)])),
  case extract_PUBRSA(Out) of
    [] -> {reply, {error, empty}, State};
    Pub_RSA -> {reply, {pubKey, iolist_to_binary(Pub_RSA)}, State}
  end;

handle_call({decode_rsa, PemBin}, _From, State) ->
  [RSAEntry] = public_key:pem_decode(PemBin),
  RSAKey = public_key:pem_entry_decode(RSAEntry),
  {reply, {key, RSAKey}, State};

handle_call({decode_rsa, PemBin, Password}, _From, State) ->
  [RSAEntry] = public_key:pem_decode(PemBin),
  RSAKey = public_key:pem_entry_decode(RSAEntry, Password),
  {reply, {key, RSAKey}, State};

handle_call({}, _From, State) ->
  {reply, {error}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

extract_PRIVRSA(RSA_Input) ->
  RList = string:tokens(RSA_Input, "\n"),
  TargetStr = "-----BEGIN RSA PRIVATE KEY-----",
  Pred = fun(Elem) ->
    case string:equal(Elem, TargetStr) of
      true -> false;
      _ -> true
    end
  end,
  string:join(lists:dropwhile(Pred, RList), io_lib:format("~n", [])).

extract_PUBRSA(RSA_Input) ->
  RList = string:tokens(RSA_Input, "\n"),
  TargetStr = "-----BEGIN PUBLIC KEY-----",
  Pred = fun(Elem) ->
    case string:equal(Elem, TargetStr) of
      true -> false;
      _ -> true
    end
  end,
  string:join(lists:dropwhile(Pred, RList), io_lib:format("~n", [])).


%% Example of use
%% {privKey, RSAKey} = sign_server:generate_rsa(private).
%% {pubKey, RSAPubKey} = sign_server:generate_rsa(public, RSAKey).
%%
%% {keyey, RSAPrivateKey} = sign_server:decode_rsa(RSAKey).
%% {keyey, PubKey} = sign_server:decode_rsa(RSAPubKey).
%%
%% {signature, Signature} = sign_server:sign(<<"test">>, RSAPrivateKey).
%%
%% sign_server:verify(<<"test">>, Signature, PubKey) =:= true.