%%% -*- erlang -*-
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
-module(ukey_server).
-author("epappas").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
  generate/1,
  check/1,
  get_ukey/1,
  srp_essentials/1
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("user_records.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

check(Email) -> gen_server:call(?MODULE, {check, Email}).

get_ukey({Type, Value}) -> gen_server:call(?MODULE, {get_ukey, {Type, Value}}).

generate(#ukey_generate{} = Token) -> gen_server:call(?MODULE, {generate, Token}).

srp_essentials(Key) -> gen_server:call(?MODULE, {srp_essentials, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({generate, #ukey_generate{email = Email,
  userPrimeBytes = UserPrimeBytes,
  userGenerator = UserGenerator, factor = Factor,
  version = Version, userRSABits = UserRSABits
}}, _From, State) ->

  MD5Key = hash_md5:build(Email),

  SrpSalt = srp_server:new_salt(),
  UKey = srp_server:new_salt(),
  Salt = srp_server:new_salt(),
  Password = srp_server:new_salt(), %% noone will know this password

  [Prime, Generator] = srp_server:prime(UserPrimeBytes, UserGenerator),
  DerivedKey = srp_server:derived_key(Salt, Email, Password, Factor),
  Verifier = srp_server:verifier(Generator, DerivedKey, Prime),
  {_, PrivKey} = crypto:generate_key(srp, {user, [Generator, Prime, Version]}),

  {privKey, RsaPrivKey} = sign_server:generate_rsa(private, UserRSABits),

  couch:save(?couch_users, {[
    {<<"_id">>, list_to_binary(UKey)},
    {<<"key">>, list_to_binary(UKey)},
    {<<"email">>, list_to_binary(Email)},
    {<<"alias">>, [list_to_binary(Email)]}
  ]}),

  couch:save(?couch_salts, {[
    {<<"_id">>, list_to_binary(UKey)},
    {<<"key">>, list_to_binary(UKey)},
    {<<"salt">>, list_to_binary(Salt)}
  ]}),

  couch:save(?couch_secrets, {[
    {<<"_id">>, list_to_binary(UKey)},
    {<<"key">>, list_to_binary(UKey)},
    {<<"srpsalt">>, base64:encode(SrpSalt)},
    {<<"verifier">>, base64:encode(Verifier)},
    {<<"prime">>, base64:encode(Prime)},
    {<<"generator">>, Generator},
    {<<"userPrimeBytes">>, UserPrimeBytes},
    {<<"userGenerator">>, UserGenerator},
    {<<"version">>, Version},
    {<<"privKey">>, base64:encode(PrivKey)},
    {<<"factor">>, Factor}
  ]}),

  couch:save(?couch_rsa, {[
    {<<"_id">>, list_to_binary(UKey)},
    {<<"key">>, list_to_binary(UKey)},
    {<<"rsaPrivKey">>, base64:encode(RsaPrivKey)},
    {<<"rsaBits">>, UserRSABits}
  ]}),

  couch:save(?couch_md5keys, {[
    {<<"_id">>, list_to_binary(MD5Key)},
    {<<"key">>, list_to_binary(UKey)}
  ]}),

  {reply, {ok, #ukey_generate_rsp{email = list_to_binary(Email), ukey = list_to_binary(UKey)}}, State};

handle_call({check, Email}, _From, State) ->
  MD5Key = hash_md5:build(Email),
  %% TODO implement HEAD req instead of GET
  case couch:get(?couch_md5keys, MD5Key) of
    {error, _Error} -> %% User Should not exist
      {reply, {nonexist, {key, MD5Key}}, State};
    _ ->
      {reply, {exist, {key, MD5Key}}, State}
  end;

handle_call({get_ukey, {email, Email}}, _From, State) ->
  MD5Key = hash_md5:build(Email),

  case couch:get(?couch_md5keys, MD5Key) of
    {ok, DocJson} ->
      {DocKVList} = DocJson,
      UKey = proplists:get_value(<<"key">>, DocKVList),
      {reply, {ok, UKey}, State};
    {error, Error} ->
      {reply, {error, Error}, State}
  end;

handle_call({srp_essentials, Key}, _From, State) ->
  case couch:get(?couch_secrets, Key) of
    {error, Error} -> {error, Error};
    {ok, EssentialsJson} ->
      {EssentialsKVList} = EssentialsJson,
      {reply, {ok, EssentialsKVList}, State};
    Error -> {error, Error}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
