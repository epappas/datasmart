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
-module(oukey_server).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
  generate/1,
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

generate(#oukey_generate{} = Token) -> gen_server:call(?MODULE, {generate, Token}).

get_ukey({Type, Value}) -> gen_server:call(?MODULE, {get_ukey, {Type, Value}}).

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

handle_call({generate, #oukey_generate{ukey = UKey,
  email = Email, userPrimeBytes = UserPrimeBytes,
  userGenerator = UserGenerator, factor = Factor,
  version = Version, userRSABits = UserRSABits
}}, _From, State) ->

  SrpSalt = srp_server:new_salt(),
  OSalt = srp_server:new_salt(),
  OUKey = srp_server:new_salt(),
  Secret = ds_util:hashPass(srp_server:new_salt(), OSalt, 2),

  [OPrime, OGenerator] = srp_server:prime(UserPrimeBytes, UserGenerator),
  ODerivedKey = srp_server:derived_key(OSalt, OUKey, Secret, Factor),
  OVerifier = srp_server:verifier(OGenerator, ODerivedKey, OPrime),
  {_, OPrivKey} = crypto:generate_key(srp, {user, [OGenerator, OPrime, Version]}),

  {privKey, ORsaPrivKey} = sign_server:generate_rsa(private, UserRSABits),

  couch:save(?couch_oukeys, {[
    {<<"_id">>, list_to_binary(OUKey)},
    {<<"key">>, UKey},
    {<<"email">>, Email},
    {<<"openkey">>, list_to_binary(OUKey)},
    {<<"scope">>, jiffy:encode(?Scope_all)}
  ]}),
  couch:save(?couch_salts, {[
    {<<"_id">>, list_to_binary(OUKey)},
    {<<"key">>, list_to_binary(OUKey)},
    {<<"salt">>, list_to_binary(OSalt)}
  ]}),
  couch:save(?couch_secrets, {[
    {<<"_id">>, list_to_binary(OUKey)},
    {<<"key">>, list_to_binary(OUKey)},
    {<<"srpsalt">>, base64:encode(SrpSalt)},
    {<<"verifier">>, base64:encode(OVerifier)},
    {<<"prime">>, base64:encode(OPrime)},
    {<<"generator">>, OGenerator},
    {<<"userPrimeBytes">>, UserPrimeBytes},
    {<<"userGenerator">>, UserGenerator},
    {<<"version">>, Version},
    {<<"privKey">>, base64:encode(OPrivKey)},
    {<<"factor">>, Factor}
  ]}),
  couch:save(?couch_rsa, {[
    {<<"_id">>, list_to_binary(OUKey)},
    {<<"key">>, list_to_binary(OUKey)},
    {<<"rsaPrivKey">>, base64:encode(ORsaPrivKey)},
    {<<"rsaBits">>, UserRSABits}
  ]}),

  {reply, {ok, #oukey_generate_rsp{oukey = list_to_binary(OUKey), secret = list_to_binary(Secret)}}, State};

handle_call({get_ukey, {oukey, OUkey}}, _From, State) ->
  case couch:get(?couch_oukeys, OUkey) of
    {error, _Error} -> {error, "Uknown Key"};
    {ok, OUKeyJson} ->
      {OUKeyKVList} = OUKeyJson,
      Ukey = proplists:get_value(<<"key">>, OUKeyKVList),
      {reply, {ok, Ukey}, State};
    _ -> {error, "Uknown Key"}
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
