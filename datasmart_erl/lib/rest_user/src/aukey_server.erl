%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aukey_server).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
  generate/1
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

generate(#aukey_generate{} = Token) -> gen_server:call(?MODULE, {generate, Token}).

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

handle_call({generate, #aukey_generate{oukey = OUKey,
  userPrimeBytes = UserPrimeBytes,
  userGenerator = UserGenerator, factor = Factor,
  version = Version, userRSABits = UserRSABits
}}, _From, State) ->

  ASalt = srp_server:new_salt(),
  AUKey = ds_util:hashPass(srp_server:new_salt(), ASalt, 2),
  ASecret = srp_server:new_salt(),

  [APrime, AGenerator] = srp_server:prime(UserPrimeBytes, UserGenerator),
  ADerivedKey = srp_server:derived_key(ASalt, AUKey, ASecret, Factor),
  AVerifier = srp_server:verifier(AGenerator, ADerivedKey, APrime),
  {_, APrivKey} = crypto:generate_key(srp, {user, [AGenerator, APrime, Version]}),

  {privKey, ARsaPrivKey} = sign_server:generate_rsa(private, UserRSABits),

  couch:save(?couch_aukeys, {[
    {<<"_id">>, list_to_binary(AUKey)},
    {<<"key">>, list_to_binary(OUKey)},
    {<<"accesskey">>, list_to_binary(AUKey)},
    {<<"scope">>, jiffy:encode(?Scope_all)}
  ]}),
  couch:save(?couch_salts, {[
    {<<"_id">>, list_to_binary(AUKey)},
    {<<"key">>, list_to_binary(AUKey)},
    {<<"salt">>, list_to_binary(ASalt)}
  ]}),
  couch:save(?couch_secrets, {[
    {<<"_id">>, list_to_binary(AUKey)},
    {<<"key">>, list_to_binary(AUKey)},
    {<<"verifier">>, base64:encode(AVerifier)},
    {<<"prime">>, base64:encode(APrime)},
    {<<"generator">>, AGenerator},
    {<<"userPrimeBytes">>, UserPrimeBytes},
    {<<"userGenerator">>, UserGenerator},
    {<<"version">>, Version},
    {<<"privKey">>, base64:encode(APrivKey)},
    {<<"factor">>, Factor}
  ]}),
  couch:save(?couch_rsa, {[
    {<<"_id">>, list_to_binary(AUKey)},
    {<<"key">>, list_to_binary(AUKey)},
    {<<"rsaPrivKey">>, base64:encode(ARsaPrivKey)},
    {<<"rsaBits">>, UserRSABits}
  ]}),

  {reply, {ok, [
    {aukey, list_to_binary(AUKey)},
    {asecret, list_to_binary(ASecret)}
  ]}, State};

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
