%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ukey_server).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
  generate/1,
  check/1
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
generate(#ukey_generate{} = Token) -> gen_server:call(?MODULE, {generate, Token}).

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

  UKey = ds_util:hashPass(MD5Key, srp_server:new_salt(), 2),
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

  {reply, {ok, [
    {email, list_to_binary(Email)},
    {ukey, list_to_binary(UKey)}
  ]}, State};

handle_call({check, Email}, _From, State) ->
  MD5Key = hash_md5:build(Email),

  case couch:get(?couch_md5keys, MD5Key) of
    {error, _Error} -> %% User Should not exist
      {reply, {exist, {key, MD5Key}}, State};
    _ ->
      {reply, {nonexist, {key, MD5Key}}, State}
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
