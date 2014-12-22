%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(user_server).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0,
  register/1,
  add_aukey/3,
  getuser/1,
  srp_essentials/1,
  updateprofile/2,
  encrypt/3,
  dencrypt/3
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

-include("user_records.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(Email) -> gen_server:call(?MODULE, {register, Email}).

add_aukey(Ukey, AUKey, Secret) -> gen_server:call(?MODULE, {addaukey, Ukey, AUKey, Secret}).

getuser(Ukey) -> gen_server:call(?MODULE, {getuser, Ukey}).

srp_essentials(Ukey) -> gen_server:call(?MODULE, {srp_essentials, Ukey}).

updateprofile(Ukey, KeyValList) -> gen_server:call(?MODULE, {updateprofile, Ukey, KeyValList}).

encrypt(Ukey, IVec, Text) ->
  case couch:get(?couch_secrets, Ukey) of
    {ok, SecretJson} ->
      {SecretKVList} = SecretJson,
      SecretBin = proplists:get_value(<<"secret">>, SecretKVList),
      SecretList = binary_to_list(SecretBin),
      Secret = lists:sublist(SecretList, 8),

      IvacBin = list_to_binary(case is_binary(IVec) of
                                 false -> lists:sublist(IVec, 8);
                                 true -> lists:sublist(binary_to_list(IVec), 8)
                               end),
      TextChecked = case length(Text) rem 8 of
                      0 -> Text;
                      N -> Text ++ [" " || _S <- lists:seq(1, 8 - N)]
                    end,
      {ok, crypto:des_cbc_encrypt(Secret, IvacBin, TextChecked)};

    Error -> {error, Error}
  end.

dencrypt(Ukey, IVec, Text) ->
  case couch:get(?couch_secrets, Ukey) of
    {ok, SecretJson} ->
      {SecretKVList} = SecretJson,
      SecretBin = proplists:get_value(<<"secret">>, SecretKVList),
      SecretList = binary_to_list(SecretBin),
      Secret = lists:sublist(SecretList, 8),

      IvacBin = list_to_binary(case is_binary(IVec) of
                                 false -> lists:sublist(IVec, 8);
                                 true -> lists:sublist(binary_to_list(IVec), 8)
                               end),
      {ok, crypto:des_cbc_decrypt(Secret, IvacBin, Text)};

    Error -> {error, Error}
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call({register, Email}, _From, State) ->
  Version = ?User_SRP_Version, %% get User's Version
  UserRSABits = ?User_RSA_Bits, %% Define RSA bits
  UserPrimeBytes = ?User_Prime_Bytes, %% get Prime of user
  UserGenerator = ?User_Generator, %% get generator of user
  Factor = ?Factor,

  case ukey_server:check(Email) of
    {nonexist, {key, MD5Key}} ->

      %% Generate a User
      {ok, [{email, Email}, {ukey, UKey}]} =
        ukey_server:generate(#ukey_generate{
          email = Email, userPrimeBytes = UserPrimeBytes,
          userGenerator = UserGenerator,
          factor = Factor, version = Version,
          userRSABits = UserRSABits
        }),

      %% Generate an Open Key for this user
      {ok, [{oukey, OUKey}, {secret, Secret}]} =
        oukey_server:generate(#oukey_generate{
          ukey = UKey, email = Email,
          userPrimeBytes = UserPrimeBytes,
          userGenerator = UserGenerator,
          factor = Factor, version = Version,
          userRSABits = UserRSABits
        }),

      %% Generate an Access Key for this Open Key
      {ok, [{aukey, AUKey}, {asecret, ASecret}]} =
        aukey_server:generate(#aukey_generate{
          oukey = OUKey, userPrimeBytes = UserPrimeBytes,
          userGenerator = UserGenerator,
          factor = Factor, version = Version,
          userRSABits = UserRSABits
        }),

      %% Store primary email as both Alias & source
      couch:save(?couch_user_alias, {[
        {<<"_id">>, list_to_binary(MD5Key)},
        {<<"key">>, list_to_binary(UKey)},
        {<<"email">>, list_to_binary(Email)},
        {<<"alias">>, list_to_binary(Email)}
      ]}),

      {reply, {ok, [
        {email, list_to_binary(Email)},
        {oukey, list_to_binary(OUKey)},
        {secret, list_to_binary(Secret)},
        {aukey, list_to_binary(AUKey)},
        {asecret, list_to_binary(ASecret)}
      ]}, State};
    _ -> {reply, {error, "Registration Failure"}, State}
  end;

handle_call({getuser, Ukey}, _From, State) ->
  {reply, doGetUser(Ukey), State};

handle_call({updateprofile, Ukey, KeyValList}, _From, State) ->
  {reply, doUpdateProfile(Ukey, KeyValList), State}.

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

doGetUser(Ukey) ->
  case couch:get(?couch_users, Ukey) of
    {error, Error} -> {error, Error};
    {ok, UserJson} ->
      {UserKVList} = UserJson,
      {ok, UserKVList};
    Error -> {error, Error}
  end.

doUpdateProfile(Ukey, KeyValList) ->
  case couch:get(?couch_users, Ukey) of
    {error, Error} -> {error, Error};
    {ok, UserJson} ->
      {UserKVList} = UserJson,
      This_id = proplists:get_value(<<"_id">>, UserKVList),
      This_rev = proplists:get_value(<<"_rev">>, UserKVList),
      NewUserValList1 = lists:ukeymerge(1, KeyValList, UserKVList),
      NewUserValList2 = lists:ukeymerge(1, [
        {<<"_id">>, This_id},
        {<<"_rev">>, This_rev}
      ], NewUserValList1),

      case couch:save(?couch_users, {NewUserValList2}) of
        {error, Error} -> {error, Error};
        {ok, Result} -> {ok, Result};
        Error -> {error, Error}
      end;
    Error -> {error, Error}
  end.