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
  register/2,
  add_aukey/3,
  getukey/1,
  match_ouKey/1,
  match_auKey/2,
  getuser/1,
  srp_essentials/1,
  updateprofile/2,
  encrypt/3,
  dencrypt/3,
  checkuser/2
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

register(Email, Password) -> gen_server:call(?MODULE, {register, Email, Password}).

add_aukey(Ukey, AUKey, Secret) -> gen_server:call(?MODULE, {addaukey, Ukey, AUKey, Secret}).

getukey(Email) -> gen_server:call(?MODULE, {getukey, Email}).

match_ouKey(OUkey) -> gen_server:call(?MODULE, {match_ouKey, OUkey}).

match_auKey(AUkey, Secret) -> gen_server:call(?MODULE, {match_auKey, AUkey, Secret}).

getuser(Ukey) -> gen_server:call(?MODULE, {getuser, Ukey}).

srp_essentials(Ukey) -> gen_server:call(?MODULE, {srp_essentials, Ukey}).

checkuser(Email, Password) -> gen_server:call(?MODULE, {checkuser, Email, Password}).

updateprofile(Ukey, KeyValList) -> gen_server:call(?MODULE, {updateprofile, Ukey, KeyValList}).

encrypt(Ukey, IVec, Text) ->
  case couch:get("erl_users_secrets", Ukey) of
    {ok, SecretJson} ->
      %% {SecretJson} = jiffy:decode(BSecretJson),
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
  case couch:get("erl_users_secrets", Ukey) of
    {ok, SecretJson} ->
      %% {SecretJson} = jiffy:decode(BSecretJson),
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

handle_call({register, Email, Password}, _From, State) ->
  MD5Key = hash_md5:build(Email),
  OpenSalt = srp_server:new_salt(),
  OUKey = hashPass(MD5Key, OpenSalt, 2),
  Secret = srp_server:new_salt(),
  Salt = srp_server:new_salt(),
  Version = '6a', %% get User's Version
  UserRSABits = 2048, %% Define RSA bits
  UserPrimeBytes = 256, %% get Prime of user
  UserGenerator = 2, %% get generator of user
  Factor = 20,
  case couch:get("erl_users_secrets", MD5Key) of
    {error, _Error} -> %% User Should not exist

      [Prime, Generator] = srp_server:prime(UserPrimeBytes, UserGenerator),
      DerivedKey = srp_server:derived_key(Salt, Email, Password, Factor),
      Verifier = srp_server:verifier(Generator, DerivedKey, Prime),
      {_, PrivKey} = crypto:generate_key(srp, {user, [Generator, Prime, Version]}),
      {privKey, RsaPrivKey} = sign_server:generate_rsa(private, UserRSABits),

      %% Super Normilize user's data, for security reasons.
      %% Sensitive data shouldn't by acccident be replicated in a non-secure couchDB
      %% but user's profile can
      couch:save("erl_users", {[
        {<<"_id">>, list_to_binary(MD5Key)},
        {<<"key">>, list_to_binary(MD5Key)},
        {<<"email">>, list_to_binary(Email)},
        {<<"openkey">>, list_to_binary(OUKey)},
        {<<"alias">>, [list_to_binary(Email)]}
      ]}),
      couch:save("erl_users_oukeys", {[
        {<<"_id">>, list_to_binary(OUKey)},
        {<<"key">>, list_to_binary(MD5Key)},
        {<<"openkey">>, list_to_binary(OUKey)}
      ]}),
      couch:save("erl_users_salts", {[
        {<<"_id">>, list_to_binary(MD5Key)},
        {<<"key">>, list_to_binary(MD5Key)},
        {<<"salt">>, list_to_binary(Salt)}
      ]}),
      couch:save("erl_users_secrets", {[
        {<<"_id">>, list_to_binary(MD5Key)},
        {<<"key">>, list_to_binary(MD5Key)},
        {<<"secret">>, list_to_binary(Secret)}
      ]}),
      couch:save("erl_users_essentials", {[
        {<<"_id">>, list_to_binary(MD5Key)},
        {<<"verifier">>, base64:encode(Verifier)},
        {<<"prime">>, base64:encode(Prime)},
        {<<"generator">>, Generator},
        {<<"userPrimeBytes">>, UserPrimeBytes},
        {<<"userGenerator">>, UserGenerator},
        {<<"salt">>, base64:encode(list_to_binary(Salt))},
        {<<"version">>, Version},
        {<<"rsaPrivKey">>, base64:encode(RsaPrivKey)},
        {<<"rsaBits">>, UserRSABits},
        {<<"privKey">>, base64:encode(PrivKey)},
        {<<"factor">>, Factor}
      ]}),
      %% couch:save("erl_users_pass", {[
      %%   {<<"_id">>, list_to_binary(MD5Key)},
      %%   {<<"pass">>, list_to_binary(hashPass(Password, Salt, 20))}
      %% ]}),
      couch:save("erl_users_alias", {[
        {<<"_id">>, list_to_binary(Email)},
        {<<"key">>, list_to_binary(MD5Key)},
        {<<"email">>, list_to_binary(Email)},
        {<<"alias">>, list_to_binary(Email)}
      ]}),

      %% lets index/cache them ?
      qredis:q(["HSETNX", "datasmart:alias", Email, Email]),
      qredis:q(["HSETNX", "datasmart:alias:keys", Email, OUKey]),

      {reply, {ok, [{email, list_to_binary(Email)}, {oukey, list_to_binary(OUKey)}]}, State};
    _ -> {reply, {error, "Registration Failure, User Exists"}, State}
  end;

handle_call({addaukey, Ukey, AUKey, Secret}, _From, State) ->
  case couch:get("erl_users_salts", Ukey) of
    {error, Error} -> {reply, {error, Error}, State};
    {ok, SaltJson} ->
      %% {SaltJson} = jiffy:decode(BSaltJson),
      {SaltKVList} = SaltJson,
      Salt = proplists:get_value(<<"salt">>, SaltKVList),

      couch:save("erl_users_accesskeys", {[
        {<<"_id">>, AUKey},
        {<<"ukey">>, Ukey},
        {<<"aukey">>, AUKey},
        {<<"secret">>, hashPass(Secret, Salt, 20)}
      ]}),

      %% lets index/cache them ?
      qredis:q(["SET", lists:concat(["datasmart:acesskey:", AUKey, ":ukey"]), Ukey]),
      {reply, {ok, [{ukey, Ukey}, {aukey, AUKey}, {secret, Secret}]}, State};
    Error -> {reply, {error, Error}, State}
  end;

handle_call({srp_essentials, Ukey}, _From, State) ->
  {reply, doEssentials(Ukey), State};

handle_call({getukey, Email}, _From, State) ->
  {reply, doGetukey(Email), State};

handle_call({match_ouKey, OUkey}, _From, State) ->
  {reply, doMatchOUKey(OUkey), State};

handle_call({match_auKey, AUkey, Secret}, _From, State) ->
  {reply, doMatchAUKey(AUkey, Secret), State};

handle_call({getuser, Ukey}, _From, State) ->
  {reply, doGetUser(Ukey), State};

handle_call({updateprofile, Ukey, KeyValList}, _From, State) ->
  {reply, doUpdateProfile(Ukey, KeyValList), State};

handle_call({checkuser, Email, Password}, _From, State) ->
  {reply, doCheckuser(Email, Password), State}.

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

doCheckuser(Email, Password) ->
  case doGetukey(Email) of
    {ok, Ukey} ->
      case couch:get("erl_users_salts", Ukey) of
        {error, Error} -> {error, Error};
        {ok, SaltJson} ->
          {SaltKVList} = SaltJson,
          %% {SaltJson} = jiffy:decode(BSaltJson),
          Salt = proplists:get_value(<<"salt">>, SaltKVList),

          case couch:get("erl_users_pass", Ukey) of
            {error, Error} -> {error, Error};
            {ok, PassJson} ->
              %% {PassJson} = jiffy:decode(BPassJson),
              {PassKVList} = PassJson,
              UserPass = proplists:get_value(<<"pass">>, PassKVList),
              HashPass = hashPass(Password, Salt, 20),

              case UserPass =:= HashPass of
                true -> {ok, Ukey};
                false -> {error, false}
              end;
            Error -> {error, Error}
          end;
        Error -> {error, Error}
      end;
    _ -> {error, "Wrong User Details"}
  end.

doGetukey(Email) ->
  case couch:get("erl_users_alias", Email) of
    {error, _Error} -> {error, "Uknown Email"};
    {ok, AliasJson} ->
      %% {AliasJson} = jiffy:decode(BAliasJson),
      {AliasKVList} = AliasJson,
      Ukey = proplists:get_value(<<"key">>, AliasKVList),
      {ok, Ukey};
    _ -> {error, "Uknown Email"}
  end.

doMatchOUKey(OUKey) ->
  case couch:get("erl_users_oukeys", OUKey) of
    {error, _Error} -> {error, "Uknown Key"};
    {ok, OUKeyJson} ->
      %% {OUKeyJson} = jiffy:decode(BOUKeyJson),
      {OUKeyKVList} = OUKeyJson,
      Ukey = proplists:get_value(<<"key">>, OUKeyKVList),
      {ok, Ukey};
    _ -> {error, "Uknown Key"}
  end.

doMatchAUKey(AUKey, Secret) ->
  case couch:get("erl_users_accesskeys", AUKey) of
    {error, _Error} -> {error, "Uknown Key"};
    {ok, AUKeyJson} ->
      %% {AUKeyJson} = jiffy:decode(BAUKeyJson),
      {AUKeyKVList} = AUKeyJson,
      Ukey = proplists:get_value(<<"ukey">>, AUKeyKVList),
      StoredSecret = proplists:get_value(<<"secret">>, AUKeyKVList),

      case couch:get("erl_users_salts", Ukey) of
        {error, Error} -> {error, Error};
        {ok, SaltJson} ->
          %% {SaltJson} = jiffy:decode(BSaltJson),
          {SaltKVList} = SaltJson,
          Salt = proplists:get_value(<<"salt">>, SaltKVList),

          HashSecret = hashPass(Secret, Salt, 20),

          case StoredSecret =:= HashSecret of
            true -> {ok, Ukey};
            false -> {error, false}
          end;

        Error -> {error, Error}
      end;
    Error -> {error, Error}
  end.

doGetUser(Ukey) ->
  case couch:get("erl_users", Ukey) of
    {error, Error} -> {error, Error};
  %% Json = ds_util:list_to_keyval(Result),
  %% User = [{binary_to_atom(Key, utf8), Val} || {Key, Val} <- Json],
    {ok, UserJson} ->
      {UserKVList} = UserJson,
      {ok, UserKVList};
    Error -> {error, Error}
  end.

doEssentials(Ukey) ->
  case couch:get("erl_users_essentials", Ukey) of
    {error, Error} -> {error, Error};
    {ok, EssentialsJson} ->
      {EssentialsKVList} = EssentialsJson,
      {ok, EssentialsKVList};
    Error -> {error, Error}
  end.

doUpdateProfile(Ukey, KeyValList) ->
  case couch:get("erl_users", Ukey) of
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

      case couch:save("erl_users", {NewUserValList2}) of
        {error, Error} -> {error, Error};
        {ok, Result} -> {ok, Result};
        Error -> {error, Error}
      end;
    Error -> {error, Error}
  end.

hashPass(Password, Salt, 0) ->
  hash_md5:build(lists:concat([Password, Salt]));

hashPass(Password, Salt, Factor) when (Factor rem 2) > 0 ->
  hashPass(hash_md5:build(lists:concat([Password, Salt])), Salt, Factor - 1);

hashPass(Password, Salt, Factor) ->
  hashPass(hash_md5:build(lists:concat([Salt, Password])), Salt, Factor - 1).