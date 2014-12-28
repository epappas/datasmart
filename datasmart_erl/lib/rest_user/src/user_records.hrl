%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("evangelosp").

-define(User_RSA_Bits, 2048).
-define(User_Prime_Bytes, 256).
-define(User_Generator, 2).
-define(User_SRP_Version, '6a').
-define(Factor, 20).
-define(Scope_all, [
  <<"all">>, <<"list_panel">>, <<"change_pass">>,
  <<"delete_access_keys">>, <<"list_access_keys">>,
  <<"list_settings">>, <<"create_access_keys">>,
  <<"edit_settings">>, <<"list_timeline">>,
  <<"edit_timeline">>, <<"post_timeline">>,
  <<"delete_timeline">>, <<"edit_profile">>,
  <<"delete_profile">>, <<"add_photo">>,
  <<"add_billing">>, <<"remove_billing">>,
  <<"download_files">>, <<"upload_files">>,
  <<"create_dir">>, <<"delete_dir">>,
  <<"use_rsa">>, <<"use_srp">>
]).

-define(couch_users, "erl_users").
-define(couch_md5keys, "erl_users_md5keys").
-define(couch_oukeys, "erl_users_oukeys").
-define(couch_aukeys, "erl_users_aukeys").
-define(couch_salts, "erl_users_salts").
-define(couch_secrets, "erl_users_secrets").
-define(couch_rsa, "erl_users_rsa").
-define(couch_user_alias, "erl_users_alias").
-define(couch_atokens, "erl_users_atokens").

-record(ukey_generate, {email, userPrimeBytes, userGenerator, factor, version, userRSABits}).
-record(oukey_generate, {ukey, email, userPrimeBytes, userGenerator, factor, version, userRSABits}).
-record(aukey_generate, {oukey, userPrimeBytes, userGenerator, factor, version, userRSABits}).
-record(atoken_generate, {aukey, expires, scope = []}).

-record(ukey_generate_rsp, {email, ukey}).
-record(oukey_generate_rsp, {oukey, secret}).
-record(aukey_generate_rsp, {aukey, asecret}).

