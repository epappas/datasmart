%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_oapikey_handler).
-author("evangelosp").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  {ok, Req3} = process(Method, Req),
  {ok, Req3, State}.

process(<<"GET">>, Req) ->
  {OApiKeyBin, Req2} = cowboy_req:qs_val(<<"apiKey">>, Req),
  {OSecretBin, Req2} = cowboy_req:qs_val(<<"secret">>, Req),

  case OApiKeyBin of
    undefined -> end_with_failure(400, "No Valid Arguments", Req);
    OApiKeyBin ->
      OApiKey = binary:bin_to_list(OApiKeyBin),
      OSecret = case OSecretBin of
                  undefined -> undefined;
                  OSecretBin -> binary:bin_to_list(OSecretBin)
                end,

      case user_server:checkuser(OApiKey, OSecret) of
        {ok, Ukey} ->
          {ok, UProfile} = user_server:getuser(Ukey),
          echo(200, jiffy:encode({[
            {profile, {UProfile}}
          ]}), Req2);
        {error, _} ->
          end_with_failure(400, "No Valid Arguments", Req)
      end
  end;

process(<<"POST">>, Req) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),
  {OUkeyBin, Req3} = cowboy_req:binding(oukey, Req2),

  OApiKeyBin = proplists:get_value(<<"apiKey">>, Params),
  OSecretBin = proplists:get_value(<<"secret">>, Params),

  case OApiKeyBin of
    undefined -> end_with_failure(400, "No Valid Arguments", Req3);
    OApiKeyBin ->
      OApiKey = binary:bin_to_list(OApiKeyBin),
      OSecret = binary:bin_to_list(OSecretBin),
      OUkey = binary:bin_to_list(OUkeyBin),

      case user_server:match_ouKey(OUkey) of
        {ok, Ukey} ->
          user_server:match_ouKey(OUkey),
          case user_server:add_aukey(Ukey, OApiKey, OSecret) of
            {ok, Resp} -> echo(200, jiffy:encode({Resp}), Req3);

            _ -> end_with_failure(400, "Uknown Error", Req3)
          end;

        _ -> end_with_failure(400, "No Valid Arguments", Req3)
      end
  end;

process(_, Req) ->
  end_with_failure(405, "Method not allowed.", Req).

echo(Status, Echo, Req) ->
  cowboy_req:reply(Status, [
    {<<"content-type">>, <<"application/json; charset=utf-8">>},
    {<<"server">>, <<"myinbox-datastore">>}
  ], Echo, Req).

end_with_failure(Code, Message, Req) ->
  echo(Code, jiffy:encode({[
    {code, Code},
    {status, error},
    {error, Message}
  ]}), Req).

terminate(_Reason, _Req, _State) ->
  ok.
