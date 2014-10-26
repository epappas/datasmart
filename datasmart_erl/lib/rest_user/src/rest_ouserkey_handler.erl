%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_ouserkey_handler).
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
  {OUkeyBin, Req2} = cowboy_req:binding(oukey, Req),

  case OUkeyBin of
    undefined -> end_with_failure(400, "No Valid Arguments", Req2);
    OUkeyBin ->
      OUkey = binary:bin_to_list(OUkeyBin),
      case handle_query({userinfo, OUkey}, Req2) of
        {info, UProfile, _} ->
          echo(200, jiffy:encode({[
            {profile, {UProfile}}
          ]}), Req2);
        _ ->
          end_with_failure(400, "No Valid Arguments", Req2)
      end
  end;

process(_, Req) ->
  end_with_failure(405, "Method not allowed.", Req).

handle_query({userinfo, OUkey}, Req) ->
  {ok, Ukey} = user_server:match_ouKey(OUkey),
  {ok, UProfile} = user_server:getuser(Ukey),
  {info, ensureUUProfileJson(UProfile), Req}.

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

ensureUUProfileJson(Json) -> Json.
%%   [
%%     {user, proplists:get_value(<<"oukey">>, Json)},
%%     {name, proplists:get_value(<<"name">>, Json)},
%%     {settings, proplists:get_value(<<"settings">>, Json)},
%%     {alias, proplists:get_value(<<"alias">>, Json)}
%%   ].