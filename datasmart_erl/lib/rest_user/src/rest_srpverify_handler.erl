%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_srpverify_handler).
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

process(<<"POST">>, Req) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  M1Bin = proplists:get_value(<<"m1">>, Params),
  SrpStateJSONBin = proplists:get_value(<<"srpstate">>, Params),

  case M1Bin of
    undefined -> end_with_failure(400, "No Valid Arguments", Req);
    M1Bin ->
      M1 = binary:bin_to_list(M1Bin),
      SrpStateJSON = binary:bin_to_list(SrpStateJSONBin),
      {SrpStateKVList} = jiffy:decode(SrpStateJSON),
      SKey = proplists:get_value(skey, SrpStateKVList),

      case M1 =:= SKey of
        true ->
          NewSrpStateKVList = lists:append([{verified, true}], SrpStateKVList),
          echo(200, jiffy:encode({NewSrpStateKVList}), Req2);
        _ ->
          end_with_failure(400, "Uknown Error", Req)
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