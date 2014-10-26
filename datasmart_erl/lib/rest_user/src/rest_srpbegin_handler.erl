%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_srpbegin_handler).
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

  EmailBin = proplists:get_value(<<"email">>, Params),

  case EmailBin of
    undefined -> end_with_failure(400, "No Valid Arguments", Req);
    EmailBin ->
      Email = binary:bin_to_list(EmailBin),

      case handle_query({srp_begin, Email}, Req2) of
        {ok, Resp, _} ->
          echo(200, jiffy:encode({Resp}), Req2);
        _ ->
          end_with_failure(400, "Uknown Error", Req)
      end
  end;

process(_, Req) ->
  end_with_failure(405, "Method not allowed.", Req).

handle_query({srp_begin, Email}, Req) ->
  case srp_server:begin_srp({email, Email}) of
    {ok, ResultKVList} -> {ok, ResultKVList, Req};
    {error, Error} -> {error, Error, Req};
    X -> {error, X, Req}
  end.

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