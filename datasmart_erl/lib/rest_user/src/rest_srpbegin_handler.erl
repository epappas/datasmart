%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_srpbegin_handler).
-author("evangelosp").

-export([init/2]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {module, method}).

init(Req, Opts) ->
  Module = proplists:get_value(module, Opts),
  case Module =:= undefined of
    true -> {ok, Req, []};
    false -> handle(Req, #state{})
  end.

handle(Req, #state{} = State) ->
  Method = cowboy_req:method(Req),
  NewState = State#state{method = Method},
  {ok, Req2} = process(Req, NewState),
  {ok, Req2, []}.

process(Req, #state{method = <<"POST">>} = _State) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  EmailBin = proplists:get_value(<<"email">>, Params),

  case EmailBin of
    undefined -> end_with_failure(400, list_to_binary("No Valid Arguments"), Req);
    EmailBin ->
      Email = binary:bin_to_list(EmailBin),

      case handle_query({srp_begin, Email}, Req2) of
        {ok, Resp, _} -> end_with_success({Resp}, Req2);
        _ -> end_with_failure(400, list_to_binary("Uknown Error"), Req)
      end
  end;

process(_, Req) -> end_with_failure(405, list_to_binary("Method not allowed"), Req).

handle_query({srp_begin, Email}, Req) ->
  case srp_server:begin_srp({email, Email}) of
    {ok, ResultKVList} -> {ok, ResultKVList, Req};
    {error, Error} -> {error, Error, Req};
    X -> {error, X, Req}
  end.

end_with_success(Message, Req) -> {ok, echo(200, jiffy:encode(Message), Req)}.

end_with_failure(Code, Message, Req) ->
  {ok, echo(Code, jiffy:encode({[
    {code, Code},
    {status, error},
    {error, Message}
  ]}), Req)}.

echo(Status, Echo, Req) ->
  cowboy_req:reply(Status, [
    {<<"content-type">>, <<"application/json; charset=utf-8">>},
    {<<"server">>, <<"myinbox-datastore">>}
  ], Echo, Req).

terminate(_Reason, _Req, _State) ->
  ok.