%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_srpverify_handler).
-author("evangelosp").

-export([init/2]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {module, method}).

init(Req, Opts) ->
  Module = proplists:get_value(module, Opts),
  case Module =:= undefined of
    true -> {ok, Req, []};
    false -> handle(Req, #state{module = Module})
  end.

handle(Req, #state{} = State) ->
  Method = cowboy_req:method(Req),
  NewState = State#state{method = Method},
  {ok, Req2} = process(Req, NewState),
  {ok, Req2, []}.

process(Req, #state{method = <<"POST">>, module = user} = _State) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  M1Bin = proplists:get_value(<<"m1">>, Params),
  SrpStateJSONBin = proplists:get_value(<<"srpstate">>, Params, <<"{}">>),

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
          end_with_success(jiffy:encode({NewSrpStateKVList}), Req2);
        _ ->
          end_with_failure(400, "Uknown Error", Req)
      end
  end;

process(Req, #state{method = <<"POST">>, module = oukey} = _State) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  M1Bin = proplists:get_value(<<"m1">>, Params),
  SrpStateJSONBin = proplists:get_value(<<"srpstate">>, Params, <<"{}">>),

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
          end_with_success(jiffy:encode({NewSrpStateKVList}), Req2);
        _ ->
          end_with_failure(400, "Uknown Error", Req)
      end
  end;

process(Req, #state{method = <<"POST">>, module = aukey} = _State) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  M1Bin = proplists:get_value(<<"m1">>, Params),
  SrpStateJSONBin = proplists:get_value(<<"srpstate">>, Params, <<"{}">>),

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
          end_with_success(jiffy:encode({NewSrpStateKVList}), Req2);
        _ ->
          end_with_failure(400, "Uknown Error", Req)
      end
  end;

process(_, Req) -> end_with_failure(405, list_to_binary("Method not allowed"), Req).

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