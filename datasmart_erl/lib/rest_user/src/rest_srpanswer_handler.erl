%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% Copyright 2015, evalonlabs
%%%
%%% Licensed under the Apache License, Version 2.0 (the 'License');
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an 'AS IS' BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_srpanswer_handler).
-author("evangelosp").

-include("user_records.hrl").

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

  ClientPubBin = proplists:get_value(<<"clientPub">>, Params),
  SesRef = proplists:get_value(<<"sesRef">>, Params),

  case ClientPubBin of
    undefined -> end_with_failure(400, "No Valid Arguments", Req);
    ClientPubBin ->
      ClientPub = binary:bin_to_list(ClientPubBin),

      case ecache:get(?CACHE_TABLE, SesRef) of
        {ok, SrpStateKVList} ->
          case srp_server:compute_key(server, {clientPub, ClientPub}, SrpStateKVList) of
            {ok, SKey, _} ->
              NewSrpStateKVList = lists:append([{skey, SKey}], SrpStateKVList),
              end_with_success(jiffy:encode({NewSrpStateKVList}), Req2);
            _ -> end_with_failure(400, "Uknown Error", Req)
          end;
        _ -> end_with_failure(400, "Uknown Error", Req)
      end
  end;

process(Req, #state{method = <<"POST">>, module = oukey} = _State) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  ClientPubBin = proplists:get_value(<<"clientPub">>, Params),
  SesRef = proplists:get_value(<<"sesRef">>, Params),

  case ClientPubBin of
    undefined -> end_with_failure(400, "No Valid Arguments", Req);
    ClientPubBin ->
      ClientPub = binary:bin_to_list(ClientPubBin),

      case ecache:get(?CACHE_TABLE, SesRef) of
        {ok, SrpStateKVList} ->
          case srp_server:compute_key(server, {clientPub, ClientPub}, SrpStateKVList) of
            {ok, SKey, _} ->
              NewSrpStateKVList = lists:append([{skey, SKey}], SrpStateKVList),
              end_with_success(jiffy:encode({NewSrpStateKVList}), Req2);
            _ -> end_with_failure(400, "Uknown Error", Req)
          end;
        _ -> end_with_failure(400, "Uknown Error", Req)
      end
  end;

process(Req, #state{method = <<"POST">>, module = aukey} = _State) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  ClientPubBin = proplists:get_value(<<"clientPub">>, Params),
  SesRef = proplists:get_value(<<"sesRef">>, Params),

  case ClientPubBin of
    undefined -> end_with_failure(400, "No Valid Arguments", Req);
    ClientPubBin ->
      ClientPub = binary:bin_to_list(ClientPubBin),

      case ecache:get(?CACHE_TABLE, SesRef) of
        {ok, SrpStateKVList} ->
          case srp_server:compute_key(server, {clientPub, ClientPub}, SrpStateKVList) of
            {ok, SKey, _} ->
              NewSrpStateKVList = lists:append([{skey, SKey}], SrpStateKVList),
              end_with_success(jiffy:encode({NewSrpStateKVList}), Req2);
            _ ->
              end_with_failure(400, "Uknown Error", Req)
          end;
        _ -> end_with_failure(400, "Uknown Error", Req)
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