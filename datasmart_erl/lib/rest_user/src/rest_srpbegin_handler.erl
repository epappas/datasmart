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
-module(rest_srpbegin_handler).
-author("evangelosp").

-export([init/2]).
-export([handle/2]).
-export([terminate/3]).

-include("user_records.hrl").

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

  EmailBin = proplists:get_value(<<"email">>, Params),

  case EmailBin of
    undefined -> end_with_failure(400, list_to_binary("No Valid Arguments"), Req);
    EmailBin ->
      Email = binary:bin_to_list(EmailBin),

      case srp_server:begin_srp({email, Email}) of
        {ok, ResultKVList} ->
          end_with_success({manage_session(ResultKVList)}, Req2);
        {error, _Error} ->
          end_with_failure(400, list_to_binary("Uknown Error"), Req);
        _X -> end_with_failure(400, list_to_binary("Uknown Error"), Req)
      end
  end;

process(Req, #state{method = <<"POST">>, module = oukey} = _State) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  OUKeyBin = proplists:get_value(<<"oukey">>, Params),

  case OUKeyBin of
    undefined -> end_with_failure(400, list_to_binary("No Valid Arguments"), Req);
    OUKeyBin ->
      OUKey = binary:bin_to_list(OUKeyBin),

      case srp_server:begin_srp({oukey, OUKey}) of
        {ok, ResultKVList} ->
          end_with_success({manage_session(ResultKVList)}, Req2);
        {error, _Error} ->
          end_with_failure(400, list_to_binary("Uknown Error"), Req);
        _X -> end_with_failure(400, list_to_binary("Uknown Error"), Req)
      end
  end;

process(Req, #state{method = <<"POST">>, module = aukey} = _State) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  AUKeyBin = proplists:get_value(<<"aukey">>, Params),

  case AUKeyBin of
    undefined -> end_with_failure(400, list_to_binary("No Valid Arguments"), Req);
    AUKeyBin ->
      AUKey = binary:bin_to_list(AUKeyBin),

      case srp_server:begin_srp({aukey, AUKey}) of
        {ok, ResultKVList} ->
          end_with_success({manage_session(ResultKVList)}, Req2);
        {error, _Error} ->
          end_with_failure(400, list_to_binary("Uknown Error"), Req);
        _X -> end_with_failure(400, list_to_binary("Uknown Error"), Req)
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

manage_session(SessionltKVList) ->
  SesRef = proplists:get_value(sesRef, SessionltKVList),

  ecache:put(?CACHE_TABLE, SesRef, SessionltKVList),

  [{sesRef, SesRef}].

terminate(_Reason, _Req, _State) ->
  ok.