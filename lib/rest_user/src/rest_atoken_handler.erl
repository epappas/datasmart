%%% -*- erlang -*-
%%%-------------------------------------------------------------------
%%% @author Evangelos Pappas <epappas@evalonlabs.com>
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
-module(rest_atoken_handler).
-author("epappas").

-export([init/2]).
-export([
  allowed_methods/2,
  content_types_provided/2,
  is_authorized/2,
  known_methods/2,
  malformed_request/2,
  options/2,
  service_available/2,
  uri_too_long/2
]).
-export([handle/2]).

-record(state, {
  state = init, method,
  isAuthorized = false
}).

%% FLOW: ALL[0]
init(Req, _Opts) ->
  {cowboy_rest, Req, #state{}}.

%% FLOW: ALL[1] OR 503
%% Return whether the service is available.
service_available(Req, State) -> {true, Req, State}.

%% FLOW: ALL[2] OR 501
%% Return the list of known methods.
known_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

%% FLOW: ALL[3] OR 414
%% Return whether the requested URI is too long.
uri_too_long(Req, State) -> {false, Req, State}.

%% FLOW: ALL[4] OR 405
%% Return the list of allowed methods.
allowed_methods(Req, State) ->
  Method = cowboy_req:method(Req),
  NewState = State#state{method = Method},
  {[<<"HEAD">>, <<"GET">>, <<"OPTIONS">>], Req, NewState}.

%% FLOW: ALL[5] OR 400
%% Return whether the request is malformed.
malformed_request(Req, State) ->
  NewState = State#state{state = malformed_request},
  handle(Req, NewState).

%% FLOW: ALL[6] OR 401
%% Return whether the user is authorized to perform the action.
is_authorized(Req, State = #state{}) ->
  NewState = State#state{state = is_authorized},
  handle(Req, NewState).

%% FLOW: ALL[10], OPTIONS[0] THEN 200
%% Handle a request for information.
options(Req, State) -> {ok, Req, State}.

%% FLOW: ALL[10] OR 406
%% Return the list of content-types the resource provides.
%% ProvideResource = iodata() | {stream, Fun} | {stream, Len, Fun} | {chunked, ChunkedFun}
content_types_provided(Req, State) ->
  NewState = State#state{state = content_types_provided},
  {[{{<<"application">>, <<"json">>, '*'}, handle}], Req, NewState}.

handle(Req, State = #state{method = <<"GET">>, state = malformed_request}) ->
  case cowboy_req:header(<<"Authorization">>, Req) of
    undefined -> {true, Req, State};
    _ -> {false, Req, State}
  end;

handle(Req, State = #state{method = <<"GET">>, state = is_authorized}) ->
  QsVals = cowboy_req:parse_qs(Req),
  Scope = proplists:get_value(<<"scope">>, QsVals, undefined),
  ScopeList =
    case Scope of
      undefined -> [];
      Scope -> [Scope]
    end,
  case ds_util:bearer_atoken_check(Req, ScopeList) of
    {ok, _TokenKVList} ->
      NewState = State#state{isAuthorized = true},
      {true, Req, NewState};
    {error, _} -> {false, Req, State}
  end;

handle(Req, State = #state{method = <<"GET">>, state = content_types_provided}) ->
  {jiffy:encode({[
    {isOk, true}
  ]}), Req, State}.