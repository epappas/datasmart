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
-module(rest_oapikey_handler).
-author("epappas").

-export([init/2]).
-export([
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  forbidden/2,
  is_authorized/2,
  known_methods/2,
  malformed_request/2,
  options/2,
  resource_exists/2,
  service_available/2
]).
-export([handle/2]).

-record(state, {
  state = init, method,
  isAuthorized = false,
  oukey, apiKey, secret,
  ukey, profile, etag
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

%% FLOW: ALL[4] OR 405
%% Return the list of allowed methods.
allowed_methods(Req, State) ->
  Method = cowboy_req:method(Req),
  NewState = State#state{method = Method},
  {[<<"HEAD">>, <<"GET">>, <<"POST">>, <<"PUT">>, <<"OPTIONS">>], Req, NewState}.

%% FLOW: ALL[5] OR 400
%% Return whether the request is malformed.
malformed_request(Req, State) ->
  NewState = State#state{state = malformed_request},
  handle(Req, NewState).

%% FLOW: ALL[6] OR 401
%% Return whether the user is authorized to perform the action.
is_authorized(Req, State) ->
  NewState = State#state{state = is_authorized},
  handle(Req, NewState).

%% FLOW: ALL[7] OR 403
%% Return whether access to the resource is forbidden.
forbidden(Req, State = #state{isAuthorized = IsAuthorized}) ->
  {not IsAuthorized, Req, State}.

%% FLOW: ALL[10], OPTIONS[0] THEN 200
%% Handle a request for information.
options(Req, State) -> {ok, Req, State}.

%% FLOW: ALL[10] OR 406
%% Return the list of content-types the resource provides.
%% ProvideResource = iodata() | {stream, Fun} | {stream, Len, Fun} | {chunked, ChunkedFun}
content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle}], Req, State}.

%% FLOW: ALL[14], GET[0], HEAD[0], POST[0], PUT[0], PATCH[0], DELETE[0]
%% Return whether the resource exists.
resource_exists(Req, State = #state{ukey = Ukey}) ->
  {Ukey =/= undefined, Req, State}.

%% FLOW: ALL[15,17,19], POST[1,3,5], PUT[2], PATCH[4] THEN 201,204 OR 400
%% Return the list of content-types the resource accepts.
content_types_accepted(Req, State) ->
  NewState = State#state{state = types_accepted},
  {[{<<"application/json; charset=utf-8">>}], Req, NewState}.

handle(Req, State = #state{state = malformed_request}) ->

  OUkeyBin = cowboy_req:binding(oukey, Req, undefined),
  #{apiKey := OApiKeyBin, secret := OSecretBin} = cowboy_req:match_qs([
    {apiKey, [], undefined},
    {secret, [], undefined}
  ], Req),

  case OApiKeyBin =/= undefined andalso
    OSecretBin =/= undefined andalso
    OUkeyBin =/= undefined of
    true ->
      OApiKey = binary_to_list(OApiKeyBin),
      OSecret = binary_to_list(OSecretBin),
      OUkey = binary_to_list(OUkeyBin),

      NewState = State#state{apiKey = OApiKey, secret = OSecret, oukey = OUkey},
      {false, Req, NewState};
    false -> {true, Req, State}
  end;

handle(Req, State = #state{apiKey = OApiKey, secret = OSecret, state = is_authorized}) ->
  case user_server:checkuser(OApiKey, OSecret) of
    {ok, Ukey} ->
      NewState = State#state{ukey = Ukey, isAuthorized = true},
      {true, Req, NewState};
    {error, _} -> {true, Req, State}
  end;

handle(Req, State = #state{method = <<"GET">>, ukey = Ukey}) ->
  case user_server:getuser(Ukey) of
    {ok, UProfile} ->
      NewState = State#state{profile = UProfile},
      {jiffy:encode({[
        {profile, {UProfile}}
      ]}), Req, NewState};

    {error, _} -> {undefined, Req, State}
  end;

handle(Req, State = #state{method = <<"POST">>, ukey = Ukey, apiKey = OApiKey, secret = OSecret}) ->
  case user_server:add_aukey(Ukey, OApiKey, OSecret) of
    {ok, Resp} ->
      %% cowboy_req:set_resp_body(jiffy:encode({Registred})
      {jiffy:encode({Resp}), Req, State};
    _ -> {undefined, Req, State}
  end.
