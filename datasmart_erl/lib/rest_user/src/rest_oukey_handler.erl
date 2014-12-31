%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_oukey_handler).
-author("evangelosp").

-export([init/2]).
-export([
  allowed_methods/2,
  content_types_provided/2,
  is_authorized/2,
  known_methods/2,
  malformed_request/2,
  options/2,
  resource_exists/2,
  service_available/2,
  uri_too_long/2
]).
-export([handle/2]).

-record(state, {
  state = init, method,
  isAuthorized = false,
  is_conflict = false,
  oukey, ukey, profile, etag
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
is_authorized(Req, State = #state{oukey = _OUkey}) ->
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

%% FLOW: ALL[14], GET[0], HEAD[0], POST[0], PUT[0], PATCH[0], DELETE[0]
%% Return whether the resource exists.
resource_exists(Req, State = #state{ukey = Ukey}) ->
  {Ukey =/= undefined, Req, State}.

handle(Req, State = #state{method = <<"GET">>, state = malformed_request}) ->
  OUkeyBin = cowboy_req:binding(oukey, Req, undefined),

  case OUkeyBin =/= undefined of
    true ->
      OUkey = binary_to_list(OUkeyBin),

      NewState = State#state{oukey = OUkey},
      {false, Req, NewState};
    false -> {true, Req, State}
  end;

handle(Req, State = #state{method = <<"GET">>, oukey = OUkey, state = is_authorized}) ->
  case oukey_server:get_ukey({oukey, OUkey}) of
    {ok, Ukey} ->
      NewState = State#state{ukey = Ukey, isAuthorized = true},
      {true, Req, NewState};
    {error, _} -> {false, Req, State}
  end;

handle(Req, State = #state{method = <<"GET">>, ukey = Ukey, state = content_types_provided}) ->

  {ok, UProfile} = user_server:getuser(Ukey),

  NewState = State#state{profile = UProfile},

  {jiffy:encode({[
    {profile, {UProfile}}
  ]}), Req, NewState}.