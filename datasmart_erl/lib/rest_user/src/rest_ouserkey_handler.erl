%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_ouserkey_handler).
-author("evangelosp").

-export([init/2]).
-export([
  allowed_methods/2,
  allow_missing_post/2,
  charsets_provided/2,
  content_types_accepted/2,
  content_types_provided/2,
  delete_completed/2,
  delete_resource/2,
  expires/2,
  forbidden/2,
  generate_etag/2,
  is_authorized/2,
  is_conflict/2,
  known_methods/2,
  languages_provided/2,
  last_modified/2,
  malformed_request/2,
  moved_permanently/2,
  moved_temporarily/2,
  multiple_choices/2,
  options/2,
  previously_existed/2,
  resource_exists/2,
  service_available/2,
  uri_too_long/2,
  valid_content_headers/2,
  valid_entity_length/2,
  variances/2
]).
-export([handle/2]).

-record(state, {
  method, isAuthorized = false,
  oukey, ukey, profile, etag
}).

%% FLOW: ALL[0]
init(Req, _Opts) ->
  {cowboy_rest, Req, #state}.

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
  OUkeyBin = cowboy_req:binding(oukey, Req, undefined),

  case OUkeyBin =/= undefined of
    true ->
      OUkey = binary_to_list(OUkeyBin),

      NewState = State#state{oukey = OUkey},
      {false, Req, NewState};
    fasle -> {true, Req, State}
  end.

%% FLOW: ALL[6] OR 401
%% Return whether the user is authorized to perform the action.
is_authorized(Req, State#state{oukey = OUkey}) ->
  case user_server:match_ouKey(OUkey) of
    {ok, Ukey} ->
      NewState = State#state{ukey = Ukey, isAuthorized = true},
      {true, Req, NewState};
    {error, _} -> {true, Req, State}
  end.

%% FLOW: ALL[7] OR 403
%% Return whether access to the resource is forbidden.
forbidden(Req, State#state{isAuthorized = IsAuthorized}) ->
  {IsAuthorized, Req, State}.

%% FLOW: ALL[8] OR 501
%% Return whether the content-* headers are valid.
valid_content_headers(Req, State) -> {true, Req, State}.

%% FLOW: ALL[9] OR 413
%% Return whether the request body length is within acceptable boundaries.
valid_entity_length(Req, State) -> {true, Req, State}.

%% FLOW: ALL[10], OPTIONS[0] THEN 200
%% Handle a request for information.
options(Req, State) -> {ok, Req, State}.

%% FLOW: ALL[10] OR 406
%% Return the list of content-types the resource provides.
%% ProvideResource = iodata() | {stream, Fun} | {stream, Len, Fun} | {chunked, ChunkedFun}
content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle}], Req, State}.

%% FLOW: ALL[11] OR 406
%% Return the list of languages the resource provides.
languages_provided(Req, State) -> {[<<"en">>], Req, State}.

%% FLOW: ALL[12] OR 406
%% Return the list of charsets the resource provides.
charsets_provided(Req, State) -> {[<<"utf-8">>], Req, State}.

%% FLOW: ALL[13]
%% Return the list of headers that affect the representation of the resource.
variances(Req, State) -> {[], Req, State}.

%% FLOW: ALL[14], GET[0], HEAD[0], POST[0], PUT[0], PATCH[0], DELETE[0]
%% Return whether the resource exists.
resource_exists(Req, State#state{ukey = Ukey}) ->
  {Ukey =/= undefined, Req, State}.

%% FLOW: IF resource_exists =:= false ALL[15], GET[1], HEAD[1], POST[1], PATCH[1], DELETE[1] THEN 404
%% Return whether the resource existed previously.
previously_existed(Req, State) -> {false, Req, State}.

%% FLOW: IF previously_existed =:= false ALL[16,18], POST[2,4] OR 404,410
%% Return whether POST is allowed when the resource doesn't exist.
%% Returning true here means that a new resource will be created.
%% The URL to the created resource should also be returned from the AcceptResource callback.
allow_missing_post(Req, State) -> {true, Req, State}.

%% FLOW: IF previously_existed =:= false ALL[16], PUT[1] THEN 409
%% Return whether the put action results in a conflict.
is_conflict(Req, State) -> {true, Req, State}.

%% FLOW: IF previously_existed =:= true ALL[16], GET[2], HEAD[2], POST[2], PATCH[2], DELETE[2] THEN 301
%% Return whether the request is malformed.
moved_permanently(Req, State) -> {false, Req, State}.

%% FLOW: IF moved_permanently =:= false ALL[17], GET[3], HEAD[3], POST[3], PATCH[3], DELETE[3] THEN 307 ELSE 410
%% Return whether the request is malformed.
moved_temporarily(Req, State) -> {false, Req, State}.

%% FLOW: ALL[15], DELETE[1] OR 500
%% Delete the resource.
delete_resource(Req, State) -> {true, Req, State}.

%% FLOW: ALL[16], DELETE[2] THEN 204 OR 202
%% Return whether the delete action has been completed.
delete_completed(Req, State) -> {true, Req, State}.

%% FLOW: ALL[15], GET[1], HEAD[1]
%% Return the entity tag of the resource.
generate_etag(Req, State#state{etag = Etag}) -> {Etag, Req, State}.

%% FLOW: ALL[16], GET[2], HEAD[2]
%% Return the date of last modification of the resource.
last_modified(Req, State) -> {os:timestamp(), Req, State}.

%% FLOW: ALL[17], GET[3], HEAD[3]
%% Return the date of expiration of the resource.
expires(Req, State) -> {undefined, Req, State}.

%% FLOW: ALL[16,18,20], GET[4], HEAD[4], POST[2,4,6], PUT[3], PATCH[5], DELETE[3] THEN 200 OR 300
%% Return whether there are multiple representations of the resource.
multiple_choices(Req, State) -> {false, Req, State}.

%% FLOW: ALL[15,17,19], POST[1,3,5], PUT[2], PATCH[4] THEN 201,204 OR 400
%% Return the list of content-types the resource accepts.
content_types_accepted(Req, State) ->
  {[{<<"application/json; charset=utf-8">>}], Req, State}.

handle(Req, State#state{method = <<"GET">>, ukey = Ukey}) ->

  {ok, UProfile} = user_server:getuser(Ukey),

  NewState = State#state{profile = UProfile},

  {jiffy:encode({[
    {profile, {UProfile}}
  ]}), Req, NewState}.