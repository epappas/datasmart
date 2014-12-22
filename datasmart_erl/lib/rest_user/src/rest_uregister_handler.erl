%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_uregister_handler).
-author("evangelosp").

-export([init/2]).
-export([
  allowed_methods/2,
  content_types_accepted/2,
  forbidden/2,
  is_authorized/2,
  known_methods/2,
  options/2,
  service_available/2
]).
-export([handle/2]).

-record(state, {
  state = init, method, isAuthorized = true, is_conflict = false,
  etag, email, password, registred
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
  {[<<"POST">>, <<"OPTIONS">>], Req, NewState}.

%% FLOW: ALL[6] OR 401
%% Return whether the user is authorized to perform the action.
is_authorized(Req, State) ->
  NewState = State#state{state = authorization},
  {true, Req, NewState}.

%% FLOW: ALL[7] OR 403
%% Return whether access to the resource is forbidden.
forbidden(Req, State = #state{isAuthorized = IsAuthorized}) ->
  {not IsAuthorized, Req, State}.

%% FLOW: ALL[10], OPTIONS[0] THEN 200
%% Handle a request for information.
options(Req, State) -> {ok, Req, State}.

%% FLOW: ALL[15,17,19], POST[1,3,5], PUT[2], PATCH[4] THEN 201,204 OR 400
%% Return the list of content-types the resource accepts.
content_types_accepted(Req, State) ->
  NewState = State#state{state = types_accepted},
  {[{{<<"application">>, <<"json">>, '*'}, handle}], Req, NewState}.

handle(Req, State = #state{method = <<"POST">>}) ->
  {ok, JsonBin, _} = cowboy_req:body(Req),

  {QsKVList} = jiffy:decode(JsonBin),

  {_, EmailBin} = lists:keyfind(<<"email">>, 1, QsKVList),

  case EmailBin =/= undefined of
    true ->
      Email = binary_to_list(EmailBin),

      case user_server:register(Email) of
        {ok, Registred} ->
          NewState = State#state{registred = Registred},
          {true, cowboy_req:set_resp_body(jiffy:encode({Registred}), Req), NewState};
        _ -> {false, Req, State}
      end;
    false ->
      {stop, Req, State}
  end.
