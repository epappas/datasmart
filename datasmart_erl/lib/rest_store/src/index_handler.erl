%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(index_handler).
-author("epappas").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, _Opts) ->
  {ok, Req, undefined}.


handle(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {SomeKey, Req2} = cowboy_req:binding(key, Req1),

  cowboy_req:reply(200, [
    {<<"content-type">>, <<"application/json; charset=utf-8">>},
    {<<"server">>, <<"myinbox-datastore">>}
  ], jiffy:encode({[
    {status, ok},
    {method, Method},
    {key, SomeKey}
  ]}), Req2),

  {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
  ok.