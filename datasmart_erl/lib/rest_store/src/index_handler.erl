%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(index_handler).
-author("epappas").

-export([init/2]).
-export([
  allowed_methods/2,
  content_types_provided/2,
  get_json/2
]).

init(Req, Opts) -> {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"HEAD">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, get_json}], Req, State}.

get_json(Req, State) ->
  Req2 = cowboy_req:set_resp_header(<<"server">>, <<"myinbox-datastore">>, Req),
  {jiffy:encode({[{status, ok}]}), Req2, State}.
