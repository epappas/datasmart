%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(qredis).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0, q/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


q(Req) ->
  gen_server:call(?MODULE, Req).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, Conn} = eredis:start_link(),
  {ok, Conn}.

handle_call(Request, _From, Conn) ->
  {reply, eredis:q(Conn, Request), Conn}.

handle_cast(_Request, Conn) ->
  {noreply, Conn}.

handle_info(_Info, Conn) ->
  {noreply, Conn}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
