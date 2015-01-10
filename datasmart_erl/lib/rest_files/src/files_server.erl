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
-module(files_server).
-author("evangelosp").

-include("file_records.hrl").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  fetch_info/3,
  outStream_file/4
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

fetch_info(AuthKeyType, AuthKey, FileKey) ->
  gen_server:call(?MODULE, {fetch_info, {AuthKeyType, AuthKey, FileKey}}).

outStream_file(AuthKeyType, AuthKey, FileKey, Req) ->
  gen_server:call(?MODULE, {outStream_file, {AuthKeyType, AuthKey, FileKey, Req}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({fetch_info, {AuthKeyType, AuthKey, FileKey}}, _From, State) ->
  {reply, doFetchInfo(AuthKeyType, AuthKey, FileKey), State};

handle_call({outStream_file, {AuthKeyType, AuthKey, FileKey, Req}}, _From, State) ->
  {reply, doStreamFileOutbound(AuthKeyType, AuthKey, FileKey, Req), State};

handle_call(_Request, _From, State) ->
  {reply, {}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

doFetchInfo(_AuthKeyType, _AuthKey, FileKey) ->
  case couch:get(?couch_file_uploads, FileKey) of
    {ok, FileJson} ->
      {FileKVList} = FileJson,
      {ok, FileKVList};
    _ -> {error, not_found}
  end.

doStreamFileOutbound(AuthKeyType, AuthKey, FileKey, Req) ->
  case doFetchInfo(AuthKeyType, AuthKey, FileKey) of
    {ok, FileKVList} ->
      case couch:get(?couch_file_secrets, FileKey) of
        {ok, EssentialsJson} ->
          {EKVList} = EssentialsJson,

          AESKey64 = proplists:get_value(<<"aes_key">>, EKVList),
          AESIV64 = proplists:get_value(<<"aes_iv">>, EKVList),
          AESKey = base64:decode(AESKey64),
          AESIV = base64:decode(AESIV64),

          CType = proplists:get_value(<<"ctype">>, FileKVList),
          FileName = proplists:get_value(<<"filename">>, FileKVList),

          case couch:fetch_attachment_stream(?couch_file_uploads, FileKey, FileName) of
            {ok, StreamRef} ->

              EncrState = crypto:stream_init(aes_ctr, AESKey, AESIV),

              StreamFun = fun(SendChunk) ->
                outStream_loop_file(SendChunk, StreamRef, EncrState, Req)
              end,
              Req2 = cowboy_req:set_resp_body_fun(chunked, StreamFun, Req),

              cowboy_req:reply(200, [
                {<<"content-type">>, CType},
                {<<"server">>, <<"myinbox-datastore">>}
              ], Req2);
            _ -> {error, not_found}
          end;
        _ -> {error, not_found}
      end;
    _ -> {error, not_found}
  end.

outStream_loop_file(SendChunk, StreamRef, EncrState, Req) ->
  case couchbeam:stream_attachment(StreamRef) of
    {ok, EncrResult} ->
      {NewEncrState, Result} = crypto:stream_decrypt(EncrState, EncrResult),

      SendChunk(Result),

      outStream_loop_file(SendChunk, StreamRef, NewEncrState, Req);
    done -> {ok, Req};
    {error, Err} ->
      %% TODO Log this
      end_with_failure(500, "Transport issue", Req),
      {error, Err}
  end.

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