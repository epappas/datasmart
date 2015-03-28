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
-module(multipartstream_server).
-author("epappas").

-include("file_records.hrl").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  stream/3
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

stream(AuthKeyType, AuthKey, Req) ->
  gen_server:call(?MODULE, {stream, {AuthKeyType, AuthKey, Req}}).

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

handle_call({stream, {AuthKeyType, AuthKey, Req}}, _From, State) ->
  {reply, multipart(AuthKeyType, AuthKey, Req), State};

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

multipart(AuthKeyType, AuthKey, Req) -> multipart(AuthKeyType, AuthKey, Req, [], []).

multipart(AuthKeyType, AuthKey, Req, FieldList, FileList) ->
  case cowboy_req:part(Req) of
    {ok, Headers, Req2} ->
      {ok, Req4, NewFieldList, NewFileList} =
        case cow_multipart:form_data(Headers) of
          {data, FieldName} -> %% Sneaky, the users wishes to store raw secret information :P
            {ok, Body, Req3} = cowboy_req:part_body(Req2),

            FieldList1 = lists:append(FieldList, [{FieldName, Body}]),

            {ok, Req3, FieldList1, FileList};
          {file, FieldName, Filename, CType, CTransferEncoding} -> %% A file/binary detected, lets encrypt everything
            %% The multipart is a file, lets create security essentials
            AESKey = crypto:strong_rand_bytes(32), %% 256 bts long
            AESIV = crypto:strong_rand_bytes(16),
            FileKey = ds_util:uuid(),
            FileKeyBin = list_to_binary(FileKey),

            %% Initiate crypto stream
            CryptoState = crypto:stream_init(aes_ctr, AESKey, AESIV),
            %% Initiate hash contexts
            MD5_Context = crypto:hmac_init(md5, FileKey),
            SHA_Context = crypto:hmac_init(sha, FileKey),
            SHA512_Context = crypto:hmac_init(sha512, FileKey),

            %% Check if file exist, if yes, state the version of the doc
            Opts =
              case couch:get(?couch_file_uploads, FileKey) of
                {ok, {FileKVList}} ->
                  case proplists:get_value(<<"_rev">>, FileKVList) of
                    undefined -> [];
                    Rev -> [{rev, Rev}]
                  end;
                {error, {error, not_found}} -> []
              end,

            %% initiate attachmanet upload
            {ok, UploadRef} = couch:attach(?couch_file_uploads, FileKey, {Filename, stream}, Opts),

            %% commit upload
            {ok, {ResultList}, Req3, {_, MD5, SHA, SHA512}} =
              inStream_loop_file(Req2, UploadRef, {
                CryptoState, MD5_Context,
                SHA_Context, SHA512_Context
              }),

            Rev1 = proplists:get_value(<<"rev">>, ResultList),
            Id = proplists:get_value(<<"id">>, ResultList),

            %% get current view
            {ok, {DocKVList}} = couch:get(?couch_file_uploads, Id, Rev1),

            %% append meta info
            Doc = {lists:concat([
              DocKVList,
              [
                %% {<<"keyType">>, AuthKeyType},
                {<<"key">>, AuthKey},
                {<<"fileKey">>, FileKeyBin},
                {<<"fieldName">>, FieldName},
                {<<"filename">>, Filename},
                {<<"ctype">>, CType},
                {<<"ctransferEncoding">>, CTransferEncoding},
                {<<"md5">>, MD5},
                {<<"sha">>, SHA},
                {<<"sha512">>, SHA512}
              ]
            ])},

            %% Commit the document
            {ok, ResultDoc} = couch:save(?couch_file_uploads, Doc),

            %% Commit the security details, check for race condition
            case couch:get(?couch_file_secrets, FileKey) of
              {ok, {EssKVList}} ->
                Ref = proplists:get_value(<<"_id">>, EssKVList),
                Rev2 = proplists:get_value(<<"_rev">>, EssKVList),
                couch:save(?couch_file_secrets,
                  {[{<<"aes_key">>, base64:encode(AESKey)},
                    {<<"aes_iv">>, base64:encode(AESIV)},
                    {<<"fileKey">>, FileKeyBin}]},
                  [{<<"_id">>, Ref}, {<<"_rev">>, Rev2}]);
              _ ->
                couch:save(?couch_file_secrets,
                  {[{<<"_id">>, FileKeyBin},
                    {<<"aes_key">>, base64:encode(AESKey)},
                    {<<"aes_iv">>, base64:encode(AESIV)},
                    {<<"fileKey">>, FileKeyBin}]})
            end,

            %% Generate a responce list
            FileList1 = lists:append([{Filename, ResultDoc}], FileList),

            {ok, Req3, FieldList, FileList1}
        end,
      %% Next spin, Next multipart of the REQ
      multipart(AuthKeyType, AuthKey, Req4, NewFieldList, NewFileList);
    {done, Req2} -> %% All done :D
      {ok, Req2, FieldList, FileList}
  end.

%% Handy loop
inStream_loop_file(Req, UploadRef, {
  CryptoState, MD5_Context,
  SHA_Context, SHA512_Context
}) ->
  case cowboy_req:part_body(Req) of
    {ok, Body, Req2} ->
      {NewCryptoState, CipherBody} = crypto:stream_encrypt(CryptoState, Body),
      NewMD5_Context = crypto:hmac_update(MD5_Context, Body),
      NewSHA_Context = crypto:hmac_update(SHA_Context, Body),
      NewSHA512_Context = crypto:hmac_update(SHA512_Context, Body),

      MD5 = crypto:hmac_final(MD5_Context),
      SHA = crypto:hmac_final(SHA_Context),
      SHA512 = crypto:hmac_final(SHA512_Context),

      ok = couch:stream_attach(UploadRef, CipherBody),
      {ok, Result} = couch:stream_attach_done(UploadRef),

      {ok, Result, Req2, {NewCryptoState, MD5, SHA, SHA512}};
    {more, Body, Req2} ->
      {NewCryptoState, CipherBody} = crypto:stream_encrypt(CryptoState, Body),
      NewMD5_Context = crypto:hmac_update(MD5_Context, Body),
      NewSHA_Context = crypto:hmac_update(SHA_Context, Body),
      NewSHA512_Context = crypto:hmac_update(SHA512_Context, Body),

      ok = couch:stream_attach(UploadRef, CipherBody),

      inStream_loop_file(Req2, UploadRef, {
        NewCryptoState, NewMD5_Context,
        NewSHA_Context, NewSHA512_Context
      })
  end.