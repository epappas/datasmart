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
-module(files_handler).
-author("evangelosp").

-include("file_records.hrl").

-export([init/2]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
  method, isAuthorized = false,
  is_conflict = false, etag,
  key_type, key
}).

init(Req, _Opts) ->
  handle(Req, #state{}).

handle(Req, State) ->
  Method = cowboy_req:method(Req),
  NewState = State#state{method = Method},
  %% TODO -> check AUTH token
  {ok, Req2} = process(Req, NewState),
  {ok, Req2, []}.

process(Req, #state{method = <<"GET">>, isAuthorized = true, key_type = KeyType, key = Key} = _State) ->
  QsVals = cowboy_req:parse_qs(Req),
  {FileKeyBin, _} = cowboy_req:binding(filekey, Req),
  {_, Attachment} = proplists:get_value(<<"attachment">>, QsVals, false),

  case FileKeyBin of
    undefined -> end_with_failure(400, "No Valid Arguments", Req);
    FileKeyBin ->
      FileKey = binary:bin_to_list(FileKeyBin),

      case Attachment of
        false -> %% When only File info is requested
          FileKVList = files_server:fetch_info(KeyType, Key, FileKey),
          end_with_success(FileKVList, Req);
        true -> %% When the actual file content is requested

          outstream_server:stream(KeyType, Key, FileKey, Req)
      end
  end;

process(Req, #state{method = <<"PUT">>, isAuthorized = true, key_type = KeyType, key = Key} = _State) ->
  case cowboy_req:parse_header(<<"content-type">>, Req) of
    {<<"multipart">>, <<"form-data">>, _} ->
      {ok, Req2, FieldList, FileList} = instream_server:stream(KeyType, Key, Req),

      echo(200, jiffy:encode({[
        {fieldList, {FieldList}},
        {fileList, {FileList}}
      ]}), Req2);

    _ -> end_with_failure(415, <<"Unsupported Media Type">>, Req)
  end;

process(_, Req) -> end_with_failure(405, "Method not allowed.", Req).

end_with_success(Message, Req) -> {ok, echo(200, jiffy:encode(Message), Req)}.

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

terminate(_Reason, _Req, _State) -> ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
