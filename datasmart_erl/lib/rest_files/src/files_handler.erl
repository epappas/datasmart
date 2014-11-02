%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(files_handler).
-author("evangelosp").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  process(Method, Req),
  {ok, Req, State}.

process(<<"GET">>, Req) ->
  case handle_query({file_info, "OUkey", "FileKey"}, Req) of
    {info, FileInfo, Req1} -> echo(200, jiffy:encode({FileInfo}), Req1);
    _ -> end_with_failure(404, "Resource not found", Req)
  end;

process(<<"POST">>, Req) ->
  {OUkeyBin, Req2} = cowboy_req:binding(oukey, Req),

  case OUkeyBin of
    undefined -> end_with_failure(400, "No Valid Arguments", Req2);
    OUkeyBin ->
      OUkey = binary:bin_to_list(OUkeyBin),
      {ok, Ukey} = user_server:match_ouKey(OUkey),
      case user_server:match_ouKey(OUkey) of
        {ok, Ukey} ->
          case cowboy_req:parse_header(<<"content-type">>, Req2) of
            {ok, {<<"multipart">>, <<"form-data">>, _}, Req3} ->

              {ok, Req4, FieldList, FileList} = multipart(Ukey, Req3),

              echo(200, jiffy:encode({[
                {fieldList, {FieldList}},
                {fileList, {FileList}}
              ]}), Req4);

            _ -> end_with_failure(400, <<"Bad content-type; Accepts only multipart">>, Req)
          end;
        _ -> end_with_failure(400, "No Valid Arguments", Req2)
      end
  end;

process(_, Req) -> end_with_failure(405, "Method not allowed.", Req).

handle_query({file_info, _OUkey, _FileKey}, Req) -> {info, [], Req};

handle_query({file_upload, _OUkey}, Req) -> {info, [], Req}.

echo(Status, Echo, Req) ->
  cowboy_req:reply(Status, [
    {<<"content-type">>, <<"application/json; charset=utf-8">>},
    {<<"server">>, <<"myinbox-datastore">>}
  ], Echo, Req).

end_with_failure(Code, Message, Req) ->
  echo(Code, jiffy:encode({[
    {code, Code},
    {status, error},
    {error, Message}
  ]}), Req).

terminate(_Reason, _Req, _State) -> ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

multipart(UKey, Req) -> multipart(UKey, Req, [], []).

multipart(UKey, Req, FieldList, FileList) ->
  case cowboy_req:part(Req) of
    {ok, Headers, Req2} ->
      {ok, Req4, NewFieldList, NewFileList} =
        case cow_multipart:form_data(Headers) of
          {data, FieldName} ->
            {ok, Body, Req3} = cowboy_req:part_body(Req2),

            FieldList1 = lists:append(FieldList, [{FieldName, Body}]),

            {ok, Req3, FieldList1, FileList};
          {file, FieldName, Filename, CType, CTransferEncoding} ->

            FileKey = lists:concat([
              binary_to_list(UKey),
              "_",
              hash_md5:build(binary_to_list(Filename))
            ]),

            Opts =
              case couch:get("user_uploads", FileKey) of
                {ok, {FileKVList}} ->
                  case proplists:get_value(<<"_rev">>, FileKVList) of
                    undefined -> [];
                    Rev -> [{rev, Rev}]
                  end;
                {error, {error, not_found}} -> []
              end,

            %% initiate attachmanet upload
            {ok, UploadRef} = couch:attach("user_uploads", FileKey, {Filename, stream}, Opts),

            %% commit upload
            {ok, {ResultList}, Req3} = stream_file(Req2, UploadRef),

            Rev1 = proplists:get_value(<<"rev">>, ResultList),
            Id = proplists:get_value(<<"id">>, ResultList),

            %% get current view
            {ok, {DocKVList}} = couch:get("user_uploads", Id, Rev1),

            %% append meta info
            Doc = {lists:concat([
              DocKVList,
              [
                %% {<<"_id">>, Id},
                %% {<<"_rev">>, Rev1},
                {<<"ukey">>, UKey},
                {<<"fileKey">>, list_to_binary(FileKey)},
                {<<"fieldName">>, FieldName},
                {<<"filename">>, Filename},
                {<<"ctype">>, CType},
                {<<"ctransferEncoding">>, CTransferEncoding}
              ]
            ])},

            {ok, ResultDoc} = couch:save("user_uploads", Doc),

            FileList1 = lists:append([{Filename, ResultDoc}], FileList),

            {ok, Req3, FieldList, FileList1}
        end,
      multipart(UKey, Req4, NewFieldList, NewFileList);
    {done, Req2} ->
      {ok, Req2, FieldList, FileList}
  end.

stream_file(Req, UploadRef) ->
  case cowboy_req:part_body(Req) of
    {ok, Body, Req2} ->
      ok = couch:stream_attach(UploadRef, Body),
      {ok, Result} = couch:stream_attach_done(UploadRef),
      {ok, Result, Req2};
    {more, Body, Req2} ->
      ok = couch:stream_attach(UploadRef, Body),
      stream_file(Req2, UploadRef)
  end.