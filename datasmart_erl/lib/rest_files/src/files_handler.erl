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

  case cowboy_req:parse_header(<<"content-type">>, Req) of
    {ok, {<<"multipart">>, <<"form-data">>, _}, _Req} ->
      {ok, Req1, FieldList, FileList} = multipart(Req),

      echo(200, jiffy:encode({[
        {fieldList, {FieldList}},
        {fileList, {FileList}}
      ]}), Req1);

    _ -> end_with_failure(400, <<"Bad content-type; Accepts only multipart">>, Req)
  end;

process(_, Req) -> end_with_failure(405, "Method not allowed.", Req).

handle_query({file_info, OUkey, FileKey}, Req) -> {info, [], Req};

handle_query({file_upload, OUkey}, Req) ->
  {info, [], Req}.

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

terminate(_Reason, _Req, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

multipart(Req) -> multipart(Req, [], []).

multipart(Req, FieldList, FileList) ->
  case cowboy_req:part(Req) of
    {ok, Headers, Req2} ->
      {ok, Req4, NewFieldList, NewFileList} =
        case cow_multipart:form_data(Headers) of
          {data, FieldName} ->
            {ok, Body, Req3} = cowboy_req:part_body(Req2),

            FieldList1 = lists:append(FieldList, [{FieldName, Body}]),

            {ok, Req3, FieldList1, FileList};
          {file, FieldName, Filename, CType, CTransferEncoding} ->

            {ok, TMPFname, Req3} = stream_file(Req2),

            FileList1 = lists:append([{Filename, {[
              {<<"fieldName">>, FieldName},
              {<<"filename">>, Filename},
              {<<"ctype">>, CType},
              {<<"ctransferEncoding">>, CTransferEncoding},
              {<<"tmpFile">>, TMPFname}
            ]}}], FileList),

            {ok, Req3, FieldList, FileList1}
        end,
      multipart(Req4, NewFieldList, NewFileList);
    {done, Req2} ->
      {ok, Req2, FieldList, FileList}
  end.

stream_file(Req) ->
  {A, B, C} = now(),
  Rand = binary_to_list(base64:encode(binary_to_list(crypto:strong_rand_bytes(10)))),
  Fname = lists:flatten(io_lib:format("~p.~p.~p.~s.tmp", [A, B, C, Rand])),
  stream_file(Req, list_to_binary(Fname)).

stream_file(Req, Fname) ->
  case cowboy_req:part_body(Req) of
    {ok, Body, Req2} ->
      file:write_file(Fname, Body, [append, raw, binary]),
      {ok, Fname, Req2};
    {more, Body, Req2} ->
      file:write_file(Fname, Body, [append, raw, binary]),
      stream_file(Req2, Fname)
  end.