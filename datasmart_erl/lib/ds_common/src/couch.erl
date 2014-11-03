%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(couch).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0,
  server_info/0,
  create_db/1,
  save/2,
  save/3,
  get/2,
  get/3,
  all/1,
  all/2,
  fetch/2,
  fetch/3,
  stream/3,
  attach/4,
  fetch_attachment/3,
  drop_attachment/3,
  stream_attach/2,
  stream_attach_done/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {server, dbList = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(server_info() -> {ok, Version :: term()}).
server_info() -> gen_server:call(?MODULE, {get_info}).

-spec(create_db(DBName :: term()) -> {ok, DB :: term()}).
create_db(DBName) -> gen_server:call(?MODULE, {create_db, DBName}).

-spec(save(DBName :: term(), Doc :: term()) -> {ok, Doc :: term()}).
save(DBName, Doc) -> gen_server:call(?MODULE, {save_doc, DBName, Doc}).

save(DBName, Doc, RefRev) ->
  {DocKVList} = Doc,
  gen_server:call(?MODULE, {
    save_doc, DBName, {lists:concat([
      RefRev,
      DocKVList
    ])}}).

-spec(get(DBName :: term(), Ref :: term()) -> {ok, Doc :: term()}).
get(DBName, Ref) -> gen_server:call(?MODULE, {get_doc, DBName, Ref, []}).

-spec(get(DBName :: term(), Ref :: term(), Rev :: term()) -> {ok, Doc :: term()}).
get(DBName, Ref, Rev) -> gen_server:call(?MODULE, {get_doc, DBName, Ref, [{rev, Rev}]}).

-spec(all(DBName :: term()) -> {ok, [Docs :: term()]}).
all(DBName) -> gen_server:call(?MODULE, {all_docs, DBName, []}).

-spec(all(DBName :: term(), included) -> {ok, [Docs :: term()]}).
all(DBName, included) -> gen_server:call(?MODULE, {all_docs, DBName, [include_docs]}).

-spec(fetch(DBName :: term(), {DesignName :: term(), ViewName :: term()}) -> {ok, [Docs :: term()]}).
%% http://127.0.0.1:5984/users/_design/rand/_view/random?startkey=[%22male%22,0.1]&endkey=[%22male%22,0.2]
%% couch:fetch("users", {"rand", "random"}, [{"startkey", "[\"male\", 0.1]"}, {"endkey", "[\"male\", 0.2]"}]).
fetch(DBName, {DesignName, ViewName}) ->
  gen_server:call(?MODULE, {fetch_docs, DBName, {DesignName, ViewName}, []}).

-spec(fetch(DBName :: term(),
    {DesignName :: term(), ViewName :: term()},
    Opts :: term()) -> {ok, [Docs :: term()]}).
fetch(DBName, {DesignName, ViewName}, Opts) ->
  gen_server:call(?MODULE, {fetch_docs, DBName, {DesignName, ViewName}, Opts}).

-spec(stream(DBName :: term(),
    {DesignName :: term(), ViewName :: term()},
    {Pid :: term(), ViewFun :: term()}) -> ok).
stream(DBName, {DesignName, ViewName}, {Pid, ViewFun}) ->
  gen_server:cast(?MODULE, {stream_docs, DBName, {DesignName, ViewName}, {Pid, ViewFun}}),
  ok.

-spec(attach(DBName :: term(), Ref :: term(),
    {Name :: term(), Attachment :: term()},
    Opts :: term()) -> {ok, Result :: term()}).
attach(DBName, Ref, {Name, Attachment}, Opts) ->
  gen_server:call(?MODULE, {attach_doc, DBName, Ref, {Name, Attachment}, Opts}).

stream_attach(Ref, Msg) ->
  gen_server:call(?MODULE, {stream_attach, Ref, Msg}).

stream_attach_done(Ref) ->
  gen_server:call(?MODULE, {stream_attach, Ref, eof}).

-spec(fetch_attachment(DBName :: term(), Ref :: term(), Name :: term()) ->
  {ok, Attachment :: term()}).
%% http://127.0.0.1:5984/randomface/6461aa999da007642fea644e7a01b68a/bigpreview_Curves.jpg
%% couch:fetch_attachment("randomface", "6461aa999da007642fea644e7a01b68a", "bigpreview_Curves.jpg").
fetch_attachment(DBName, Ref, Name) -> gen_server:call(?MODULE, {fetch_attachment, DBName, Ref, Name}).

-spec(drop_attachment(DBName :: term(), Ref :: term(), Name :: term()) -> ok).
drop_attachment(DBName, Ref, Name) -> gen_server:cast(?MODULE, {drop_attachment, DBName, Ref, Name}),
  ok.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([]) -> init(["http://localhost:5984"]);

init([Url]) ->
  Options = [],
  Server = couchbeam:server_connection(Url, Options),
  State = #state{server = Server, dbList = []},
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a DB (or just Open it)
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, {ok, DB :: term()}, NewState :: #state{}}).
handle_call({create_db, DBName}, _From, State) ->
  case db(State, DBName, []) of
    {ok, DB, State2} -> {reply, {ok, DB}, State2};
    Error -> {reply, {error, Error}, State}
  end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Save a Doc to a DB
%%
%% @end
%%--------------------------------------------------------------------
handle_call({save_doc, DBName, Doc}, _From, State) ->
  {ok, DB, State2} = db(State, DBName, []),

  case couchbeam:save_doc(DB, Doc) of
    {ok, Doc1} -> {reply, {ok, Doc1}, State2};
    Error -> {reply, {error, Error}, State2}
  end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% GET a Doc by ref from a DB
%%
%% @end
%%--------------------------------------------------------------------
handle_call({get_doc, DBName, Ref, Options}, _From, State) ->
  {ok, DB, State2} = db(State, DBName, Options),

  case couchbeam:open_doc(DB, Ref) of
    {ok, Doc1} -> {reply, {ok, Doc1}, State2};
    Error -> {reply, {error, Error}, State2}
  end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% LIST all Docs from a DB
%%
%% @end
%%--------------------------------------------------------------------
handle_call({all_docs, DBName, Options}, _From, State) ->
  {ok, DB, State2} = db(State, DBName, []),
  case couchbeam_view:fetch(DB, 'all_docs', Options) of
    {ok, AllDocs} -> {reply, {ok, AllDocs}, State2};
    Error -> {reply, {error, Error}, State2}
  end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% LIST Docs from a view of a DB
%%
%% @end
%%--------------------------------------------------------------------
handle_call({fetch_docs, DBName, {DesignName, ViewName}, Options}, _From, State) ->
  {ok, DB, State2} = db(State, DBName, []),

  case couchbeam_view:fetch(DB, {DesignName, ViewName}, Options) of
    {ok, AllDocs} -> {reply, {ok, AllDocs}, State2};
    Error -> {reply, {error, Error}, State2}
  end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set an Attachment to a Doc
%%
%% @end
%%--------------------------------------------------------------------
handle_call({attach_doc, DBName, Ref, {Name, Attachment}, Options}, _From, State) ->
  {ok, DB, State2} = db(State, DBName, []),

  case couchbeam:put_attachment(DB, Ref, Name, Attachment, Options) of
    {ok, Result} -> {reply, {ok, Result}, State2};
    Error -> {reply, {error, Error}, State2}
  end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set an Attachment to a Doc
%%
%% @end
%%--------------------------------------------------------------------
handle_call({stream_attach, Ref, Msg}, _From, State) ->
  case couchbeam:send_attachment(Ref, Msg) of
    {error, Error} -> {reply, {error, Error}, State};
    Result -> {reply, Result, State}
  end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set an Attachment to a Doc
%%
%% @end
%%--------------------------------------------------------------------
handle_call({fetch_attachment, DBName, Ref, Name}, _From, State) ->
  {ok, DB, State2} = db(State, DBName, []),

  case couchbeam:fetch_attachment(DB, Ref, Name) of
    {ok, Result} -> {reply, {ok, Result}, State2};
    Error -> {reply, {error, Error}, State2}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle Streamed rows
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({stream_docs, DBName, {DesignName, ViewName}, {Pid, ViewFun}}, State) ->
  {ok, DB, State2} = db(State, DBName, []),

  case couchbeam_view:stream(DB, {DesignName, ViewName}, Pid) of
    {ok, StartRef, ViewPid} ->
      ViewFun(StartRef, ViewFun, ViewPid),
      {noreply, State2};
    Error -> {noreply, State2} %% TODO Handle this somehow
  end;

handle_cast({drop_attachment, DBName, Ref, Name}, State) ->
  {ok, DB, State2} = db(State, DBName, []),

  case couchbeam:open_doc(DB, Ref) of
    {ok, Doc1} ->
      ok = couchbeam:delete_attachment(DB, Doc1, Name),
      {noreply, State2};
    Error -> {noreply, State2} %% TODO Handle this somehow
  end;

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

db(State, DBName, Options) ->
  CurrentList = case is_list(State#state.dbList) of
    false -> lists:flatten([State#state.dbList]);
    true -> State#state.dbList
  end,
  case lists:keyfind(DBName, 1, CurrentList) of
    {DBName, DB} -> {ok, DB, State};
    false ->
      {ok, DB} = couchbeam:open_or_create_db(State#state.server, DBName, Options),
      State2 = State#state{dbList = lists:append(CurrentList, [{DBName, DB}])},
      {ok, DB, State2}
  end.