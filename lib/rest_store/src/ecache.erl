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
-module(ecache).
-author("epappas").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  new/1,
  new/2,
  put/3,
  put/4,
  get/2,
  get/3,
  purge/2,
  drop/1
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

-export_type([ets_cache/0]).

-define(SERVER, ?MODULE).
-define(CACHE_SIZE, cache_size).
-define(NAME_POOL, ecache_name_pool).
-define(DEFAULT_MAX, 2000).

-record(state, {cacheKVList = []}).

%% @doc Record for ets_cache.
%% It has 2 ETS tables, a maximum size and the current size.
-record(cache, {
  name :: key(),
  max_size :: non_neg_integer(),
  table :: ets:tab(),
  itable :: ets:tab()
}).

%% @doc Row includes a key, a value and last modified timestamp
-record(row, {
  key :: key(),
  ts :: timestamp(),
  value :: value()
}).

%% @doc Inverted Row, ordered by timestamp to sort DESC by the timestamp (older).
-record(irow, {
  ts :: timestamp(),
  key :: key()
}).

-opaque ets_cache() :: #cache{}.
-type key() :: any().
-type value() :: any().
-type timestamp() :: pos_integer().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(new(DBName :: term()) -> {ok, Cache :: #cache{}}).
new(DBName) -> new(DBName, ?DEFAULT_MAX).

-spec(new(DBName :: term(), Size :: number()) -> {ok, DB :: term()}).
new(DBName, Size) -> gen_server:call(?MODULE, {new, DBName, Size}).

%% @doc Inserts data in cache with the current timestamp.
-spec put(DBName :: term(), key(), value()) -> {ok, timestamp()}.
put(DBName, Key, Value) -> put(DBName, Key, Value, ds_util:timestamp()).

%% @doc Inserts data in cache with a given timestamp.
-spec put(DBName :: term(), key(), value(), timestamp()) -> {ok, timestamp()}.
put(DBName, Key, Value, Time) -> gen_server:call(?MODULE, {put, DBName, Key, Value, Time}).

-spec get(DBName :: term(), key()) -> {ok, value()} | {not_found, undefined} | {expired, undefined}.
get(DBName, Key) -> get(DBName, Key, undefined).

%% @doc Gets data given the key, if not expired. Otherwise Default is given
-spec get(ets_cache(), key(), Default :: (undefined | any())) -> {ok, value()} | {not_found, Default} | {expired, Default}.
get(DBName, Key, Default) -> gen_server:call(?MODULE, {get, DBName, Key, Default}).

-spec purge(DBName :: term(), key()) -> ok.
purge(DBName, Key) -> gen_server:call(?MODULE, {purge, DBName, Key}).

-spec drop(DBName :: term()) -> ok.
drop(DBName) -> gen_server:call(?MODULE, {drop, DBName}).

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
init([]) ->
  %% Check if the ?NAME_POOL table has been created -- first worker's job
  case lists:member(?NAME_POOL, ets:all()) of
    false ->
      ets:new(?NAME_POOL, [named_table, set, protected, {keypos, #cache.name}]);
    _ -> ok
  end,
  {ok, #state{cacheKVList = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({new, DBName, Size}, _From, State) ->
  %% Check if Table exists in the pool
  case ets:lookup(?NAME_POOL, DBName) of
    [] -> %% Cache doesn't pre-exist, lets create one
      Table = ets:new(table, [set, private, {keypos, #row.key}]),
      ITable = ets:new(itable, [ordered_set, private, {keypos, #irow.ts}]),
      ets:insert(Table, {dummy, ?CACHE_SIZE, 0}),
      Cache = #cache{
        name = DBName,
        max_size = Size,
        table = Table,
        itable = ITable
      },
      ets:insert(?NAME_POOL, Cache),
      {reply, {ok, Cache}, State};
    [_] -> {reply, {duplicate, DBName}, State}
  end;

handle_call({put, DBName, Key, Value, Time}, _From, State) ->
  %% Check if Table exists in the pool
  case ets:lookup(?NAME_POOL, DBName) of
    [Cache] -> %% Cache exists, lets insert
      #cache{max_size = MaxSize, table = Tab, itable = ITab} = Cache,

      % try to insert data in primary table -> O(1)
      case ets:insert_new(Tab, #row{key = Key, ts = Time, value = Value}) of
      % key already exists
        false ->
          % insert data in primary table -> O(1)
          ets:insert(Tab, #row{key = Key, ts = Time, value = Value}),
          % delete current timestamp for the key
          ets:match_delete(Tab, #irow{ts = '_', key = Key});
      % the key was new and it was inserted
        true ->
          % get the cache's current size
          CurrentSize = ets:lookup_element(Tab, ?CACHE_SIZE, 3),
          % test if max size of cache has been reached
          case CurrentSize >= MaxSize of
          % there is sufficient size
            false ->
              ets:update_counter(Tab, ?CACHE_SIZE, 1);
          % need to prune data
            true ->
              % get oldest timestamp
              case ets:slot(ITab, 0) of
                [RIT] -> %% delete timestamp
                  ets:delete(ITab, RIT#irow.ts),
                  %% delete key
                  ets:delete(Tab, RIT#irow.key);
                '$end_of_table' -> ok
              end
          end
      end,
      % insert new timestamp for the key -> O(log(N))
      ets:insert(ITab, #irow{ts = Time, key = Key}),

      {reply, {ok, Time}, State};
    [] -> {reply, {unkonwn_db, DBName}, State}
  end;

handle_call({get, DBName, Key, Default}, _From, State) ->
  %% Check if Table exists in the pool
  case ets:lookup(?NAME_POOL, DBName) of
    [Cache] -> %% Cache exists
      #cache{max_size = _MaxSize, table = Tab, itable = _ITab} = Cache,

      % lookup key
      case ets:lookup(Tab, Key) of
        [Row] -> {reply, {ok, Row#row.value}, State};
        [] -> {reply, {not_found, Default}, State}
      end;

    [] -> {reply, {unkonwn_db, DBName}, State}
  end;

handle_call({purge, DBName, Key}, _From, State) ->
  %% Check if Table exists in the pool
  case ets:lookup(?NAME_POOL, DBName) of
    [Cache] -> %% Cache exists
      #cache{max_size = _MaxSize, table = Tab, itable = ITab} = Cache,

      ets:match_delete(ITab, #irow{ts = '_', key = Key}),
      ets:match_delete(Tab, #row{ts = '_', key = Key}),

      {reply, ok, State};

    [] -> {reply, {unkonwn_db, DBName}, State}
  end;

handle_call({drop, DBName}, _From, State) ->
  %% Check if Table exists in the pool
  case ets:lookup(?NAME_POOL, DBName) of
    [Cache] -> %% Cache exists
      #cache{max_size = _MaxSize, table = Tab, itable = ITab} = Cache,

      ets:delete(Tab),
      ets:delete(ITab),
      ets:match_delete(?NAME_POOL, #cache{name = DBName}),

      {reply, ok, State};

    [] -> {reply, {unkonwn_db, DBName}, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
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
