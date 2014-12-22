%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(atoken_server).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
  generate/1,
  check/1,
  get_aukey/1
]).

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

-include("user_records.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

check(AToken) -> gen_server:call(?MODULE, {check, AToken}).

get_aukey({Type, Value}) -> gen_server:call(?MODULE, {get_aukey, {Type, Value}}).

generate(#ukey_generate{} = Token) -> gen_server:call(?MODULE, {generate, Token}).

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

handle_call({generate, #atoken_generate{aukey = AUKey, scope = Scope, expires = Expires}}, _From, State) ->

  AToken = srp_server:new_salt(),
  Salt = srp_server:new_salt(),

  couch:save(?couch_atokens, {[
    {<<"_id">>, list_to_binary(AToken)},
    {<<"key">>, list_to_binary(AUKey)},
    {<<"salt">>, list_to_binary(Salt)},
    {<<"expires">>, list_to_binary(Expires)},
    {<<"scope">>, Scope}
  ]}),

  {reply, {ok, [
    {atoken, list_to_binary(AToken)},
    {opensalt, list_to_binary(Salt)},
    {scope, Scope}
  ]}, State};

handle_call({check, AToken}, _From, State) ->
  case couch:get(?couch_atokens, AToken) of
    {ok, DocJson} ->
      {DocKVList} = DocJson,
      Salt = proplists:get_value(<<"salt">>, DocKVList),
      Scope = proplists:get_value(<<"scope">>, DocKVList, []),
      ExpiresBin = proplists:get_value(<<"expires">>, DocKVList, <<"0">>),
      Expires = binary_to_integer(ExpiresBin),

      case ds_util:timestamp() > Expires of
        true ->
          %% TODO drop the doc
          {reply, {error, "Expired"}, State};
        false ->
          {reply, {ok, [
            {opensalt, list_to_binary(Salt)},
            {scope, Scope},
            {expires, Expires}
          ]}, State}
      end;
    {error, Error} ->
      {reply, {error, Error}, State}
  end;

handle_call({get_aukey, {atoken, AToken}}, _From, State) ->
  case couch:get(?couch_atokens, AToken) of
    {ok, DocJson} ->
      {DocKVList} = DocJson,
      AUKey = proplists:get_value(<<"key">>, DocKVList),
      {reply, {ok, AUKey}, State};
    {error, Error} ->
      {reply, {error, Error}, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
