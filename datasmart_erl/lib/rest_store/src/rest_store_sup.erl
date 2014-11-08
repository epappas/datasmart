%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_store_sup).

-behaviour(supervisor).

%% API.
-export([start_listeners/0, start_listeners/1, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_listeners() ->
  {ok, Application} = application:get_application(),
  start_listeners(Application).

start_listeners(Application) ->
  Port = application:get_env(Application, http_port, 4421),
  ListenerCount = application:get_env(Application, http_listener_count, 100),

  Dispatch = routing_dispatch(),
  {ok, _} = cowboy:start_http(http, ListenerCount, [{port, Port}], [
    {env, [{dispatch, Dispatch}]},
    {timeout, 12000}
  ]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, Application} = application:get_application(),
  CouchUrl = application:get_env(Application, couch_url, "http://localhost:5984"),
  CouchOpts = application:get_env(Application, couch_opts, []),

  pg2:create(datastore_rest_listeners),
  {ok, {{one_for_one, 10, 10}, [
    {rest_store,
      {rest_store_sup, start_listeners, [Application]},
      permanent, 1000, worker,
      [rest_store_sup]}
  ]}}.

%% ===================================================================
%%% Routing
%% ===================================================================
routing_dispatch() ->
  List = [
    root, index_key,
    user, user_apiKey, user_srp_begin, user_srp_answer, user_srp_verify, user_oukey, user_oukey_apiKey,
    files, files_key
  ],
  cowboy_router:compile([
    %% {URIHost, list({URIPath, Handler, Opts})}
    {'_', [route(Name) || Name <- List]}
  ]).

route(root) ->
  {"/", index_handler, []};

route(index_key) ->
  {"/index/:key", index_handler, []};

route(user) ->
  {"/user", rest_uregister_handler, []};

route(user_apiKey) ->
  {"/user/apiKey", rest_oapikey_handler, []};

route(user_srp_begin) ->
  {"/user/srp/begin", rest_srpbegin_handler, []};

route(user_srp_answer) ->
  {"/user/srp/answer", rest_srpanswer_handler, []};

route(user_srp_verify) ->
  {"/user/srp/verify", rest_srpverify_handler, []};

route(user_oukey) ->
  {"/user/:oukey", rest_ouserkey_handler, []};

route(user_oukey_apiKey) ->
  {"/user/:oukey/apiKey", rest_oapikey_handler, []};

route(files) ->
  {"/user/:oukey/files", files_handler, []};

route(files_key) ->
  {"/user/:oukey/files/:filekey", files_handler, []}.


