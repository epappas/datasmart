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

  CouchUrl =
    case application:get_env(Application, couch_port) of
      undefined -> application:get_env(Application, couch_url, "http://localhost:5984");
      COUCH_PORT ->
        re:replace(COUCH_PORT, "tcp", "http", [global, {return, list}])
    end,
  CouchOpts = application:get_env(Application, couch_opts, []),

  pg2:create(datastore_rest_listeners),
  {ok, {{one_for_one, 10, 10}, [
    {couch,
      {couch, start_link, [CouchUrl, CouchOpts]},
      permanent, 1000, worker,
      [couch]},
    {ecache,
      {ecache, start_link, []},
      permanent, 1000, worker,
      [ecache]},
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
    user, user_srp_begin, user_srp_answer, user_srp_verify,
    user_oukey, user_oukey_srp_begin, user_oukey_srp_answer, user_oukey_srp_verify,
    user_aukey, user_aukey_srp_begin, user_aukey_srp_answer, user_aukey_srp_verify,
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

%% =========================================
%% user
%% =========================================
route(user) ->
  {"/user", rest_uregister_handler, []};

route(user_srp_begin) ->
  {"/user/_srp/begin", rest_srpbegin_handler, [{module, user}]};

route(user_srp_answer) ->
  {"/user/_srp/answer", rest_srpanswer_handler, [{module, user}]};

route(user_srp_verify) ->
  {"/user/_srp/verify", rest_srpverify_handler, [{module, user}]};

%% =========================================
%% oukey
%% =========================================
route(user_oukey) ->
  {"/user/_oukey/:oukey", rest_oukey_handler, []};

route(user_oukey_srp_begin) ->
  {"/user/_oukey/_srp/begin", rest_srpbegin_handler, [{module, oukey}]};

route(user_oukey_srp_answer) ->
  {"/user/_oukey/_srp/answer", rest_srpanswer_handler, [{module, oukey}]};

route(user_oukey_srp_verify) ->
  {"/user/_oukey/_srp/verify", rest_srpverify_handler, [{module, oukey}]};

%% =========================================
%% aukey
%% =========================================
route(user_aukey) ->
  {"/user/_aukey", rest_oukey_handler, []};

route(user_aukey_srp_begin) ->
  {"/user/_aukey/_srp/begin", rest_srpbegin_handler, [{module, aukey}]};

route(user_aukey_srp_answer) ->
  {"/user/_aukey/_srp/answer", rest_srpanswer_handler, [{module, aukey}]};

route(user_aukey_srp_verify) ->
  {"/user/_aukey/_srp/verify", rest_srpverify_handler, [{module, aukey}]};

%% =========================================
%% files
%% =========================================
route(files) ->
  {"/user/:type/:key/files", files_handler, []};

route(files_key) ->
  {"/user/:type/:key/files/:filekey", files_handler, []}.