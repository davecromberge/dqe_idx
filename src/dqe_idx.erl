%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Project-FiFo UG
%%% @doc
%%% Call back module and wrapper for the Dalmatiner Query Engine
%%% Indexer. This module should be used in place of calling
%%% different indexer backends.
%%% @end
%%% Created : 16 Apr 2016 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(dqe_idx).

%% API exports
-export([lookup/1, expand/2, init/0,
         add/4, add/5, add/7,
         delete/4, delete/5, delete/7]).

-type bucket() :: binary().
-type collection() :: binary().
-type metric() :: binary().
-type key() :: binary().
-type glob_metric() :: [binary() | '*'].
-type namespace() :: binary().
-type tag_name() :: binary().
-type tag() :: {tag, Namespace::namespace(), TagName::tag_name()}.
-type tag_value() :: binary().



-type where() :: {'=',  tag(), tag_value()} |
                 {'and', where(), where()} |
                 {'or', where(), where()}.

-type lqry() :: {in, collection(), [metric()]} |
                {in, collection(), [metric()], where()}.

-export_type([bucket/0, collection/0, metric/0, key/0,
              glob_metric/0, tag_name/0, tag_value/0,
              where/0, lqry/0]).

-callback init() ->
    ok |
    {error, Error::term()}.

-callback lookup(lqry()) ->
    {ok, [{bucket(), key()}]} |
    {error, Error::term()}.

-callback expand(bucket(), [glob_metric()]) ->
    {ok, {bucket(), [metric()]}} |
    {error, Error::term()}.

-callback add(Collection::collection(),
              Metric::metric(),
              Bucket::bucket(),
              Key::key()) ->
    {ok, MetricIdx::term()}|
    {error, Error::term()}.

-callback add(Collection::collection(),
              Metric::metric(),
              Bucket::bucket(),
              Key::key(),
              Tags::[{namespace(), tag_name(), tag_value()}]) ->
    {ok, MetricIdx::term()}|
    {error, Error::term()}.

-callback add(Collection::collection(),
              Metric::metric(),
              Bucket::bucket(),
              Key::key(),
              Namespace::namespace(),
              TagName::tag_name(),
              TagValue::tag_value()) ->
    {ok, MetricIdx::term()}|
    {error, Error::term()}.

-callback delete(Collection::collection(),
                 Metric::metric(),
                 Bucket::bucket(),
                 Key::key()) ->
    ok | {error, Error::term()}.

-callback delete(Collection::collection(),
                 Metric::metric(),
                 Bucket::bucket(),
                 Key::key(),
                 Tags::[{namespace(), tag_name(), tag_value()}]) ->
    ok | {error, Error::term()}.

-callback delete(Collection::collection(),
                 Metric::metric(),
                 Bucket::bucket(),
                 Key::key(),
                 Namespace::namespace(),
                 TagName::tag_name(),
                 TagValue::tag_value()) ->
    ok | {error, Error::term()}.

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes the Dalmatiner Query Engine indexer, this will hand
%% down to whatever indexing backend is used.
%% @end
%%--------------------------------------------------------------------

-spec init() -> ok | {error, Error::term()}.
init() ->
    Mod = idx_module(),
    Mod:init().

%%--------------------------------------------------------------------
%% @doc
%% Takes a lookup query and reutrns a list of all metric/bucket
%% paris that metch the lookup criteria.
%% @end
%%--------------------------------------------------------------------

-spec lookup(lqry()) ->
                    {ok, [{bucket(), key()}]} |
                    {error, Error::term()}.
lookup(Query) ->
    Mod = idx_module(),
    Mod:lookup(Query).

%%--------------------------------------------------------------------
%% @doc
%% Expands a glob into all matching metrics for a given bucket.
%% WARNING: This might go away!
%% @end
%%--------------------------------------------------------------------

-spec expand(bucket(), [glob_metric()]) ->
                    {ok, {bucket(), [metric()]}} |
                    {error, Error::term()}.
expand(B, Gs) ->
    Mod = idx_module(),
    Mod:expand(B, Gs).

%%--------------------------------------------------------------------
%% @doc
%% Links a collection/metric to a bucket and key. Returns whatever
%% identifyer the colleciton/metric has if any.
%% @end
%%--------------------------------------------------------------------

-spec add(Collection::collection(),
          Metric::metric(),
          Bucket::bucket(),
          Key::key()) ->
                 {ok, MetricIdx::term()} |
                 {error, Error::term()}.

add(Collection, Metric, Bucket, Key) ->
    Mod = idx_module(),
    Mod:add(Collection, Metric, Bucket, Key).

%%--------------------------------------------------------------------
%% @doc
%% Adds one or more metrics tag pairs to a metric. This function
%% might call add/6 repeatively or use a more optimized method.
%% @end
%%--------------------------------------------------------------------

-spec add(Collection::collection(),
          Metric::metric(),
          Bucket::bucket(),
          Key::key(),
          Tags::[{namespace(), tag_name(), tag_value()}]) ->
                 {ok, MetricIdx::term()} |
                 {error, Error::term()}.

add(Collection, Metric, Bucket, Key, Tags) ->
    Mod = idx_module(),
    Mod:add(Collection, Metric, Bucket, Key, Tags).

%%--------------------------------------------------------------------
%% @doc
%% Adds a tag pair to a metrics.
%% @end
%%--------------------------------------------------------------------

-spec add(Collection::collection(),
          Metric::metric(),
          Bucket::bucket(),
          Key::key(),
          Namespace::namespace(),
          TagName::tag_name(),
          TagValue::tag_value()) ->
                 {ok, MetricIdx::term()} |
                 {error, Error::term()}.

add(Collection, Metric, Bucket, Key, Namespace, TagName, TagValue) ->
    Mod = idx_module(),
    Mod:add(Collection, Metric, Bucket, Key, Namespace, TagName, TagValue).

%%--------------------------------------------------------------------
%% @doc
%% Deletes a Collection/Metric pair and all it's tags.
%% @end
%%--------------------------------------------------------------------

-spec delete(Collection::collection(),
             Metric::metric(),
             Bucket::bucket(),
             Key::key()) ->
                    ok |
                    {error, Error::term()}.

delete(Collection, Metric, Bucket, Key) ->
    Mod = idx_module(),
    Mod:delete(Collection, Metric, Bucket, Key).

%%--------------------------------------------------------------------
%% @doc
%% Deletes one or more Tag pairs from a Metric. This funciton can
%% call delete/6 multiple times or use a more optimized method.
%% @end
%%--------------------------------------------------------------------
-spec delete(Collection::collection(),
             Metric::metric(),
             Bucket::bucket(),
             Key::key(),
             Tags::[{namespace(), tag_name(), tag_value()}]) ->
                    ok |
                    {error, Error::term()}.

delete(Collection, Metric, Bucket, Key, Tags) ->
    Mod = idx_module(),
    Mod:delete(Collection, Metric, Bucket, Key, Tags).

%%--------------------------------------------------------------------
%% @doc
%% Deletes a Tag pairs from a Metric.
%% @end
%%--------------------------------------------------------------------

-spec delete(Collection::collection(),
             Metric::metric(),
             Bucket::bucket(),
             Key::key(),
             Namespace::namespace(),
             TagName::tag_name(),
             TagValue::tag_value()) ->
                    ok |
                    {error, Error::term()}.

delete(Collection, Metric, Bucket, Key, Namespace, TagName, TagValue) ->
    Mod = idx_module(),
    Mod:delete(Collection, Metric, Bucket, Key, Namespace, TagName, TagValue).

%%====================================================================
%% Internal functions
%%====================================================================
idx_module() ->
    application:get_env(dqe_idx, lookup_module, dqe_idx_ddb).
