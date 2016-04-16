-module(dqe_idx).

%% API exports
-export([lookup/1, expand/2,
         add/4, add/5, add/6,
         delete/4, delete/5, delete/6]).

-type bucket() :: binary().
-type collection() :: binary().
-type metric() :: binary().
-type key() :: binary().
-type glob_metric() :: [binary() | '*'].
-type tag_name() :: binary().
-type tag_value() :: binary().

-type where() :: {tag_name(), tag_value()} |
                 {'and', where(), where()} |
                 {'or', where(), where()}.
-type lqry() :: {collection(), metric()} |
                {collection(), metric(), where()}.

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
    {ok, {MetricIdx::non_neg_integer(),
          TagIdx::non_neg_integer()}}|
    {error, Error::term()}.

-callback add(Collection::collection(),
              Metric::metric(),
              Bucket::bucket(),
              Key::key(),
              Tags::[{tag_name(), tag_value()}]) ->
    {ok, {MetricIdx::non_neg_integer(),
          TagIdx::non_neg_integer()}}|
    {error, Error::term()}.
-callback add(Collection::collection(),
              Metric::metric(),
              Bucket::bucket(),
              Key::key(),
              TagName::tag_name(),
              TagValue::tag_value()) ->
    {ok, {MetricIdx::non_neg_integer(), TagIdx::non_neg_integer()}}|
    {error, Error::term()}.

-callback delete(Collection::collection(),
                 Metric::metric(),
                 Bucket::bucket(),
                 Key::key()) ->
    {ok, {MetricIdx::non_neg_integer(),
          TagIdx::non_neg_integer()}}|
    {error, Error::term()}.


-callback delete(Collection::collection(),
                 Metric::metric(),
                 Bucket::bucket(),
                 Key::key(),
                 Tags::[{tag_name(), tag_value()}]) ->
    {ok, {MetricIdx::non_neg_integer(),
          TagIdx::non_neg_integer()}}|
    {error, Error::term()}.

-callback delete(Collection::collection(),
                 Metric::metric(),
                 Bucket::bucket(),
                 Key::key(),
                 TagName::tag_name(),
                 TagValue::tag_value()) ->
    ok |
    {error, Error::term()}.

%%====================================================================
%% API functions
%%====================================================================

init() ->
    Mod = idx_module(),
    Mod:init().

-spec lookup(lqry()) ->
                    {ok, [{bucket(), key()}]} |
                    {error, Error::term()}.
lookup(Query) ->
    Mod = idx_module(),
    Mod:lookup(Query).

-spec expand(bucket(), [glob_metric()]) ->
                    {ok, {bucket(), [metric()]}} |
                    {error, Error::term()}.
expand(B, Gs) ->
    Mod = idx_module(),
    Mod:expand(B, Gs).

-spec add(Collection::collection(),
          Metric::metric(),
          Bucket::bucket(),
          Key::key()) ->
                 {ok, {MetricIdx::non_neg_integer(),
                       TagIdx::non_neg_integer()}}|
                 {error, Error::term()}.

add(Collection, Metric, Bucket, Key) ->
    Mod = idx_module(),
    Mod:add(Collection, Metric, Bucket, Key).

-spec add(Collection::collection(),
          Metric::metric(),
          Bucket::bucket(),
          Key::key(),
          Tags::[{tag_name(), tag_value()}]) ->
                 {ok, {MetricIdx::non_neg_integer(),
                       TagIdx::non_neg_integer()}}|
                 {error, Error::term()}.

add(Collection, Metric, Bucket, Key, Tags) ->
    Mod = idx_module(),
    Mod:add(Collection, Metric, Bucket, Key, Tags).

-spec add(Collection::collection(),
          Metric::metric(),
          Bucket::bucket(),
          Key::key(),
          TagName::tag_name(),
          TagValue::tag_value()) ->
                 {ok, {MetricIdx::non_neg_integer(),
                       TagIdx::non_neg_integer()}}|
                 {error, Error::term()}.

add(Collection, Metric, Bucket, Key, TagName, TagValue) ->
    Mod = idx_module(),
    Mod:add(Collection, Metric, Bucket, Key, TagName, TagValue).

-spec delete(Collection::collection(),
             Metric::metric(),
             Bucket::bucket(),
             Key::key()) ->
                    {ok, {MetricIdx::non_neg_integer(),
                          TagIdx::non_neg_integer()}}|
                    {error, Error::term()}.

delete(Collection, Metric, Bucket, Key) ->
    Mod = idx_module(),
    Mod:delete(Collection, Metric, Bucket, Key).

-spec delete(Collection::collection(),
             Metric::metric(),
             Bucket::bucket(),
             Key::key(),
             Tags::[{tag_name(), tag_value()}]) ->
                    {ok, {MetricIdx::non_neg_integer(),
                          TagIdx::non_neg_integer()}}|
                    {error, Error::term()}.

delete(Collection, Metric, Bucket, Key, Tags) ->
    Mod = idx_module(),
    Mod:delete(Collection, Metric, Bucket, Key, Tags).

-spec delete(Collection::collection(),
             Metric::metric(),
             Bucket::bucket(),
             Key::key(),
             TagName::tag_name(),
             TagValue::tag_value()) ->
                    {ok, {MetricIdx::non_neg_integer(),
                          TagIdx::non_neg_integer()}}|
                    {error, Error::term()}.

delete(Collection, Metric, Bucket, Key, TagName, TagValue) ->
    Mod = idx_module(),
    Mod:delete(Collection, Metric, Bucket, Key, TagName, TagValue).

%%====================================================================
%% Internal functions
%%====================================================================
idx_module() ->
    application:get_env(dqe_idx, lookup_module, dqe_idx_ddb).
