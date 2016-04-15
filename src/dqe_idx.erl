-module(dqe_idx).

%% API exports
-export([lookup/1, add/5, delete/5]).

-type bucket() :: binary().
-type metric() :: binary().
-type token_metric() :: [binary()].
-type glob_metric() :: [binary() | '*'].
-type tag_key() :: binary().
-type tag_value() :: binary().

-type where() :: {tag_key(), tag_value()} |
                 {'and', where(), where()} |
                 {'or', where(), where()}.
-type lqry() :: {bucket(), token_metric()} |
                {bucket(), token_metric(), where()}.
-type eqry() :: [{bucket(), [token_metric()]}].

-callback lookup(lqry()) ->
    {ok, [{bucket(), metric()}]} |
    {error, Error::term()}.

-callback expand(eqry()) ->
    {ok, [{bucket(), [metric()]}]} |
    {error, Error::term()}.

-callback add(Bucket::bucket(),
              Metric::token_metric(),
              LookupMetric::binary(),
              TagKey::tag_key(),
              TagValue::tag_value()) ->
    {ok, {MetricIdx::non_neg_integer(), TagIdx::non_neg_integer()}}|
    {error, Error::term()}.

-callback delete(Bucket::bucket(),
                 Metric::token_metric(),
                 LookupMetric::binary(),
                 TagKey::tag_key(),
                 TagValue::tag_value()) ->
    ok |
    {error, Error::term()}.

%%====================================================================
%% API functions
%%====================================================================

-spec lookup(lqry()) ->
                    {ok, [{binary(), binary()}]} |
                    {error, Error::term()}.
lookup(Query) ->
    Mod = idx_module(),
    Mod:lookup(Query).

-spec expand(eqry()) ->
                    {ok, [{bucket(), [metric()]}]} |
                    {error, Error::term()}.
expand(Query) ->
    Mod = idx_module(),
    Mod:expand(Query).

-spec add(Bucket::binary(),
          Metric::[binary()],
          LookupMetric::binary(),
          TagKey::tag_key(),
          TagValue::tag_value()) ->
    {ok, {MetricIdx::non_neg_integer(), TagIdx::non_neg_integer()}}|
    {error, Error::term()}.

add(Bucket, Metric, LookupMetric, TagKey, TagValue) ->
    Mod = idx_module(),
    Mod:add(Bucket, Metric, LookupMetric, TagKey, TagValue).

-spec delete(Bucket::binary(),
             Metric::[binary()],
             LookupMetric::binary(),
             TagKey::tag_key(),
             TagValue::tag_value()) ->
    ok |
    {error, Error::term()}.

delete(Bucket, Metric, LookupMetric, TagKey, TagValue) ->
    Mod = idx_module(),
    Mod:delete(Bucket, Metric, LookupMetric, TagKey, TagValue).

%%====================================================================
%% Internal functions
%%====================================================================
idx_module() ->
    application:get_env(dqe, lookup_module, dqe_idx_ddb).
