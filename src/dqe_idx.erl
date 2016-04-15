-module(dqe_idx).

%% API exports
-export([lookup/1, add/5, delete/5]).

-type where() :: {binary(), binary()} |
                 {'and', where(), where()} |
                 {'or', where(), where()}.

-type qry() :: dql:bm() |
               {binary(), [binary()], where()}.

-callback lookup(qry()) ->
    {ok, {binary(), binary()}}.

-callback add(Bucket::binary(),
              Metric::[binary()],
              LookupMetric::binary(),
              TagKey::binary(),
              TagValue::binary()) ->
    {ok, {MetricIdx::non_neg_integer(), TagIdx::non_neg_integer()}}|
    {error, Error::term()}.

-callback delete(Bucket::binary(),
                 Metric::[binary()],
                 LookupMetric::binary(),
                 TagKey::binary(),
                 TagValue::binary()) ->
    ok |
    {error, Error::term()}.

%%====================================================================
%% API functions
%%====================================================================

-spec lookup(qry()) ->
                    {ok, [{binary(), binary()}]} |
                    {error, Error::term()}.
lookup(Query) ->
    Mod = idx_module(),
    Mod:lookup(Query).


-spec add(Bucket::binary(),
              Metric::[binary()],
              LookupMetric::binary(),
              TagKey::binary(),
              TagValue::binary()) ->
    {ok, {MetricIdx::non_neg_integer(), TagIdx::non_neg_integer()}}|
    {error, Error::term()}.

add(Bucket, Metric, LookupMetric, TagKey, TagValue) ->
    Mod = idx_module(),
    Mod:add(Bucket, Metric, LookupMetric, TagKey, TagValue).

-spec delete(Bucket::binary(),
                 Metric::[binary()],
                 LookupMetric::binary(),
                 TagKey::binary(),
                 TagValue::binary()) ->
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
