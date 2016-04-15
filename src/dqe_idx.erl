-module(dqe_idx).

%% API exports
-export([lookup/1]).

-type where() :: {binary(), binary()} |
                 {'and', where(), where()} |
                 {'or', where(), where()}.
-type query() :: dql:bm() |
                 {binary(), [binary()], where()}.

-callback lookup(query()) -> {ok, {binary(), binary()}}.

%%====================================================================
%% API functions
%%====================================================================

-spec lookup(query()) -> {ok, [{binary(), binary()}]}.
lookup(Query) ->
    Mod = idx_module(),
    Mod:lookup(Query).

%%====================================================================
%% Internal functions
%%====================================================================
idx_module() ->
    application:get_env(dqe, lookup_module, dqe_idx_ddb).
