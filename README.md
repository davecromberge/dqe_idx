dqe_idx
=====

Dalmatiner query engine indexer abstract module.

This application forwards indexer calls to configured
implementation. By default it will use _dqe_idx_ddb_ that is using plain
DDB for tag expansion and skips meta-data queries. There is also
Posgres indexer backend implemented in _dqe_idx_pg_ module.

Build
-----

    $ rebar3 compile
