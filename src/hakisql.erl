-module(hakisql).

-export([
    create/2,
    insert/2,

    q/2
]).

%% @doc Create a new table.
-spec create(atom(), map()) -> ok.
create(TableName, Schema) ->
    hakisql_table:create(TableName, Schema).

%% @doc Insert rows into a table.
-spec insert(atom(), list(map())) -> ok.
insert(TableName, Rows) ->
    hakisql_table:insert(TableName, Rows).

%% @doc Run a query on a table and return the result.
-spec q(atom(), list()) -> {ok, list()} | {error, Reason :: atom(), []}.
q(TableName, Query) ->
    hakisql_query:rows_for_query(TableName, Query).

