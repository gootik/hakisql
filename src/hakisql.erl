-module(hakisql).

-include("types.hrl").

-export([
    create/2,
    insert/2,

    q/2
]).

%% @doc Create a new table.
-spec create(table_name(), table_column_definition()) -> ok.
create(TableName, ColumnDefinition) ->
    hakisql_table:create(TableName, ColumnDefinition).

%% @doc Insert rows into a table.
-spec insert(table_name(), [map()]) -> ok.
insert(TableName, Rows) ->
    hakisql_table:insert(TableName, Rows).

%% @doc Run a query on a table and return the result.
-spec q(table_name(), string()) -> {ok, [map()]} | {error, Reason :: atom(), []}.
q(TableName, Query) ->
    hakisql_query:rows_for_query(TableName, Query).