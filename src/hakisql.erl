-module(hakisql).

-include("types.hrl").

-include_lib("metronome/include/metronome.hrl").

-export([
    create/2,
    drop/1,
    insert/2,

    q/2
]).

%% @doc Create a new table.
-spec create(table_name(), table_column_definition()) -> ok.
create(TableName, ColumnDefinition) ->
    hakisql_table:create(TableName, ColumnDefinition).

%% @doc Drop a table
-spec drop(table_name()) -> ok.
drop(TableName) ->
    hakisql_table:drop(TableName).

%% @doc Insert rows into a table.
-spec insert(table_name(), [table_row()]) -> ok.
insert(TableName, Rows) ->
    hakisql_table:insert(TableName, Rows).

%% @doc Run a query on a table and return the result.
-spec q(table_name(), select_query()) -> {ok, [table_row()]} | {error, Reason :: atom(), []}.
q(TableName, Query) ->
    try
        Bitmap = hakisql_query:bitmap_for_query(TableName, Query),

        Rows = hakisql_table:fetch_using_bitmap(TableName, Bitmap),

        {ok, Rows}
    catch
        error:Reason ->
            {error, Reason, []}
    end.