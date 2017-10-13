-module(hakisql_table).

-include("internal.hrl").

-export([
    create/2,
    insert/2,

    schema_for_table/1
]).

%% @doc Given a column definition map this will create a new table
%%      ready to be populated.
%% @end
-spec create(atom(), map()) -> ok.
create(TableName, Schema) ->
    SchemaTableName = internal_table_name(schema, TableName),
    IndexTableName = internal_table_name(index, TableName),
    IndexFieldNames = schema_index_fields(Schema),

    InternalSchema = #{cols => Schema,
                       num_rows => 0,
                       table_name => TableName,
                       index_table => IndexTableName,
                       index_field_names => IndexFieldNames},

    haki:cache(SchemaTableName, InternalSchema).

%% @doc Will insert the rows in the given table and update index mappings.
%% @end
-spec insert(atom(), [map()]) -> ok.
insert(TableName, Rows) ->
    Schema = schema_for_table(TableName),

    {EnrichedRows, {RowMap, NumRows}} = lists:mapfoldl(
        fun(RowMap, {AccMap, RowId}) ->
            RowKey = list_to_atom(integer_to_list(RowId)),
            {RowMap#{'_id' => RowId}, {AccMap#{RowKey => RowMap}, RowId + 1}}
        end, {#{}, maps:get(num_rows, Schema)}, Rows),

    haki:cache(internal_table_name(schema, TableName), Schema#{num_rows => NumRows}),
    haki:cache_bucket(TableName, RowMap, #{compiler => haki_beam_compiler}),

    %% TODO: Need to UPDATE indexes not rebuild using only newly added rows.
    IndexMap = hakisql_index:calculate_index_map(Schema, EnrichedRows, NumRows),

    haki:cache_bucket(maps:get(index_table, Schema), IndexMap).

%% @doc Will return the internal schema for the given table.
%% @end
-spec schema_for_table(atom()) -> map() | error.
schema_for_table(TableName) ->
    case haki:get(internal_table_name(schema, TableName)) of
        bad_key -> error(no_table);
        Schema -> Schema
    end.


internal_table_name(schema, TableName) -> list_to_atom(atom_to_list(TableName) ++ ?SCHEMA_TABLE_POSTFIX);
internal_table_name(index, TableName) -> list_to_atom(atom_to_list(TableName) ++ ?INDEX_TABLE_POSTFIX).

schema_index_fields(Columns) ->
    lists:filtermap(
        fun({Field, SchemaDef}) ->
            case lists:member(index, SchemaDef) of
                true -> {true, Field};
                false -> false
            end
        end, maps:to_list(Columns)).