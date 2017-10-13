-module(hakisql_table).

-export([
    create/2,
    insert/2,

    schema_for_table/1,
    internal_table_name/2
]).

-define(INDEX_TABLE_POSTFIX, "_i").
-define(SCHEMA_TABLE_POSTFIX, "_schema").

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

insert(TableName, Rows) ->
    SchemaTableName = list_to_atom(atom_to_list(TableName) ++ ?SCHEMA_TABLE_POSTFIX),
    Schema = case haki:get(SchemaTableName) of
                 bad_key -> error(no_table);
                 S -> S
             end,

    {EnrichedRows, {RowMap, NumRows}} = lists:mapfoldl(
        fun(RowMap, {AccMap, RowId}) ->
            RowKey = list_to_atom(integer_to_list(RowId)),
            {RowMap#{'_id' => RowId}, {AccMap#{RowKey => RowMap}, RowId + 1}}
        end, {#{}, maps:get(num_rows, Schema)}, Rows),

    haki:cache(SchemaTableName, Schema#{num_rows => NumRows}),
    haki:cache_bucket(TableName, RowMap, #{compiler => haki_beam_compiler}),

    IndexMap = hakisql_index:calculate_index_map(Schema, EnrichedRows, NumRows),

    haki:cache_bucket(maps:get(index_table, Schema), IndexMap).

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