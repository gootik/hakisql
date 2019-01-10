-module(hakisql_table).

-compile(inline).
-compile({inline_size, 520}).
-compile({inline_effort, 500}).
-compile(inline_list_funcs).

-include_lib("metronome/include/metronome.hrl").

-include("internal.hrl").

-export([
    create/2,
    insert/2,
    fetch_using_bitmap/2,

    schema_for_table/1
]).

%% @doc Given a column definition map this will create a new table
%%      ready to be populated.
%% @end
-spec create(table_name(), table_column_definition()) -> ok.
create(TableName, ColumnDefinition) ->
    SchemaTableName = internal_table_name(schema, TableName),
    IndexTableName = internal_table_name(index, TableName),
    IndexFieldNames = schema_index_fields(ColumnDefinition),

    InternalSchema = #{cols => ColumnDefinition,
                       num_rows => 0,
                       table_name => TableName,
                       index_table => IndexTableName,
                       index_field_names => IndexFieldNames},

    haki:cache(SchemaTableName, InternalSchema).

%% @doc Will insert the rows in the given table and update index mappings.
%% @end
-spec insert(table_name(), [table_row()]) -> ok.
insert(TableName, Rows) ->
    Schema = schema_for_table(TableName),
    InitialNumRows = maps:get(num_rows, Schema),

    {EnrichedRows, {RowMap, NumRows}} = lists:mapfoldl(
        fun(RowMap, {AccMap, RowId}) ->
            {RowMap#{'_id' => RowId}, {AccMap#{RowId => RowMap}, RowId + 1}}
        end, {#{}, InitialNumRows}, Rows),

    haki:cache(internal_table_name(schema, TableName), Schema#{num_rows => NumRows}),
    haki:cache_bucket(TableName, RowMap),

    %% TODO(gootik): Need to UPDATE indexes not rebuild. Using only newly added rows.
    IndexMap = hakisql_index:calculate_index_map(Schema, EnrichedRows, NumRows),

    haki:cache_bucket(maps:get(index_table, Schema), IndexMap).

%% @doc Will return the internal schema for the given table.
%% @end
-spec schema_for_table(table_name()) -> map() | error.
schema_for_table(TableName) ->
    case haki:get(internal_table_name(schema, TableName)) of
        bad_key -> error(no_table);
        Schema -> Schema
    end.

%% @doc Given a bitmap, we will fetch all the rows defined in the bitmap
%%      and return the result. The order of result is not guaranteed.
%% @end
-spec fetch_using_bitmap(table_name(), bitmap:bitmap()) -> [table_row()].
fetch_using_bitmap(TableName, Bitmap) ->
    TableMod = haki_compiler:mod_name(TableName),
    traverse_bitmap(TableMod, Bitmap).

internal_table_name(schema, TableName) -> list_to_atom(atom_to_list(TableName) ++ ?SCHEMA_TABLE_POSTFIX);
internal_table_name(index, TableName)  -> list_to_atom(atom_to_list(TableName) ++ ?INDEX_TABLE_POSTFIX).

schema_index_fields(Columns) ->
    lists:filtermap(
        fun({Field, ColDef}) ->
            case lists:member(index, ColDef) of
                true -> {true, Field};
                false -> false
            end
        end, maps:to_list(Columns)).

traverse_bitmap(TableMod, <<Size:64/unsigned, Bitmap:Size/bitstring, _/bitstring>>) ->
    traverse_bitmap(TableMod, Bitmap, 0, []).

traverse_bitmap(_, <<>>, _, Acc) ->
    Acc;
traverse_bitmap(TableMod, <<0:1, Rest/bitstring>>, RowId, Acc) ->
    traverse_bitmap(TableMod, Rest, RowId + 1, Acc);
traverse_bitmap(TableMod, <<1:1, Rest/bitstring>>, RowId, Acc) ->
    R = TableMod:get(RowId),
    traverse_bitmap(TableMod, Rest, RowId + 1, [R | Acc]).