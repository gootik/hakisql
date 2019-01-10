-module(hakisql_query).

-compile(inline).
-compile({inline_size, 520}).
-compile({inline_effort, 500}).
-compile(inline_list_funcs).

-include("types.hrl").

-export([
    bitmap_for_query/2
]).

%% @doc Given a query string this function will return a bitmap with right
%%      row bits set.
%% @end
-spec bitmap_for_query(table_name(), select_query()) -> bitmap:bitmap().
bitmap_for_query(TableName, Query) ->
    Schema = hakisql_table:schema_for_table(TableName),
    {ok, ParserTree} = parse_query(Query),
    query_to_bitmap(Schema, ParserTree).

parse_query(Query) ->
    {ok, Lex, _} = hakisql_lexer2:string(Query),
    hakisql_parser2:parse(Lex).

query_to_bitmap(Schema, {'or', Lterm, Rterm}) ->
    bitmap:union(
        query_to_bitmap(Schema, Lterm),
        query_to_bitmap(Schema, Rterm)
    );

query_to_bitmap(Schema, {'and', Lterm, Rterm}) ->
    bitmap:intersection(
        query_to_bitmap(Schema, Lterm),
        query_to_bitmap(Schema, Rterm)
    );

query_to_bitmap(#{index_table := IndexTable, num_rows := NumRows} = _Schema, {'op', '=', Field, Value}) ->
    {ok, EmptyField} = bitmap:new([{size, NumRows}]),

    field_value_bitmap(IndexTable, Field, Value, EmptyField);

query_to_bitmap(#{index_table := IndexTable, num_rows := NumRows} = _Schema, {'op', '!=', Field, Value}) ->
    {ok, EmptyField} = bitmap:new([{size, NumRows}]),

    Bitmap = field_value_bitmap(IndexTable, Field, Value, EmptyField),
    bitmap:invert(Bitmap);

query_to_bitmap(#{index_table := IndexTable, num_rows := NumRows} = _Schema, {'op', 'has', Field, Value}) ->
    {ok, EmptyField} = bitmap:new([{size, NumRows}]),
    field_value_bitmap(IndexTable, Field, Value, EmptyField);

query_to_bitmap(#{index_table := IndexTable, num_rows := NumRows} = _Schema, {'op', 'in', Field, {list, Values}}) ->
    {ok, EmptyField} = bitmap:new([{size, NumRows}]),

    %% Union of all field bitmaps
    lists:foldl(
        fun(Value, Acc) ->
            bitmap:union(
                Acc,
                field_value_bitmap(IndexTable, Field, Value, EmptyField)
            )
        end, EmptyField, Values);

query_to_bitmap(#{index_table := IndexTable, num_rows := NumRows} = _Schema, {'op', 'notin', Field, {list, Values}}) ->
    {ok, EmptyField} = bitmap:new([{size, NumRows}]),

    %% Union of all field bitmaps
    Bitmap = lists:foldl(
        fun(Value, Acc) ->
            bitmap:union(
                Acc,
                field_value_bitmap(IndexTable, Field, Value, EmptyField)
            )
        end, EmptyField, Values),

    bitmap:invert(Bitmap);

query_to_bitmap(_, _) ->
    error(not_implemented).

field_value_bitmap(IndexTable, Field, Value, Default) ->
    case haki:get(IndexTable, Field) of
        bad_key -> Default;
        FieldMap -> maps:get(Value, FieldMap, Default)
    end.