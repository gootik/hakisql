-module(hakisql_query).

-export([
    rows_for_query/2
]).

rows_for_query(TableName, Query) ->
    try
        Schema = hakisql_table:schema_for_table(TableName),
        {ok, AQT} = parse_query(Query),
        Bitmap = query_to_bitmap(Schema, AQT),
        Result = fetch_using_bitmap(TableName, Bitmap),

        {ok, Result}
    catch
        error:Reason ->
            {error, Reason, []}
    end.

parse_query(Query) ->
    {ok, Lex, _} = hakisql_lexer:string(Query),
    hakisql_parser:parse(Lex).

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

    case haki:get(IndexTable, Field) of
        bad_key -> EmptyField;
        FieldMap -> maps:get(Value, FieldMap, EmptyField)
    end;

query_to_bitmap(#{index_table := IndexTable, num_rows := NumRows} = _Schema, {'op', 'has', Field, Value}) ->
    {ok, EmptyField} = bitmap:new([{size, NumRows}]),

    case haki:get(IndexTable, Field) of
        bad_key -> EmptyField;
        FieldMap -> maps:get(Value, FieldMap, EmptyField)
    end;

query_to_bitmap(#{index_table := IndexTable, num_rows := NumRows} = _Schema, {'op', '!=', Field, Value}) ->
    {ok, EmptyField} = bitmap:new([{size, NumRows}]),

    Bitmap = case haki:get(IndexTable, Field) of
                 bad_key -> EmptyField;
                 FieldMap -> maps:get(Value, FieldMap, EmptyField)
             end,

    bitmap:invert(Bitmap);

query_to_bitmap(_, {'op', _, _, _}) ->
    error(not_implemented).

fetch_using_bitmap(Table, Bitmap) ->
    Rows = bitmap:to_list(Bitmap),

    [begin
         RowKey = list_to_atom(integer_to_list(Row)),
         haki:get(Table, RowKey)
     end || Row <- Rows].

