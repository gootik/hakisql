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
    #{num_rows := NumRows} = Schema,

    {ok, EmptyBitmap} = bitmap:new([{size, NumRows}]),
    State = #{empty_bitmap => EmptyBitmap},

    {ok, ParserTree} = parse_query(Query),
    query_to_bitmap(Schema, ParserTree, State).

parse_query(Query) ->
    {ok, Lex, _} = hakisql_lexer2:string(Query),
    hakisql_parser2:parse(Lex).

query_to_bitmap(Schema, {'or', Lterm, Rterm}, State) ->
    bitmap:union(
        query_to_bitmap(Schema, Lterm, State),
        query_to_bitmap(Schema, Rterm, State)
    );

query_to_bitmap(Schema, {'and', Lterm, Rterm}, State) ->
    bitmap:intersection(
        query_to_bitmap(Schema, Lterm, State),
        query_to_bitmap(Schema, Rterm, State)
    );

query_to_bitmap(#{index_table := IndexTable} = _Schema, {'op', '=', Field, Value}, #{empty_bitmap := EmptyBitmap} = _State) ->
    field_value_bitmap(IndexTable, Field, Value, EmptyBitmap);

query_to_bitmap(#{index_table := IndexTable} = _Schema, {'op', '!=', Field, Value}, #{empty_bitmap := EmptyBitmap} = _State) ->
    Bitmap = field_value_bitmap(IndexTable, Field, Value, EmptyBitmap),
    bitmap:invert(Bitmap);

query_to_bitmap(#{index_table := IndexTable} = _Schema, {'op', 'has', Field, Value}, #{empty_bitmap := EmptyBitmap} = _State) ->
    field_value_bitmap(IndexTable, Field, Value, EmptyBitmap);

query_to_bitmap(#{index_table := IndexTable} = _Schema, {'op', 'in', Field, {list, Values}}, #{empty_bitmap := EmptyBitmap} = _State) ->
    %% Union of all field bitmaps
    lists:foldl(
        fun(Value, Acc) ->
            bitmap:union(
                Acc,
                field_value_bitmap(IndexTable, Field, Value, EmptyBitmap)
            )
        end, EmptyBitmap, Values);

query_to_bitmap(#{index_table := IndexTable} = _Schema, {'op', 'notin', Field, {list, Values}}, #{empty_bitmap := EmptyBitmap} = _State) ->
    %% Union of all field bitmaps
    Bitmap = lists:foldl(
        fun(Value, Acc) ->
            bitmap:union(
                Acc,
                field_value_bitmap(IndexTable, Field, Value, EmptyBitmap)
            )
        end, EmptyBitmap, Values),

    bitmap:invert(Bitmap);

query_to_bitmap(_, _, _) ->
    error(not_implemented).

-ifdef('HAS_PERSISTENT_TERM').
field_value_bitmap(IndexTable, Field, Value, Default) ->
    case hakisql_storage:map_get(IndexTable, Field) of
        bad_key -> Default;
        FieldMap -> maps:get(Value, FieldMap, Default)
    end.
-else.
field_value_bitmap(IndexTable, Field, Value, Default) ->
    case hakisql_storage:map_get(haki_compiler:mod_name(IndexTable), Field) of
        bad_key -> Default;
        FieldMap -> maps:get(Value, FieldMap, Default)
    end.
-endif.