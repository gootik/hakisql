-module(hakisql).

-export([
    create/2,
    insert/2,

    q/2
]).

create(TableName, Schema) ->
    TableNameList = atom_to_list(TableName),
    SchemaTableName = list_to_atom(TableNameList ++ "_schema"),
    IndexTable = list_to_atom(TableNameList ++ "_i"),

    InternalSchema = #{cols => Schema,
                       num_rows => 0,
                       table_name => TableName,
                       index_table => IndexTable},

    haki:cache(SchemaTableName, InternalSchema).

insert(TableName, Rows) ->
    SchemaTableName = list_to_atom(atom_to_list(TableName) ++ "_schema"),
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

    IndexMap = calculate_index_map(Schema, EnrichedRows, NumRows),

    haki:cache_bucket(maps:get(index_table, Schema), IndexMap).

q(TableName, Query) ->
    try
        SchemaTableName = list_to_atom(atom_to_list(TableName) ++ "_schema"),
        Schema = case haki:get(SchemaTableName) of
                     bad_key -> error(no_table);
                     S -> S
                 end,

        {ok, AQT} = parse_query(Query),
        Bitmap = query_to_bitmap(Schema, AQT),
        Result = get_using_bitmap(TableName, Bitmap),

        {ok, Result}
    catch
        error:Reason ->
            {Reason, []}
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

query_to_bitmap(_, {'op', _, _, _}) ->
    error(not_implemented).

get_using_bitmap(Table, Bitmap) ->
    Rows = bitmap:to_list(Bitmap),

    [begin
         RowKey = list_to_atom(integer_to_list(Row)),
         haki:get(Table, RowKey)
     end || Row <- Rows].

calculate_index_map(Schema, Rows, NumRows) ->
    IndexFields = lists:filtermap(
        fun({Field, SchemaDef}) ->
            case lists:member(index, SchemaDef) of
                true -> {true, Field};
                false -> false
            end
        end, maps:to_list(maps:get(cols, Schema))),

    InitMap = lists:foldl(
        fun(Field, Acc) ->
            Acc#{Field => #{}}
        end, #{}, IndexFields),

    %% @TODO: Use bitmap:set_many() to set all bits at once instead of looping
    lists:foldl(
        fun(#{'_id' := Id} = Row, IndexMap) ->
            Values = [{Field, maps:get(Field, Row)} || Field <- IndexFields],
            lists:foldl(
                fun({Field, Value}, Acc) ->
                    FM = maps:get(Field, Acc),
                    NFM = case maps:is_key(Value, FM) of
                              true ->
                                  {ok, NewIndex} = bitmap:set(Id, maps:get(Value, FM)),
                                  FM#{Value => NewIndex};
                              false ->
                                  {ok, NewIndex0} = bitmap:new([{size, NumRows}]),
                                  {ok, NewIndex} = bitmap:set(Id, NewIndex0),
                                  FM#{Value => NewIndex}
                          end,

                    Acc#{Field => NFM}
                end, IndexMap, Values)
        end, InitMap, Rows).