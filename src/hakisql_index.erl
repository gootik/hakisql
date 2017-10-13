-module(hakisql_index).

-export([
    calculate_index_map/3
]).

calculate_index_map(#{index_field_names := IndexFieldNames} = _Schema, Rows, NumRows) ->
    InitMap = lists:foldl(
        fun(Field, Acc) ->
            Acc#{Field => #{}}
        end, #{}, IndexFieldNames),

    %% @TODO: Use bitmap:set_many() to set all bits at once instead of looping
    lists:foldl(
        fun(#{'_id' := Id} = Row, IndexMap) ->
            IndexFieldValues = index_values(Row, IndexFieldNames),
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
                end, IndexMap, IndexFieldValues)
        end, InitMap, Rows).

index_values(Row, IndexFieldNames) ->
    [{Field, maps:get(Field, Row)} || Field <- IndexFieldNames].