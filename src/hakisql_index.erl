-module(hakisql_index).

-export([
    calculate_index_map/3
]).

%% @doc To demonstrate the index calculation we can use this example:
%%      Schema ->
%%          a => [index]
%%          b => [index]
%%          c => []
%%
%%      Which means we need to have separate bitmap maps for every value
%%      of 'a' and 'b'.
%%
%%      Here we first create a map of every indexed field, that contains
%%      all possible value bitmaps. Hence we get:
%%
%%      InitMap = #{
%%          a => #{},
%%          b => #{}
%%      }
%%
%%      Next, we loop through every row and adding/updating the bitmaps for
%%      every entry of the indexed fields. Given below example as inserted
%%      rows:
%%
%%      [#{a => test, b = 2, c => not_important},
%%       #{a => test2, b = 2, c => not_important},
%%       #{a => test, b = 3, c => not_important}]
%%
%%      We _should_ have a final map that looks like:
%%
%%      FinalMap = #{
%%          a => #{
%%              test  => 101,
%%              test2 => 010
%%          },
%%          b => #{
%%              2 => 110,
%%              3 => 001
%%          }
%%      }
%%
%%      We then cache this map for every field and use the bitmaps at query
%%      time to figure out which rows need to be fetched.
%% @end
calculate_index_map(#{index_field_names := IndexFieldNames} = _Schema, Rows, NumRows) ->

    InitMap = lists:foldl(
        fun(Field, Acc) ->
            Acc#{Field => #{}}
        end, #{}, IndexFieldNames),

    %% TODO: Use bitmap:set_many() to set all bits at once instead of looping
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