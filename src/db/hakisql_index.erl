-module(hakisql_index).

-compile(inline).
-compile({inline_size, 520}).
-compile({inline_effort, 500}).
-compile(inline_list_funcs).

-include("internal.hrl").

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
%%      Here we first create a map of every indexed field, that will contain
%%      all possible values' bitmaps. Hence we get:
%%
%%      InitMap = #{
%%          a => #{},
%%          b => #{}
%%      }
%%
%%      Next, we loop through every row and add/update the bitmaps for
%%      every entry of the indexed fields. Given below example as inserted
%%      rows:
%%
%%      [#{a => [test, test3], b = 2, c => not_important, _id => 0},
%%       #{a => test2, b = 2, c => not_important, _id => 1},
%%       #{a => test, b = 3, c => not_important, _id => 2}]
%%
%%      We _should_ have a final map that looks like:
%%
%%      FinalMap = #{
%%          a => #{
%%              test  => 101,
%%              test2 => 010,
%%              test3 => 100
%%          },
%%          b => #{
%%              2 => 110,
%%              3 => 001
%%          }
%%      }
%%
%%      We then cache this map for every field and use the bitmaps at query
%%      time to figure out which rows need to be fetched.
%%
%%      For fields with multiple values, we add the row into all bitmaps. This
%%      will let us to be able to run queries like `a has test3`. Which actually
%%      would translate into `a = test3` for the bitmap translation.
%% @end
-spec calculate_index_map(internal_schema(), [table_row()], non_neg_integer()) -> index_map().
calculate_index_map(#{index_field_names := IndexFieldNames} = _Schema, Rows, NumRows) ->

    InitMap = lists:foldl(
        fun(Field, Acc) ->
            Acc#{Field => #{}}
        end, #{}, IndexFieldNames),

    %% TODO: Use bitmap:from_list() to set all bits at once instead of looping
    lists:foldl(
        fun(#{'_id' := RowId} = Row, IndexMap) ->

            %% All the values that need to be added to bitmaps from this row
            IndexFieldValues = index_values(Row, IndexFieldNames),

            %% For every {Field, Value} pair find it in the final map and update
            %% the bitmap.
            lists:foldl(
                fun({Field, Value}, Acc) ->
                    FieldMap = maps:get(Field, Acc),
                    Values = case is_list(Value) of
                                 %% TODO: LOL this is dumb.
                                 true -> case io_lib:printable_list(Value) of
                                             true -> [Value];
                                             false -> Value
                                         end;
                                 false ->
                                     [Value]
                             end,

                    NewFieldMap = calculate_field_map(FieldMap, Values, NumRows, RowId),

                    %% Update the final map with the newly updated bitmap for the field.
                    Acc#{Field => NewFieldMap}
                end, IndexMap, IndexFieldValues)
        end, InitMap, Rows).

index_values(Row, IndexFieldNames) ->
    [{Field, maps:get(Field, Row)} || Field <- IndexFieldNames].


calculate_field_map(FieldMap, [], _, _) ->
    FieldMap;
calculate_field_map(FieldMap, [Value | Rest], NumRows, RowId) ->
%% If the value has not been seen yet, then create a new bitmap with
%% the specific bit set. If it exists, get it and update it.
    NFM = case maps:is_key(Value, FieldMap) of
              true ->
                  {ok, NewIndex} = bitmap:set(RowId, maps:get(Value, FieldMap)),
                  FieldMap#{Value => NewIndex};
              false ->
                  {ok, NewIndex0} = bitmap:new([{size, NumRows}]),
                  {ok, NewIndex} = bitmap:set(RowId, NewIndex0),
                  FieldMap#{Value => NewIndex}
          end,
    calculate_field_map(NFM, Rest, NumRows, RowId).
