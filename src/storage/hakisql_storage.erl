-module(hakisql_storage).

-export([
    insert/2,
    insert_map/2,

    get/1,
    map_get/2,

    drop/1
]).
-ifdef('HAS_PERSISTENT_TERM').
insert(Key, Value) ->
    persistent_term:put(Key, Value).

insert_map(Table, Values) ->
    List = maps:to_list(Values),

    lists:foreach(
        fun({K, V}) ->
            persistent_term:put({Table, K}, V)
        end, List).

get(Key) ->
    persistent_term:get(Key).

map_get(Table, Key) ->
    persistent_term:get({Table, Key}).

drop(Key) ->
    persistent_term:erase(Key).
-else.
insert(Key, Value) ->
    haki:cache(Key, Value).

insert_map(Table, Values) ->
    haki:cache_bucket(Table, Values).

get(Key) ->
    haki:get(Key).

map_get(Table, Key) ->
    Table:get(Key).

drop(Key) ->
    code:purge(Key),
    code:delete(Key).
-endif.