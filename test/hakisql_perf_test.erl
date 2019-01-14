-module(hakisql_perf_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(test_record, {
    a :: atom(),
    b :: integer(),
    c :: float(),
    name :: string()
}).

simple_ets_test() ->

    TestDataMap = [
        #{a => test, b => 2, c => 3.1, name => <<"A">>},
        #{a => test2, b => 24, c => 12.1, name => "B"},
        #{a => test, b => 24, c => 12.1, name => "C"},
        #{a => test2, b => 24, c => 12.1, name => "D"},
        #{a => test, b => 24, c => 12.1, name => "E"},
        #{a => test4, b => 24, c => 12.1, name => "F"}],

    TestDataRec = [
        #test_record{a = test, b = 2, c = 3.1, name = <<"A">>},
        #test_record{a = test2, b = 24, c = 12.1, name = "B"},
        #test_record{a = test, b = 24, c = 12.1, name = "C"},
        #test_record{a = test2, b = 24, c = 12.1, name = "D"},
        #test_record{a = test, b = 24, c = 12.1, name = "E"},
        #test_record{a = test4, b = 24, c = 12.1, name = "F"}
    ],

    FinalTestDataMap = lists:foldl(
        fun(_, Acc) ->
            [#{a => test, b => 2, c => 3.1, name => <<"A">>} | Acc]
        end, TestDataMap, lists:seq(0, 1000)),

    FinalTestDataRec = lists:foldl(
        fun(_, Acc) ->
            [#test_record{a = test, b = 2, c = 3.1, name = <<"A">>} | Acc]
        end, TestDataRec, lists:seq(0, 1000)),


    TestTable = ets:new(test_table, [named_table, public, duplicate_bag]),
    ets:insert(TestTable, FinalTestDataRec),

    ok = hakisql:create(test, #{
        a => [index, atom],
        b => [index, number],
        c => [index, number],
        name => [string]
    }),

    ok = hakisql:insert(test, FinalTestDataMap),
    {ok, _} = hakisql:q(test, "b = 2 AND c = 3.1"),

    MatchSpec = ets:fun2ms(fun(#test_record{b = B, c = C} = R) when B =:= 2 andalso C =:= 3.1 -> R end),

    EtsTiming = timing:function(
        fun() ->
            _ = ets:select(test_table, MatchSpec)
        end, 1000),

    HakiTiming = timing:function(
        fun() ->
            {ok, _} = hakisql:q(test, "b = 2 AND c = 3.1")
        end, 1000),

    {min, HakiMin} = lists:keyfind(min, 1, HakiTiming),
    {min, EtsMin} = lists:keyfind(min, 1, EtsTiming),

    ?assert(EtsMin >= HakiMin),

    io:format("~p(ETS) ~p(Haki)~n", [EtsMin, HakiMin]),

    ets:delete(test_table),
    hakisql:drop(test).
