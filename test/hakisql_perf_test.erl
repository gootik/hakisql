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
        #{a => test, b => 2, c => 3.1, name => "A"},
        #{a => test2, b => 24, c => 12.1, name => "B"},
        #{a => test, b => 24, c => 12.1, name => "C"},
        #{a => test2, b => 24, c => 12.1, name => "D"},
        #{a => test, b => 24, c => 12.1, name => "E"},
        #{a => test4, b => 24, c => 12.1, name => "F"}],

    TestDataRec = [
        #test_record{a = test, b = 2, c = 3.1, name = "A"},
        #test_record{a = test2, b = 24, c = 12.1, name = "B"},
        #test_record{a = test, b = 24, c = 12.1, name = "C"},
        #test_record{a = test2, b = 24, c = 12.1, name = "D"},
        #test_record{a = test, b = 24, c = 12.1, name = "E"},
        #test_record{a = test4, b = 24, c = 12.1, name = "F"}
    ],


    TestTable = ets:new(test_table, [named_table, public, duplicate_bag]),
    ets:insert(TestTable, TestDataRec),

    ok = hakisql:create(test, #{
        a => [index, atom],
        b => [index, number],
        c => [number],
        name => [string]
    }),

    ok = hakisql:insert(test, TestDataMap),
    {ok, [#{a := test, b := 2, c := 3.1, name := "A"}]} = hakisql:q(test, "b = 2"),

    MatchSpec = ets:fun2ms(fun(#test_record{b = B} = R) when B =:= 2 -> R end),

    EtsTiming = timing:function(
        fun() ->
            [#test_record{a = test, b = 2, c = 3.1, name = "A"}] = ets:select(test_table, MatchSpec)
        end, 1000),

    HakiTiming = timing:function(
        fun() ->
            {ok, [#{a := test, b := 2, c := 3.1, name := "A"}]} = hakisql:q(test, "b = 2")
        end, 1000),

    io:format("ETS: ~p~n", [EtsTiming]),
    io:format("HAKI: ~p~n", [HakiTiming]).

