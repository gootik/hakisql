-module(hakisql_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ok = hakisql:create(test, #{
            a => [index, atom],
            b => [index, number],
            c => [number],
            name => [string]
    }),

    ok = hakisql:insert(test, [
        #{a => test, b => 2, c => 3.1, name => "A"},
        #{a => test2, b => 24, c => 12.1, name => "B"},
        #{a => test, b => 24, c => 12.1, name => "C"},
        #{a => test2, b => 24, c => 12.1, name => "D"},
        #{a => test, b => 24, c => 12.1, name => "E"},
        #{a => test4, b => 24, c => 12.1, name => "F"}
    ]),

    {ok, [#{a := test, b := 2, c := 3.1, name := "A"}]} = hakisql:q(test, "b = 2").

simple_not_test() ->
    ok = hakisql:create(test_not, #{
        a => [index, atom],
        b => [index, number],
        c => [number],
        name => [string]
    }),

    ok = hakisql:insert(test_not, [
        #{a => test, b => 2, c => 3.1, name => "A"},
        #{a => test2, b => 24, c => 12.1, name => "B"},
        #{a => test, b => 24, c => 12.1, name => "C"},
        #{a => test2, b => 24, c => 12.1, name => "D"},
        #{a => test, b => 24, c => 12.1, name => "E"},
        #{a => test4, b => 24, c => 12.1, name => "F"}
    ]),

    {ok, [#{a := test, b := 2, c := 3.1, name := "A"}]} = hakisql:q(test_not, "b != 24").


range_test() ->
    hakisql:create(range_test, #{
        b => [index]
    }),

    ok = hakisql:insert(range_test, [
        #{a => test, b => 2, c => 3.1, name => "A"},
        #{a => test, b => 3, c => 3.1, name => "A"},
        #{a => test, b => 4, c => 3.1, name => "A"},
        #{a => test, b => 5, c => 3.1, name => "A"},
        #{a => test, b => 6, c => 3.1, name => "A"},
        #{a => test, b => 10, c => 3.1, name => "A"},
        #{a => test, b => 400, c => 3.1, name => "A"}
    ]),

    {ok, _R1} = hakisql:q(test, "b <= 400"),
    {ok, _R2} = hakisql:q(test, "b <= 2"),
    {ok, _R3} = hakisql:q(test, "b >= 400"),
    {ok, _R4} = hakisql:q(test, "b >= 2"),

    {ok, _R5} = hakisql:q(test, "b <= 6 AND b >= 3").