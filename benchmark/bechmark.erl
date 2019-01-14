-module(bechmark).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


-export([
    complex_test/0
]).

-record(complex_record, {
    id :: binary(),
    name :: binary(),
    application :: binary(),
    start_date :: binary(),
    end_date :: binary(),
    stats,
    max_ver,
    min_ver,
    blacklist :: list(binary()),
    tags :: list(binary()),
    extra :: map()
}).

complex_test() ->
    TestTable = ets:new(hakisql_complex_table, [
        named_table,
        public,
        duplicate_bag,
        {write_concurrency, true},
        {read_concurrency, true}
    ]),

    SqlTable = #{
        id => [index],
        application => [index],
        max_ver => [index],
        blacklist => [index]
    },


    Samples = lists:foldl(
        fun(_, Acc) ->
            Row = #{
                id => seq_binary(<<"id">>, 10),
                application => seq_binary(<<"application">>, 10),
                name => random_binary(16),
                start_date => random_binary(6),
                end_date => random_binary(6),
                stats => random_stats(),
                max_ver => random_ver(),
                min_ver => random_ver(),
                blacklist => lists:map(fun(_) -> seq_binary(<<"blacklist">>, 5) end, lists:seq(0, 3)),
                tags => lists:map(fun(_) -> seq_binary(<<"tag">>, 5) end, lists:seq(0, 3)),
                extra => #{
                    <<"mute">> => false,
                    <<"view">> => [<<"static">>]
                }
            },

            [Row | Acc]
        end, [], lists:seq(1, 10000)),

    Recs = lists:map(
        fun(Row) ->
            #{id := Id, application := Application, name := Name, start_date := StartDate,
              end_date := EndDate, stats := Stats, max_ver := MaxVer,
              min_ver := MinVer, blacklist := Blacklist, tags := Tags, extra := Extra} = Row,

            #complex_record{
                id = Id,
                application = Application,
                name = Name,
                start_date = StartDate,
                end_date = EndDate,
                stats = Stats,
                max_ver = MaxVer,
                min_ver = MinVer,
                blacklist = Blacklist,
                tags = Tags,
                extra = Extra
            }
        end, Samples),

    ets:insert(TestTable, Recs),
    hakisql:create(sql_table, SqlTable),
    hakisql:insert(sql_table, Samples),

    MatchSpec = ets:fun2ms(fun(#complex_record{id = Id, application = Application} = R) when Application =:= <<"application_1">> andalso Id =:= <<"id_1">> -> R end),

    L2 = ets:select(hakisql_complex_table, MatchSpec),

    {ok, L} = hakisql:q(sql_table, "id = <<'id_1'>> AND application = <<'application_1'>>"),

    ?assert(length(L) > 1),
    ?assert(length(L2) > 1),

    EtsTiming = eministat:s("EtsTiming",
                            fun() ->
                                _ = ets:select(hakisql_complex_table, MatchSpec)
                            end, 1000),

    HakiTiming = eministat:s("HakiTiming",
                             fun() ->
                                 {ok, _} = hakisql:q(sql_table, "id = <<'id_1'>> AND application = <<'application_1'>>")
                             end, 1000),


    eministat:x(95.0, EtsTiming, HakiTiming),

    ets:delete(hakisql_complex_table),
    hakisql:drop(sql_table).

random_stats() ->
    {stats,
     rand:uniform(10000),
     rand:uniform(20000),
     rand:uniform(30000)}.

random_ver() ->
    {semver, rand:uniform(4), 0, 0}.

seq_binary(Prefix, Max) ->
    Integer = integer_to_binary(rand:uniform(Max)),
    <<Prefix/bitstring, "_", Integer/binary>>.

random_binary(Size) ->
    base64:encode(crypto:strong_rand_bytes(Size)).