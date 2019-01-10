-module(hakisql_rw_test).

-include_lib("eunit/include/eunit.hrl").

campaign_test() ->
    Table = #{
        id => [index],
        application => [index],
        max_ver => [index],
        blacklist => [index]
    },

    Sample = #{id => <<"526ea26fb18bc0751d000027">>,
               name => <<"Campaign_Name">>,
               application => <<"526e9c24b18bc0751d000009">>,
               start_date => <<"2013-10-28">>,
               end_date => <<"2020-09-30">>,
               stats => {stats, 434953, 398702, 328482, 13613, 4390},
               max_ver => {semver, 4, 0, 0},
               min_ver => {semver, infinity, 0, 0},
               blacklist => [<<"506481985c7b11ed297dac49">>, <<"507686ae771615941001aca5">>, <<"50ffc8e4a74d0bbc48000006">>],
               tags => [<<"builder">>],
               extra => #{
                   <<"mute">> => [],
                   <<"postroll_view">> => [<<"https://someurl.com">>]
               }},

    ok = hakisql:create(test_campaign, Table),
    ok = hakisql:insert(test_campaign, [Sample]),

    {ok, [#{id := <<"526ea26fb18bc0751d000027">>}]} = hakisql:q(test_campaign, "blacklist = <<'507686ae771615941001aca5'>>").

campaign_ver_test() ->
    Table = #{
        id => [index],
        application => [index],
        max_ver => [index],
        blacklist => [index]
    },

    Sample = #{id => <<"526ea26fb18bc0751d000027">>,
               name => <<"Campaign_Name">>,
               application => <<"526e9c24b18bc0751d000009">>,
               start_date => <<"2013-10-28">>,
               end_date => <<"2020-09-30">>,
               stats => {stats, 434953, 398702, 328482, 13613, 4390},
               max_ver => {semver, 4, 0, 0},
               min_ver => {semver, infinity, 0, 0},
               blacklist => [<<"506481985c7b11ed297dac49">>, <<"507686ae771615941001aca5">>, <<"50ffc8e4a74d0bbc48000006">>],
               tags => [<<"builder">>],
               extra => #{
                   <<"mute">> => [],
                   <<"postroll_view">> => [<<"https://someurl.com">>]
               }},

    ok = hakisql:create(test_campaign_ver, Table),
    ok = hakisql:insert(test_campaign_ver, [Sample]),

    {ok, [#{id := <<"526ea26fb18bc0751d000027">>}]} = hakisql:q(test_campaign_ver, "max_ver = {semver, 4, 0, 0}").
