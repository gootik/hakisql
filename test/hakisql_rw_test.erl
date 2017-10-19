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
               name => <<"DragonCity_US_Android_VIDEO">>,
               application => <<"526e9c24b18bc0751d000009">>,
               start_date => <<"2013-10-28">>,
               end_date => <<"2020-09-30">>,
               stats => {stats, 434953, 398702, 328482, 13613, 4390},
               max_ver => {bidbox_normalized_version, 4, 0, 0},
               min_ver => {bidbox_normalized_version, infinity, 0, 0},
               blacklist => [<<"506481985c7b11ed297dac49">>, <<"507686ae771615941001aca5">>, <<"50ffc8e4a74d0bbc48000006">>],
               tags => [<<"builder">>],
               extra =>
               #{<<"mute">> => [],
                 <<"play_percentage">> => [],
                 <<"postroll_click">> => [],
                 <<"postroll_view">> => [<<"https://impression.appsflyer.com/es.socialpoint.DragonCity?pid=vungle_int&af_click_lookback=7d&android_id={{{isu}}}&advertising_id={{{aaid}}}&clickid={{{id}}}&af_siteid={{{site_id}}}&vungleappid={{{app_id}}}&af_cost_model=CPI&af_cost_value={{{bid_value}}}&af_cost_currency={{{bid_currency}}}&af_sub1={{{country}}}&af_sub2={{{device_android}}}&af_sub3={{{site_id}}}.{{{site_name}}}&af_ad_type={{{campaign_name}}}">>],
                 <<"unmute">> => [],
                 <<"video_close">> => []}},


    ok = hakisql:create(test_campaign, Table),
    ok = hakisql:insert(test_campaign, [Sample]),

    {ok, [#{id => <<"526ea26fb18bc0751d000027">>}]} = hakisql:q(test_campaign, "blacklist has <<'507686ae771615941001aca5'>>").
