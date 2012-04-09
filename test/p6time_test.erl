-module(p6time_test).

-include_lib("eunit/include/eunit.hrl").
-include("time.hrl").

nop_test_() ->
	[ ?_assertEqual(2,p6time:convert(2,Type,Type)) || Type <- [mega,s,ms,us] ].

sec_msu_test() ->
	?assertEqual({0,1,0},p6time:convert(1,s,msu)).

ms_msu_test() ->
	?assertEqual({0,0,1000},p6time:convert(1,ms,msu)).

us_msu_test() ->
	?assertEqual({0,0,1},p6time:convert(1,us,msu)).

normalize_no_roll_test() ->
	?assertEqual({1,1,1},p6time:normalize({1,1,1})).

normalize_roll_sec_test() ->
	?assertEqual({2,1,1},p6time:normalize({1,?MEGA_SEC+1,1})).

normalize_roll_us_test() ->
	?assertEqual({1,2,1},p6time:normalize({1,1,?SEC_US+1})).

