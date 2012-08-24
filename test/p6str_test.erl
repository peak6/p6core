-module(p6str_test).

-include_lib("eunit/include/eunit.hrl").

to_lower_bin_test_() ->
    [
     ?_assertEqual(<<"marketmash">>, p6str:to_lower_bin("MarketMash")),
     ?_assertEqual(<<"marketmash">>, p6str:to_lower_bin("marketMash")),
     ?_assertEqual(<<"marketmash">>, p6str:to_lower_bin("marketmash")),
     ?_assertEqual(<<"market_mash">>, p6str:to_lower_bin("market_Mash")),
     ?_assertEqual(<<"market_mash">>, p6str:to_lower_bin("MARKET_MASH")),

     ?_assertEqual(<<"marketmash">>, p6str:to_lower_bin(<<"MarketMash">>)),
     ?_assertEqual(<<"marketmash">>, p6str:to_lower_bin(<<"marketMash">>)),
     ?_assertEqual(<<"marketmash">>, p6str:to_lower_bin(<<"marketmash">>)),
     ?_assertEqual(<<"market_mash">>, p6str:to_lower_bin(<<"market_Mash">>)),
     ?_assertEqual(<<"market_mash">>, p6str:to_lower_bin(<<"MARKET_MASH">>)),

     ?_assertEqual(<<"marketmash">>, p6str:to_lower_bin('MarketMash')),
     ?_assertEqual(<<"marketmash">>, p6str:to_lower_bin('marketMash')),
     ?_assertEqual(<<"marketmash">>, p6str:to_lower_bin('marketmash')),
     ?_assertEqual(<<"market_mash">>, p6str:to_lower_bin('market_Mash')),
     ?_assertEqual(<<"market_mash">>, p6str:to_lower_bin('MARKET_MASH'))
    ].


