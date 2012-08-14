-module(p6str_test).

-include_lib("eunit/include/eunit.hrl").

mkservicename_test_() ->
    [
     ?_assertEqual(<<"marketmash">>, p6str:mkservicename("MarketMash")),
     ?_assertEqual(<<"marketmash">>, p6str:mkservicename("marketMash")),
     ?_assertEqual(<<"marketmash">>, p6str:mkservicename("marketmash")),
     ?_assertEqual(<<"market_mash">>, p6str:mkservicename("market_Mash")),
     ?_assertEqual(<<"market_mash">>, p6str:mkservicename("MARKET_MASH")),

     ?_assertEqual(<<"marketmash">>, p6str:mkservicename(<<"MarketMash">>)),
     ?_assertEqual(<<"marketmash">>, p6str:mkservicename(<<"marketMash">>)),
     ?_assertEqual(<<"marketmash">>, p6str:mkservicename(<<"marketmash">>)),
     ?_assertEqual(<<"market_mash">>, p6str:mkservicename(<<"market_Mash">>)),
     ?_assertEqual(<<"market_mash">>, p6str:mkservicename(<<"MARKET_MASH">>)),

     ?_assertEqual(<<"marketmash">>, p6str:mkservicename('MarketMash')),
     ?_assertEqual(<<"marketmash">>, p6str:mkservicename('marketMash')),
     ?_assertEqual(<<"marketmash">>, p6str:mkservicename('marketmash')),
     ?_assertEqual(<<"market_mash">>, p6str:mkservicename('market_Mash')),
     ?_assertEqual(<<"market_mash">>, p6str:mkservicename('MARKET_MASH'))
    ].


