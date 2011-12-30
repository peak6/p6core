-module(p6init_file_test).

-include_lib("eunit/include/eunit.hrl").
-include("p6init.hrl").

%% Note: these tests run from $TOP/.eunit, so use relative paths

path_test() ->
    Res = p6init_file:loadFile(#env{noexit=true},"../test/test1.txt"),
    HasTestDir = lists:any(fun(P) -> lists:suffix("/test",P) end, Res#env.paths),
    LoadedTest2 = lists:any(fun(P) -> lists:suffix("test2.txt",P) end, Res#env.loaded),
    ?assertEqual(true,HasTestDir),
    ?assertEqual(true,LoadedTest2).
