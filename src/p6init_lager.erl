-module(p6init_lager).

-include("p6init.hrl").

-export([initLager/2]).

-define(lset(K,V), application:set_env(lager,K,V)).

initLager(Env,_Props) ->
    application:load(sasl),
    application:set_env(sasl,errlog_type,error),
    application:load(lager),
    application:load(p6core),
    case application:get_env(p6core,log_dir) of
        {ok,LogDir} -> ok;
        _ -> LogDir = "log"
    end,
    ?lset(crash_log_count,5),
    ?lset(included_applications,[]),
    ?lset(crash_log_date,"$D0"),
    ?lset(crash_log_size,10485760),
    ?lset(crash_log_msg_size,65536),
    ?lset(crash_log,p6str:mkstr("~s/crash.log",[LogDir])),
    case application:get_env(p6core,log_console) of
        {ok,true} -> Console = [{lager_console_backend,[debug,true]}];
        _ -> Console = []
    end,
    
    ?lset(handlers,
          Console ++ [
                      {lager_file_backend,[{p6str:mkstr("~s/server.log",[LogDir]),debug,10485760,"$D0",5}]}
                     ]),
    ?lset(error_logger_redirect,true),
    Env.

