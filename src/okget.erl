-module(okget).

-export([ok/1]).

ok({ok,Term}) -> Term.
