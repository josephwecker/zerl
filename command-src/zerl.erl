-module(zerl).

-export([main/1]).

main(Opts) ->
  io:format(zerl_parse_opts:parse(Opts)).
