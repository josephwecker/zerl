-module(zerl_parse_opts).

-export([parse/1]).

parse(Opts) ->
  [lists:flatten(Opts)].
