-module(zerl).

-export([main/1]).

main([Command | Rest]) ->
  command(list_to_atom(Command), Rest);
main(_) ->
  usage().

command(init, [Name | Opts]) ->
  io:format("Creating new project ~s...\n", [Name]);

command(_, _) ->
  usage().


usage() ->
  io:format(standard_error,
    "Usage:
    init           ProjectName
    new view       ViewName
    new controller ControllerName
    new model      ModelEntityName
    new behavior   StoryName
    server         [start | console | stop] [Environment=development]
      (console connects to running server if there is one)
    command        [Environment=development] Command [Opts...]
    help           [Command]
", []),
  halt(1).
