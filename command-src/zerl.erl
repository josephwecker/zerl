-module(zerl).

-export([main/1]).

-define(LAYOUT, [
    {"./", ["README.md"]},
    {"./behaviors", [{"README.behaviors.md","README.md"}]},
    {"./config", ["routes.erl", {"README.config.md", "README.md"}]},
    {"./controllers", [{"README.controllers.md", "README.md"}]},
    {"./test", [{"README.test.md", "README.md"}]},
    {"./views", [{"README.views.md", "README.md"}]}
  ].

-define(S_INIT_DEEP,
"You seem to already be inside another zerl project. Continue? (y/n):"
).
-define(S_INIT_ROOT,
"This seems to already be a zerl project.
Refresh project? (WARNING- this can change project name, etc. etc.) (y/n):"
).


main([Command | Rest]) ->
  command(list_to_atom(Command), Rest);
main(_) ->
  usage().

command(init, [Name | Opts]) ->
  case is_in_project() of
    deep -> prompts:if_promptyn(?S_INIT_DEEP,
        fun() -> initialize_project(Name, Opts) end, fun cancel/0);
    root -> prompts:if_promptyn(?S_INIT_ROOT,
        fun() -> refresh_project(Name, Opts) end, fun cancel/0);
    false ->
      initialize_project(Name, Opts)
  end;

command(_, _) ->
  usage().


usage() ->
  io:format(standard_error,
    "Usage (NYI = Not Yet Implemented):
    init           ProjectName (NYI)
    new view       ViewName (NYI)
    new controller ControllerName (NYI)
    new model      ModelEntityName (NYI)
    new behavior   StoryName (NYI)
    server         [start | console | stop] [Environment=development] (NYI)
      (console connects to running server if there is one)
    command        [Environment=development] Command [Opts...] (NYI)
    help           [Command]
", []),
  halt(1).

cancel() ->
  io:format(standard_error, "Canceled.\n", []),
  halt(2).


is_in_project() ->
  deep.

initialize_project(Name, _Options) ->
  io:format("Creating new project ~s...\n", [Name]).

refresh_project(_Name, _Options) ->
  io:format(standard_error, "Sorry, I don't know how to do that yet.\n", []),
  cancel().

