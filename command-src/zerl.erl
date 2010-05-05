-module(zerl).

-export([main/1]).

-import(clutil, [
  estat/1, estat/2,
  cancel/0,
  stat/1, stat/2,
  make_dirp/1,
  abort/2,
  script_dir/0,
  promptyn/3
  ]).


-define(LAYOUT, [
    {"./", ["README.md", {"zerl_project", ".zerl_project"}]},
    {"./behaviors", [{"README.behaviors.md","README.md"}]},
    {"./config", ["routes.erl", {"README.config.md", "README.md"}]},
    {"./controllers", [{"README.controllers.md", "README.md"}]},
    {"./test", [{"README.test.md", "README.md"}]},
    {"./views", [{"README.views.md", "README.md"}]}
  ].

-define(S_INIT_DEEP,
"You seem to already be inside another zerl project. Continue? (y/n): "
).
-define(S_INIT_ROOT,
"This seems to already be a zerl project.
Refresh project? (WARNING- this can change project name, etc. etc.) (y/n): "
).


main([Command | Rest]) ->
  command(list_to_atom(Command), Rest);
main(_) ->
  usage().

command(init, [Name | Opts]) ->
  case is_in_project() of
    deep -> promptyn(?S_INIT_DEEP,
        fun() -> initialize_project(Name, Opts) end, fun clutil:cancel/0);
    root -> promptyn(?S_INIT_ROOT,
        fun() -> refresh_project(Name, Opts) end, fun clutil:cancel/0);
    false ->
      initialize_project(Name, Opts)
  end;

command(_, _) ->
  usage().


usage() ->
  estat("Usage (NYI = Not Yet Implemented):
    init           ProjectName (NYI)
    new view       ViewName (NYI)
    new controller ControllerName (NYI)
    new model      ModelEntityName (NYI)
    new behavior   StoryName (NYI)
    server         [start | console | stop] [Environment=development] (NYI)
      (console connects to running server if there is one)
    command        [Environment=development] Command [Opts...] (NYI)
    help           [Command]"),
  halt(1).

is_in_project() ->
  false.

initialize_project(Name, Options) ->
  stat("Creating new project ~s...", Name),
  create_layout(Name, Options,
    filename:join([script_dir(),"..","templates"]),
    ?LAYOUT),
  stat("Done.").

refresh_project(_Name, _Options) ->
  estat("Sorry, I don't know how to do that yet."),
  cancel().

create_layout(_, _, _, []) ->
  ok;
create_layout(Name, Opts, TmplDir, [{NewDir, NewFiles} | T]) ->
  ok = make_dirp(NewDir),
  create_files(Name, Opts, TmplDir, NewDir, NewFiles),
  create_layout(Name, Opts, TmplDir, T).

create_files(_, _, _, _, []) ->
  ok;
create_files(PName, Opts, TmplDir, NewDir, [{SrcName, DstName} | T]) ->
  Dest = filename:join([NewDir, DstName]),
  Src = filename:join([TmplDir, SrcName]),
  case filelib:is_file(Src) of
    false -> abort("Zerl templates seem to be missing (looking for ~p)", Src);
    true ->
      case filelib:is_file(Dest) of
        true ->
          promptyn(
            "File "++Dest++" already exists. Overwrite? (y/n): ",
            fun() -> create_file_from_tmpl(PName, Opts, Dest, Src) end,
            fun() -> estat("Skipping ~p", [Dest]) end);
        false ->
          create_file_from_tmpl(PName, Opts, Dest, Src)
      end
  end,
  create_files(PName, Opts, TmplDir, NewDir, T);
create_files(PName, Opts, TmplDir, NewDir, [SrcDstName | T]) ->
  create_files(PName, Opts, TmplDir, NewDir, [{SrcDstName, SrcDstName} | T]).

create_file_from_tmpl(PName, _Opts, Dest, Src) ->
  stat("Creating ~p", Dest),
  {ok, Tmpl} = file:read_file(Src),
  ok = file:write_file(Dest, rex:replace_all(Tmpl, "\!project\!", PName)).

