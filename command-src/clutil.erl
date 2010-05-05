-module(clutil).

-export([
  cancel/0,
  stat/1, stat/2,
  estat/1, estat/2,
  abort/1, abort/2, abort/3,
  make_dirp/1,
  promptyn/3, promptc/2,
  script_name/0, script_dir/0
  ]).

cancel() ->
  estat("Canceled."),
  halt(2).

stat(Txt) ->
  o_stat(standard_io, Txt, []).
stat(Txt, Args) ->
  o_stat(standard_io, Txt, Args).
estat(Txt) ->
  o_stat(standard_error, Txt, []).
estat(Txt, Args) ->
  o_stat(standard_error, Txt, Args).

abort(Msg) ->
  abort(Msg, []).
abort(Msg, Args) ->
  abort(Msg, Args, 100).
abort(Msg, Args, ErrNumber) ->
  estat(Msg, Args),
  halt(ErrNumber).

o_stat(IoDev, Txt, []) ->
  io:format(IoDev, Txt ++ "\n", []);
o_stat(IoDev, Txt, [Args]) when not is_list(Args) ->
  io:format(IoDev, Txt ++ "\n", [Args]);
o_stat(IoDev, Txt, Args) ->
  io:format(IoDev, Txt ++ "\n", [Args]).

make_dirp(Dir) ->
  case filelib:ensure_dir(Dir) of
    ok ->
      case file:make_dir(Dir) of
        ok -> ok;
        {error, eexist} -> ok;
        OtherErr -> OtherErr
      end;
    OtherErr2 -> OtherErr2
  end.

promptyn(Prompt, YesFun, NoFun) ->
  promptc(Prompt,
    [{"yY", fun(_) -> YesFun() end},
     {"nN", fun(_) -> NoFun() end}]).

promptc(Prompt, Cases) ->
  Line = io:get_line(Prompt),
  case Line of
    eof -> halt(3);
    {error, Reason} -> io:format(standard_error, "ERROR: ~p", [Reason]);
    [C, $\n] ->
      case run_c(C, Cases) of
        '$X-PROMPTS-CHAR-NOT-FOUND' -> promptc(Prompt, Cases);
        RV -> RV
      end;
    UnRec ->
      io:format(standard_error, "Unrecognized input: ~p", [UnRec]),
      promptc(Prompt, Cases)
  end.

run_c(_C, []) ->
  '$X-PROMPTS-CHAR-NOT-FOUND';
run_c(C, [{LookFor, RunFun} | T]) ->
  case lists:member(C, LookFor) of
    true -> RunFun(C);
    false -> run_c(C, T)
  end.

script_name() ->
  follow_file(escript:script_name()).

script_dir() ->
  filename:dirname(script_name()).

follow_file(CurrFile) ->
  case file:read_link(CurrFile) of
    {error, einval} -> CurrFile;
    {error, enotsup} -> CurrFile;
    {ok, NewFile} -> follow_file(NewFile);
    Err -> Err
  end.

