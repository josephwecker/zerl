-module(prompts).

-export([if_promptc/2, if_promptyn/3]).


if_promptyn(Prompt, YesFun, NoFun) ->
  if_promptc(Prompt,
    [{"yY", fun(_) -> YesFun() end},
     {"nN", fun(_) -> NoFun() end}]).

if_promptc(Prompt, Cases) ->
  case io:get_chars(Prompt, 1) of
    eof -> halt(3);
    {error, Reason} -> io:format(standard_error, "ERROR: ~p", [Reason]);
    [C] ->
      case run_c(C, Cases) of
        '$X-PROMPTS-CHAR-NOT-FOUND' -> if_promptc(Prompt, Cases);
        RV -> RV
      end
  end.

run_c(_C, []) ->
  '$X-PROMPTS-CHAR-NOT-FOUND';
run_c(C, [{LookFor, RunFun} | T]) ->
  case lists:member(C, LookFor) of
    true -> RunFun(C);
    false -> run_c(C, T)
  end.

