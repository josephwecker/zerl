-module(rex).

-export([replace_all/3]).

-import(lists, [member/2, ukeysort/2, ukeymerge/3]).
-import(proplists, [unfold/1, compact/1]).

-define(COMPILE_OPTS, [
  ]).

-define(RUN_OPTS, [
  ]).

replace_all(Subj, RE, Replacement) ->
  replace_all(Subj, RE, Replacement, []).
replace_all(Subj, RE, Replacement, Opts) ->
  RE2 = memoize_regex(RE, fix_opts_compile(Opts, [dotall, multiline])),
  replace_all_inner(Subj, RE2, Replacement, Opts).

replace_all_type([H|_] = Subj, RE2, Replacement, Opts) when is_list(Subj) and is_integer(H) ->
  re:replace(Subj, RE2, Replacement,
    fix_opts_run(Opts, [global, {return, charlist}]);
replace_all_type(Subj, RE2, Replacement, Opts) when is_binary(Subj) ->
  re:replace(Subj, RE2, Replacement,
    fix_opts_run(Opts, [global, {return, binary}]);
replace_all_type(Subj, RE2, Replacement, Opts) ->
  re:replace(Subj, RE2, Replacement,
    fix_opts_run(Opts, [global]).

fix_opts_compile(FromUser, FromFun) ->
  Merged = ukeymerge(1, ukeysort(1, unfold(FromUser)),
                        ukeysort(1, unfold(FromFun))),
  compact([{K,V} || {K,V} <- Merged, member(K, ?COMPILE_OPTS)]).

fix_opts_compile(FromUser, FromFun) ->
  Merged = ukeymerge(1, ukeysort(1, unfold(FromUser)),
                        ukeysort(1, unfold(FromFun))),
  compact([{K,V} || {K,V} <- Merged, member(K, ?RUN_OPTS)]).

