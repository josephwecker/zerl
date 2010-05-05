-module(rex).

-export([replace_all/3]).

-import(lists, [member/2, ukeysort/2, ukeymerge/3]).
-import(proplists, [unfold/1, compact/1]).

-define(COMPILE_OPTS, [
    unicode, anchored, caseless, dollar_endonly, dotall, extended, firstline,
    multiline, no_auto_capture, dupnames, ungreedy, newline, bsr_anycrlf,
    bsr_unicode
  ]).

-define(RUN_OPTS, [
    anchored, global, notbol, noteol, notempty, offset, newline, bsr_anycrlf,
    bsr_unicode, capture, return
  ]).

replace_all(Subj, RE, Replacement) ->
  replace_all(Subj, RE, Replacement, []).
replace_all(Subj, RE, Replacement, Opts) ->
  RE2 = memoize_regex(RE, fix_opts_compile(Opts, [dotall, multiline])),
  replace_all_type(Subj, RE2, Replacement, Opts).

replace_all_type([H|_] = Subj, RE2, Replacement, Opts) when is_list(Subj) and is_integer(H) ->
  re:replace(Subj, RE2, Replacement,
    fix_opts_run(Opts, [global, {return, list}]));
replace_all_type(Subj, RE2, Replacement, Opts) when is_binary(Subj) ->
  re:replace(Subj, RE2, Replacement,
    fix_opts_run(Opts, [global, {return, binary}]));
replace_all_type(Subj, RE2, Replacement, Opts) ->
  re:replace(Subj, RE2, Replacement,
    fix_opts_run(Opts, [global])).

fix_opts_compile(FromUser, FromFun) ->
  fix_opts(FromUser, FromFun, ?COMPILE_OPTS).
fix_opts_run(FromUser, FromFun) ->
  fix_opts(FromUser, FromFun, ?RUN_OPTS).

fix_opts(FromUser, FromFun, Members) ->
  Merged = ukeymerge(1, ukeysort(1, unfold(FromUser)),
                        ukeysort(1, unfold(FromFun))),
  compact([{K,V} || {K,V} <- Merged, member(K, Members)]).

memoize_regex(RE, Options) ->
  Key = {RE, lists:sort(Options)},
  case get(Key) of
    undefined ->
      {ok, Compiled} = re:compile(RE, Options),
      put(Key, Compiled),
      Compiled;
    Memoized ->
      Memoized
  end.
