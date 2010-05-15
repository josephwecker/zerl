-module(zerl_req, [MReq]).

% Important request data
-export([
    resource/1,
    params/0,
    p/1
  ]).

% Responses
-export([
    ok/1, ok/2, ok/3,
    respond/2, respond/3, respond/4,
    stream/1, stream/2, stream/3,
    file/1, file/2
  ]).

% Detailed request data
-export([
    raw/0,
    socket/0, socket_mode/0, connection/0,
    peer_addr/0, peer_port/0, peer_cert/0,
    content_length/0,
    version/0, method/0,
    uri/0, args/0,
    headers/0, body/0, cookies/0,
    header/1, cookie/1
  ]).

resource(Options) ->
  MReq:resource(Options).

params() ->
  % Process dictionary safe for these??
  case get('__zerl_params') of
    undefined ->
      P = params2(),
      put('__zerl_params', P),
      P;
    P ->
      P
  end.

params2() ->
  case method() of
    'POST' ->
      MReq:parse_qs() ++ MReq:parse_post() ++ cookies();
    _ ->
      MReq:parse_qs() ++ cookies()
  end.

p(Key) ->
  proplists:get_value(

raw() ->
  MReq:raw().

ok(Template) ->
  MReq:ok(Template).
ok(Headers, Template) ->
  MReq:ok(Headers, Template).
ok(Headers, Template, Vars) ->
  MReq:ok(Headers, Template, Vars).

respond(HttpCode, Template) ->
  MReq:respond(HttpCode, Template).
respond(HttpCode, Headers, Template) ->
  MReq:respond(HttpCode, Headers, Template).
respond(HttpCode, Headers, Template, Vars) ->
  MReq:respond(HttpCode, Headers, Template, Vars).

stream(close) ->
  MReq:stream(close).
stream(head) ->
  MReq:stream(head).
stream(Template) ->
  MReq:stream(Template).
stream(Template, Vars) ->
  MReq:stream(Template, Vars).
stream(head, HttpCode, Headers) ->
  MReq:stream(head, HttpCode, Headers).

get(Key) ->
  MReq:get(Key).


headers() ->
  MReq:get(headers).
get(header, Name) when is_list(Name) ->
  get(header, list_to_atom(Name));
get(header, Name) ->
  proplists:get_value(Name, get(headers));
get(cookie, Name) ->
  proplists

parse_qs() ->
  MReq:parse_qs().
parse_post() ->
  MReq:parse_post().

file(FilePath) ->
  MReq:file(FilePath).
file(attachment, FilePath) ->
  MReq:file(attachment, FilePath).

