% ==========================================================================================================
% MISULTIN - Socket
%
% >-|-|-(°>
% 
% Copyright (C) 2010, Roberto Ostinelli <roberto@ostinelli.net>, Sean Hinde.
% All rights reserved.
%
% Code portions from Sean Hinde have been originally taken under BSD license from Trapexit at the address:
% <http://www.trapexit.org/A_fast_web_server_demonstrating_some_undocumented_Erlang_features>
%
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%	 following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%	 the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%	 products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
% ==========================================================================================================
-module(misultin_socket).
-vsn("0.5.0").

% API
-export([start_link/7]).

% callbacks
-export([listener/7]).

% internal
-export([socket_loop/2]).
-export([listen/3, setopts/3, send/3, close/2]).

% macros
-define(MAX_HEADERS_COUNT, 100).

% records
-record(c, {
	sock,
	socket_mode,
	port,
	loop,
	recv_timeout,
	stream_support,
	ws_loop
}).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the socket.
start_link(ListenSocket, ListenPort, Loop, RecvTimeout, StreamSupport, WsLoop, SocketMode) ->
	proc_lib:spawn_link(?MODULE, listener, [ListenSocket, ListenPort, Loop, RecvTimeout, StreamSupport, WsLoop, SocketMode]).

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the socket.
listener(ListenSocket, ListenPort, Loop, RecvTimeout, StreamSupport, WsLoop, SocketMode) ->
	case catch accept(ListenSocket, SocketMode) of
		{ok, {sslsocket, _, _} = Sock} ->
			% received a SSL socket -> spawn a ssl_accept process to avoid locking the main listener
			spawn(fun() ->
				case ssl:ssl_accept(Sock, 60000) of
					ok ->
						create_socket_pid(Sock, ListenPort, Loop, RecvTimeout, StreamSupport, WsLoop, SocketMode);
					{error, _Reason} ->
						% could not negotiate a SSL transaction, leave process
						?LOG_WARNING("could not negotiate a SSL transaction: ~p", [_Reason]),
						catch close(Sock, SocketMode)
				end
			end),
			% get back to accept loop
			listener(ListenSocket, ListenPort, Loop, RecvTimeout, StreamSupport, WsLoop, SocketMode);
		{ok, Sock} ->
			% received a HTTP socket
			create_socket_pid(Sock, ListenPort, Loop, RecvTimeout, StreamSupport, WsLoop, SocketMode),
			% get back to accept loop
			listener(ListenSocket, ListenPort, Loop, RecvTimeout, StreamSupport, WsLoop, SocketMode);
		{error, _Error} ->
			?LOG_WARNING("accept failed with error: ~p", [_Error]),
			% get back to accept loop
			listener(ListenSocket, ListenPort, Loop, RecvTimeout, StreamSupport, WsLoop, SocketMode);
		{'EXIT', Error} ->
			?LOG_ERROR("accept exited with error: ~p, quitting process", [Error]),
			exit({error, {accept_failed, Error}})
	end.

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% start socket Pid
create_socket_pid(Sock, ListenPort, Loop, RecvTimeout, StreamSupport, WsLoop, SocketMode) ->
	?LOG_DEBUG("accepted an incoming TCP connection in ~p mode on socket ~p, spawning controlling process", [SocketMode, Sock]),
	Pid = spawn(fun() ->
		receive
			set ->
				?LOG_DEBUG("activated controlling process ~p", [self()]),
				% get peer address and port
				{PeerAddr, PeerPort} = peername(Sock, SocketMode),
				% get peer certificate, if any
				PeerCert = peercert(Sock, SocketMode),
				% build connection record
				C = #c{sock = Sock, socket_mode = SocketMode, port = ListenPort, loop = Loop, recv_timeout = RecvTimeout, stream_support = StreamSupport, ws_loop = WsLoop},
				% jump to state 'request'
				?LOG_DEBUG("jump to state request", []),
				request(C, #req{socket = Sock, socket_mode = SocketMode, peer_addr = PeerAddr, peer_port = PeerPort, peer_cert = PeerCert})
		after 60000 ->
			?LOG_ERROR("timeout waiting for set in controlling process, closing socket", []),
			catch close(Sock, SocketMode)
		end
	end),
	% set controlling process
	case controlling_process(Sock, Pid, SocketMode) of
		ok ->
			Pid ! set;
		{error, _Reason} ->
			?LOG_ERROR("could not set controlling process: ~p, closing socket", [_Reason]),
			catch close(Sock, SocketMode)
	end.

% REQUEST: wait for a HTTP Request line. Transition to state headers if one is received. 
request(#c{sock = Sock, socket_mode = SocketMode, recv_timeout = RecvTimeout} = C, Req) ->
	setopts(Sock, [{active, once}, {packet, http}], SocketMode),
	receive
		{SocketMode, Sock, {http_request, Method, Path, Version}} ->
			?LOG_DEBUG("received full headers of a new HTTP packet", []),
			% change packet type if in ssl mode
			case SocketMode of
				ssl -> setopts(Sock, [{packet, httph}], SocketMode);
				_ -> ok
			end,
			% go to headers
			headers(C, Req#req{vsn = Version, method = Method, uri = Path, connection = default_connection(Version)}, []);
		{SocketMode, Sock, {http_error, "\r\n"}} ->
			request(C, Req);
		{SocketMode, Sock, {http_error, "\n"}} ->
			request(C, Req);
		{http, Sock, {http_error, _Other}}  ->
			?LOG_WARNING("received a http error, might be a ssl request while socket in http mode: ~p, sending forbidden response and closing socket", [_Other]),
			send(Sock, misultin_utility:get_http_status_code(403), SocketMode),
			close(Sock, SocketMode),
			exit(normal);
		_Other ->
			?LOG_WARNING("tcp error on incoming request: ~p, send bad request error back, closing socket", [_Other]),
			close(Sock, SocketMode),
			exit(normal)
	after RecvTimeout ->
		?LOG_DEBUG("normal receive timeout, exit", []),
		close(Sock, SocketMode),
		exit(normal)
	end.

% HEADERS: collect HTTP headers. After the end of header marker transition to body state.
headers(C, Req, H) ->
	headers(C, Req, H, 0).
headers(#c{sock = Sock, socket_mode = SocketMode}, _Req, _H, ?MAX_HEADERS_COUNT) ->
	?LOG_DEBUG("too many headers sent, bad request",[]),
	send(Sock, misultin_utility:get_http_status_code(400), SocketMode);
headers(#c{sock = Sock, socket_mode = SocketMode, recv_timeout = RecvTimeout, ws_loop = WsLoop} = C, Req, H, HeaderCount) ->
	setopts(Sock, [{active, once}], SocketMode),
	receive
		{SocketMode, Sock, {http_header, _, 'Content-Length', _, Val} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req#req{content_length = Val}, [{'Content-Length', Val}|H], HeaderCount + 1);
		{SocketMode, Sock, {http_header, _, 'Connection', _, Val} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req#req{connection = keep_alive(Req#req.vsn, Val)}, [{'Connection', Val}|H], HeaderCount + 1);
		{SocketMode, Sock, {http_header, _, Header, _, Val} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req, [{Header, Val}|H], HeaderCount + 1);
		{SocketMode, Sock, {http_error, "\r\n"} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req, H, HeaderCount);
		{SocketMode, Sock, {http_error, "\n"} = _Head} ->
			?LOG_DEBUG("received header: ~p", [_Head]),
			headers(C, Req, H, HeaderCount);
		{SocketMode, Sock, http_eoh} ->
			?LOG_DEBUG("received EOH header", []),
			Headers = lists:reverse(H),
			{_PathType, Path} = Req#req.uri,
			% check if it's a websocket request
			CheckWs = case WsLoop of
				none -> false;
				_Function -> misultin_websocket:check(Path, Headers)
			end,	
			case CheckWs of
				false ->
					?LOG_DEBUG("normal http request received", []),
					body(C, Req#req{headers = Headers});
				{true, Origin, Host, Path} ->
					?LOG_DEBUG("websocket request received", []),
					misultin_websocket:connect(#ws{socket = Sock, socket_mode = SocketMode, peer_addr = Req#req.peer_addr, peer_port = Req#req.peer_port, origin = Origin, host = Host, path = Path}, WsLoop)
			end;
		{SocketMode, Sock, _Other} ->
			?LOG_DEBUG("tcp error treating headers: ~p, send bad request error back", [_Other]),
			send(Sock, misultin_utility:get_http_status_code(400), SocketMode);
		_Other ->
			?LOG_DEBUG("received unknown message: ~p, ignoring", [_Other]),
			ignored
	after RecvTimeout ->
		?LOG_DEBUG("headers timeout, sending request timeout error", []),
		send(Sock, misultin_utility:get_http_status_code(408), SocketMode)
	end.

% default connection
default_connection({1,1}) -> keep_alive;
default_connection(_) -> close.

% Shall we keep the connection alive? Default case for HTTP/1.1 is yes, default for HTTP/1.0 is no.
keep_alive({1,1}, "close")		-> close;
keep_alive({1,1}, "Close")		-> close;
% string:to_upper is used only as last resort.
keep_alive({1,1}, Head) ->
	case string:to_upper(Head) of
		"CLOSE" -> close;
		_		-> keep_alive
	end;
keep_alive({1,0}, "Keep-Alive") -> keep_alive;
keep_alive({1,0}, Head) ->
	case string:to_upper(Head) of
		"KEEP-ALIVE"	-> keep_alive;
		_				-> close
	end;
keep_alive({0,9}, _)	-> close;
keep_alive(_Vsn, _KA)	-> close.

% BODY: collect the body of the HTTP request if there is one, and lookup and call the implementation callback.
% Depending on whether the request is persistent transition back to state request to await the next request or exit.
body(#c{sock = Sock, socket_mode = SocketMode, recv_timeout = RecvTimeout} = C, Req) ->
	case Req#req.method of
		'GET' ->
			?LOG_DEBUG("GET request received",[]),
			Close = handle_get(C, Req),
			case Close of
				close ->
					% close socket
					close(Sock, SocketMode);
				keep_alive ->
					request(C, #req{socket = Sock, socket_mode = SocketMode, peer_addr = Req#req.peer_addr, peer_port = Req#req.peer_port, peer_cert = Req#req.peer_cert})
			end;
		'POST' ->
			?LOG_DEBUG("POST request received", []),
			case catch list_to_integer(Req#req.content_length) of
				{'EXIT', _} ->
					% TODO: provide a fallback when content length is not or wrongly specified
					?LOG_DEBUG("specified content length is not a valid integer number: ~p", [Req#req.content_length]),
					send(Sock, misultin_utility:get_http_status_code(411), SocketMode),
					exit(normal);
				0 ->
					?LOG_DEBUG("zero content-lenght specified, skipping parsing body of request", []),
					Close = handle_post(C, Req),
					case Close of
						close ->
							% close socket
							close(Sock, SocketMode);
						keep_alive ->
							setopts(Sock, [{packet, http}], SocketMode),
							request(C, #req{socket = Sock, socket_mode = SocketMode, peer_addr = Req#req.peer_addr, peer_port = Req#req.peer_port, peer_cert = Req#req.peer_cert})
					end;					
				Len ->
					?LOG_DEBUG("parsing POST content in body of request", []),
					setopts(Sock, [{packet, raw}, {active, false}], SocketMode),
					case recv(Sock, Len, RecvTimeout, SocketMode) of
						{ok, Bin} ->
							Close = handle_post(C, Req#req{body = Bin}),
							case Close of
								close ->
									% close socket
									close(Sock, SocketMode);
								keep_alive ->
									setopts(Sock, [{packet, http}], SocketMode),
									request(C, #req{socket = Sock, socket_mode = SocketMode, peer_addr = Req#req.peer_addr, peer_port = Req#req.peer_port, peer_cert = Req#req.peer_cert})
							end;
						{error, timeout} ->
							?LOG_DEBUG("request timeout, sending error", []),
							send(Sock, misultin_utility:get_http_status_code(408), SocketMode); 
						_Other ->
							?LOG_DEBUG("tcp error treating post data: ~p, send bad request error back", [_Other]),
							send(Sock, misultin_utility:get_http_status_code(400), SocketMode)
					end
			end;
		_Other ->
			?LOG_DEBUG("method not implemented: ~p", [_Other]),
			send(Sock, misultin_utility:get_http_status_code(501), SocketMode),
			exit(normal)
	end.

% handle a get request
handle_get(C, #req{socket_mode = SocketMode, connection = Conn} = Req) ->
	case Req#req.uri of
		{abs_path, Path} ->
			{F, Args} = split_at_q_mark(Path, []),
			call_mfa(C, Req#req{args = Args, uri = {abs_path, F}}),
			Conn;
		{absoluteURI, http, _Host, _, Path} ->
			{F, Args} = split_at_q_mark(Path, []),
			call_mfa(C, Req#req{args = Args, uri = {absoluteURI, F}}),
			Conn;
		{absoluteURI, _Other_method, _Host, _, _Path} ->
			send(C#c.sock, misultin_utility:get_http_status_code(501), SocketMode),
			close;
		{scheme, _Scheme, _RequestString} ->
			send(C#c.sock, misultin_utility:get_http_status_code(510), SocketMode),
			close;
		_  ->
			send(C#c.sock, misultin_utility:get_http_status_code(403), SocketMode),
			close
	end.

% handle a post request
handle_post(C, #req{socket_mode = SocketMode, connection = Conn} = Req) ->
	case Req#req.uri of
		{abs_path, _Path} ->
			call_mfa(C, Req),
			Conn;
		{absoluteURI, http, _Host, _, _Path} ->
			call_mfa(C, Req),
			Conn;
		{absoluteURI, _Other_method, _Host, _, _Path} ->
			send(C#c.sock, misultin_utility:get_http_status_code(501), SocketMode),
			close;
		{scheme, _Scheme, _RequestString} ->
			send(C#c.sock, misultin_utility:get_http_status_code(501), SocketMode),
			close;
		_  ->
			send(C#c.sock, misultin_utility:get_http_status_code(403), SocketMode),
			close
	end.

% Description: Main dispatcher
call_mfa(#c{sock = Sock, socket_mode = SocketMode, loop = Loop, stream_support = StreamSupport} = C, Request) ->
	% spawn listening process for Request messages [only used to support stream requests]
	case StreamSupport of
		true ->
			SocketPid = spawn(?MODULE, socket_loop, [C, Request]);
		false ->
			SocketPid = no_stream_support_proc
	end,
	% create request
	Req = misultin_req:new(Request, SocketPid),
	% call loop
	case catch Loop(Req) of
		{'EXIT', _Reason} ->
			?LOG_ERROR("worker crash: ~p", [_Reason]),
			% kill listening socket
			catch SocketPid ! shutdown,
			% send response
			send(Sock, misultin_utility:get_http_status_code(500), SocketMode),
			% force exit
			exit(normal);
		{HttpCode, Headers0, Body} ->
			% received normal response
			?LOG_DEBUG("sending response", []),
			% kill listening socket
			catch SocketPid ! shutdown,
			% flatten body [optimization since needed for content length]
			BodyBinary = convert_to_binary(Body),
			% provide response
			Headers1 = add_output_header('Content-Length', {Headers0, BodyBinary}),
			Headers = add_output_header('Connection', {Headers1, Request}),
			Enc_headers = enc_headers(Headers),
			Resp = [misultin_utility:get_http_status_code(HttpCode), Enc_headers, <<"\r\n">>, BodyBinary],
			send(Sock, Resp, SocketMode);
		{raw, Body} ->
			send(Sock, Body, SocketMode);
		_ ->
			% loop exited normally, kill listening socket
			catch SocketPid ! shutdown
	end.

% Description: Ensure Body is binary.
convert_to_binary(Body) when is_list(Body) ->
	list_to_binary(lists:flatten(Body));
convert_to_binary(Body) when is_binary(Body) ->
	Body;
convert_to_binary(Body) when is_atom(Body) ->
	list_to_binary(atom_to_list(Body)).

% Description: Socket loop for stream responses
socket_loop(#c{sock = Sock, socket_mode = SocketMode} = C, Request) ->
	receive
		{stream_head, HttpCode, Headers0} ->
			?LOG_DEBUG("sending stream head", []),
			Headers = add_output_header('Connection', {Headers0, Request}),
			Enc_headers = enc_headers(Headers),
			Resp = [misultin_utility:get_http_status_code(HttpCode), Enc_headers, <<"\r\n">>],
			send(Sock, Resp, SocketMode),
			socket_loop(C, Request);
		{stream_data, Body} ->
			?LOG_DEBUG("sending stream data", []),
			send(Sock, Body, SocketMode),
			socket_loop(C, Request);
		stream_close ->
			?LOG_DEBUG("closing stream", []),
			close(Sock, SocketMode);
		shutdown ->
			?LOG_DEBUG("shutting down socket loop", []),
			shutdown
	end.

% Description: Add necessary Content-Length Header
add_output_header('Content-Length', {Headers, Body}) ->
	case proplists:get_value('Content-Length', Headers) of
		undefined ->
			[{'Content-Length', size(Body)}|Headers];
		_ExistingContentLength ->
			Headers
	end;

% Description: Add necessary Connection Header
add_output_header('Connection', {Headers, Req}) ->
	case Req#req.connection of
		undefined ->
			% nothing to echo
			Headers;
		Connection ->
			% echo
			case proplists:get_value('Connection', Headers) of
				undefined ->
					[{'Connection', connection_str(Connection)}|Headers];
				_ExistingConnectionHeaderValue ->
					Headers
			end
	end.

% Helper to Connection string
connection_str(keep_alive) -> "Keep-Alive";
connection_str(close) -> "Close".

% Description: Encode headers
enc_headers([{Tag, Val}|T]) when is_atom(Tag) ->
	[atom_to_list(Tag), ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([{Tag, Val}|T]) when is_list(Tag) ->
	[Tag, ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([]) ->
	[].
enc_header_val(Val) when is_atom(Val) ->
	atom_to_list(Val);
enc_header_val(Val) when is_integer(Val) ->
	integer_to_list(Val);
enc_header_val(Val) ->
	Val.

% Split the path at the ?
split_at_q_mark([$?|T], Acc) ->
	{lists:reverse(Acc), T};
split_at_q_mark([H|T], Acc) ->
	split_at_q_mark(T, [H|Acc]);
split_at_q_mark([], Acc) ->
	{lists:reverse(Acc), []}.

% socket listen
listen(Port, Options, http) -> gen_tcp:listen(Port, Options);
listen(Port, Options, ssl) -> ssl:listen(Port, Options).

% socket accept
accept(ListenSocket, http) -> gen_tcp:accept(ListenSocket);
accept(ListenSocket, ssl) ->
	try ssl:transport_accept(ListenSocket)
	catch
		error:{badmatch, {error, Reason}} ->
			{error, Reason}
	end.					

% socket controlling process
controlling_process(Sock, Pid, http) -> gen_tcp:controlling_process(Sock, Pid);
controlling_process(Sock, Pid, ssl) -> ssl:controlling_process(Sock, Pid).

% Function: -> {PeerAddr, PeerPort} | PeerAddr = list() | undefined | PeerPort = integer() | undefined
% Description: Get socket peername
peername(Sock, http) -> peername(Sock, fun inet:peername/1);
peername(Sock, ssl) -> peername(Sock, fun ssl:peername/1);
peername(Sock, F) ->
	case F(Sock) of
		{ok, {Addr, Port}} ->
			{Addr, Port};
		{error, _Reason} ->
			{undefined, undefined}
	end.

% Function: -> Certificate | undefined
% Description: Get socket certificate
peercert(_Sock, http) -> undefined;
peercert(Sock, ssl) ->
	case ssl:peercert(Sock) of
		{ok, Cert} -> Cert;
		{error, _Reason} -> undefined
	end.

% socket set options
setopts(Sock, Options, http) -> inet:setopts(Sock, Options);
setopts(Sock, Options, ssl) -> ssl:setopts(Sock, Options).

% socket receive
recv(Sock, Len, RecvTimeout, http) -> gen_tcp:recv(Sock, Len, RecvTimeout);
recv(Sock, Len, RecvTimeout, ssl) -> ssl:recv(Sock, Len, RecvTimeout).

% socket send
send(Sock, Data, http) -> send(Sock, Data, fun gen_tcp:send/2);
send(Sock, Data, ssl) -> send(Sock, Data, fun ssl:send/2);
send(Sock, Data, F) -> 
	?LOG_DEBUG("sending data: ~p", [Data]),
	case F(Sock, Data) of
		ok ->
			ok;
		{error, _Reason} ->
			?LOG_ERROR("worker crash: ~p", [_Reason]),
			exit(normal)
	end.

% TCP close
close(Sock, http) -> close(Sock, fun gen_tcp:close/1);
close(Sock, ssl) -> close(Sock, fun ssl:close/1);
close(Sock, F) ->
	?LOG_DEBUG("closing socket", []),
	case catch F(Sock) of
		ok ->
			ok;
		_Else ->
			?LOG_WARNING("could not close socket: ~p", [_Else]),
			exit(normal)
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
