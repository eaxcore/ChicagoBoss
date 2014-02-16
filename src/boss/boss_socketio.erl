-module(boss_socketio).

-export([start/0]).
-export([dispatch/0, open/3, recv/4, handle_info/4, close/3]).

%-record(session_state, {}).


start() ->
    application:start(sasl),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(ranch),
    application:start(cowboy),
    ok = application:start(socketio),
    ok.

dispatch() ->
              [
               {"/socket.io/1/[...]", socketio_handler,
                [socketio_session:configure([
                 {heartbeat, 5000},
                 {heartbeat_timeout, 30000},
                 {session_timeout, 30000},
                 {callback, ?MODULE},
                 {protocol, socketio_data_protocol}])]}
              ].

-define(REQPATH, null).

%% ---- Handlers
open(Pid, Sid, Opts) ->
    case boss_socketio_routes:get_route(?REQPATH) of
        {ok, Module} -> Module:open(Pid, Sid, Opts);
        _ -> disconnect
    end.
%    erlang:send_after(5000, self(), tick),
%    lager:info("open ~p ~p~n", [Pid, Sid]),
%    demo_mgr:add_session(Pid),
%    {ok, #session_state{}}.

%recv(Pid, _Sid, {json, <<>>, Json}, SessionState = #session_state{}) ->
%    lager:info("recv json ~p~n", [Json]),
%    demo_mgr:publish_to_all(Json),
%    {ok, SessionState};

%recv(Pid, _Sid, {message, <<>>, Message}, SessionState = #session_state{}) ->
%    socketio_session:send_message(Pid, Message),
%    {ok, SessionState};

%recv(Pid, Sid, Message, SessionState = #session_state{}) ->
%    lager:info("recv ~p ~p ~p~n", [Pid, Sid, Message]),
%    {ok, SessionState}.

recv(Pid, Sid, Message, SessionState) ->
    case boss_socketio_routes:get_route(?REQPATH) of
        {ok, Module} -> Module:recv(Pid, Sid, Message, SessionState);
        _ -> {disconnect, SessionState}
    end.

handle_info(_Pid, _Sid, tick, SessionState) ->
    lager:info("Tick...", []),
    {ok, SessionState};

handle_info(_Pid, _Sid, _Info, SessionState) ->
    {ok, SessionState}.

close(Pid, Sid, SessionState) ->
    case boss_socketio_routes:get_route(?REQPATH) of
        {ok, Module} -> Module:close(Pid, Sid, SessionState);
        _ -> ok
    end.
%    lager:info("close ~p ~p~n", [Pid, Sid]),
%    demo_mgr:remove_session(Pid),
%    ok.

