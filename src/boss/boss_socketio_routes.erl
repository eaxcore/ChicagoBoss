-module(boss_socketio_routes).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_info/2,
         handle_call/3, handle_cast/2, terminate/2, code_change/3]).
-export([register_route/2, get_route/1]).

-record(socketio_routes, {routes}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, null, []).

init(_Args) ->
    State = #socketio_routes{routes=ets:new(socketio_routes, [set, named_table])},
    {ok, State}.

request(Msg) ->
    gen_server:call(?MODULE, Msg).

register_route(ReqPath, Module) ->
    request({register_route, ReqPath, Module}).

get_route(ReqPath) ->
    request({get_route, ReqPath}).

handle_call({register_route, ReqPath, Module}, _From, State) ->
    Routes = State#socketio_routes.routes,
    true = ets:insert(Routes, {ReqPath, Module}),
    {reply, ok, State};

handle_call({get_route, ReqPath}, _From, State) ->
    Routes = State#socketio_routes.routes,
    R = ets:lookup(Routes, ReqPath),
    case R of
        [{ReqPath, Module}] -> {reply, {ok, Module}, State};
        _ -> {reply, notfound, State}
    end;

handle_call(UnknownCall, _From, State) ->
    {reply, {error, invalid_call, UnknownCall}, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

