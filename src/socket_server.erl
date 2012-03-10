-module(socket_server).

-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {port, loop, ip, lsocket = null}).

start(Name, Options, Loop) ->
    Port = proplists:get_value(port, Options),
    Ip = proplists:get_value(ip, Options, any),
    State = #server_state{port = Port, ip = Ip, loop = Loop},
    gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port = Port, ip = Ip}) ->
    process_flag(trap_exit, true),
    Options = [{ip, Ip}|?TCP_OPTIONS],
    case gen_tcp:listen(Port, Options) of
        {ok, LSocket} ->
            NewState = State#server_state{lsocket = LSocket},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State = #server_state{}) ->
    {noreply, accept(State)}.

accept_loop({Server, LSocket, {M, F}}) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            gen_server:cast(Server, {accepted, self()}),
            M:F(Socket);
        {error, closed} ->
            ok
    end.

accept(State = #server_state{lsocket = LSocket, loop = Loop}) ->
    proc_lib:spawn_link(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
    State.

handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
