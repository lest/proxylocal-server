-module(client).

-behaviour(gen_server).

-export([connection/1, stream/2, close/1]).
-export([host_binded/1, temporary_host/1, allowed_bert_atoms/0]).
-export([start/1, loop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-record(state, {socket, options, hosts = [], connections}).

-define(KEEPALIVE_INTERVAL, 30000).
-define(TEMPORARY_HOSTS_LIMIT, 8).

connection(Host) when is_list(Host) ->
    connection(list_to_binary(Host));
connection(Host) ->
    case find_client(Host) of
        {ok, {_, ClientRef}} ->
            gen_server:call(ClientRef, {connection, self()});
        {error, Reason} ->
            {error, Reason}
    end.

stream({ClientRef, _Id} = Connection, Bin) ->
    gen_server:cast(ClientRef, {stream, Connection, Bin}).

close({ClientRef, _Id} = Connection) ->
    gen_server:cast(ClientRef, {close, Connection}).

start(Options) ->
    ets:new(binded_hosts, [set, public, named_table]),
    socket_server:start(?MODULE, Options, {?MODULE, loop}).

loop(Socket) ->
    {ok, State} = init([Socket]),
    gen_server:enter_loop(?MODULE, [], State).

init([Socket]) ->
    inet:setopts(Socket, [{packet, 4}, {active, once}]),
    timer:send_interval(?KEEPALIVE_INTERVAL, keepalive),
    {ok, #state{socket = Socket, connections = dict:new()}}.

handle_call({connection, Req}, _From, State) ->
    Id = uuid:v4(),
    Connections = dict:store(Id, Req, State#state.connections),
    send_bert(State#state.socket, {connection, Id}),
    Reply = {ok, {self(), Id}},
    {reply, Reply, State#state{connections = Connections}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({stream, {_, Id}, Bin}, State) ->
    send_bert(State#state.socket, {stream, Id, Bin}),
    {noreply, State};

handle_cast({close, {_, Id}}, State) ->
    send_bert(State#state.socket, {close, Id}),
    {noreply, State};

handle_cast(bind_random_host, State) ->
    Host = generate_random_host(),
    case ets:member(binded_hosts, Host) of
        true ->
            timer:apply_after(5000, gen_server, cast, [self(), bind_random_host]),
            send_bert(State#state.socket, {message, <<"Waiting for available host...">>}),
            {noreply, State};
        false ->
            bind_hosts([Host], State)
    end;

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({tcp, _Socket, _Data} = Info, State) ->
    handle_socket_data(Info, State);
handle_info({ssl, _Socket, _Data} = Info, State) ->
    handle_socket_data(Info, State);

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({ssl_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({bert, Obj}, State) ->
    handle_bert(Obj, State);

handle_info(keepalive, State) ->
    send_bert(State#state.socket, keepalive),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    error_logger:info_report(["Disconnected", {hosts, State#state.hosts}]),
    lists:foreach(fun(Host) -> ets:delete(binded_hosts, Host) end, State#state.hosts),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

allowed_bert_atoms() ->
    [bert, options, verbose, tls, local_port, token, server_host, server_port,
     version, daemonize, true, false].

handle_socket_data({Type, Socket, Data}, State) ->
    Result = handle_bert(bert:decode_term(binary_to_term(Data, [safe])), State),
    M = case Type of
            tcp ->
                inet;
            ssl ->
                ssl
        end,
    M:setopts(Socket, [{active, once}]),
    Result.

handle_bert(start_tls, State) ->
    send_bert(State#state.socket, start_tls),
    ssl:start(),
    {ok, SSLSocket} = ssl:ssl_accept(State#state.socket, [{cacertfile, "priv/cacerts.pem"},
                                                          {certfile, "priv/cert.pem"},
                                                          {keyfile, "priv/key.pem"}]),
    ssl:setopts(SSLSocket, [{active, once}]),
    {noreply, State#state{socket = SSLSocket}};

handle_bert({options, Options}, State) ->
    error_logger:info_report(["Connected", {options, dict:to_list(Options)}]),
    NewState = State#state{options = Options},
    case dict:find(hosts, Options) of
        {ok, RequestedHosts} ->
            case allowed_hosts(RequestedHosts, Options) of
                [] ->
                    send_bert(State#state.socket, {message, <<"Requested hosts aren't allowed">>}),
                    send_bert(State#state.socket, halt),
                    {stop, normal, State};
                Hosts ->
                    bind_hosts(Hosts, NewState)
            end;
        _ ->
            gen_server:cast(self(), bind_random_host),
            {noreply, NewState}
    end;

handle_bert({stream, Id, Bin}, State) ->
    Req = dict:fetch(Id, State#state.connections),
    proxy:stream(Req, Bin),
    {noreply, State};

handle_bert({close, Id}, #state{connections = Connections} = State) ->
    Req = dict:fetch(Id, Connections),
    proxy:close(Req),
    {noreply, State#state{connections = dict:erase(Id, Connections)}};

handle_bert(Obj, State) ->
    error_logger:info_report(["Unrecognized object", {obj, Obj}]),
    {stop, normal, State}.

send_bert(Socket, Obj) ->
    M = case Socket of
            Socket when is_port(Socket) ->
                gen_tcp;
            {sslsocket, _, _} ->
                ssl
        end,
    M:send(Socket, bert:encode(Obj)).

splithost(Host) when is_list(Host) ->
    splithost(list_to_binary(Host));
splithost(Host) ->
    splithost(Host, 0).

splithost(Host, Idx) ->
    case Host of
        <<This:Idx/binary, $., Tail/binary>> ->
            {This, Tail};
        <<_This:Idx/binary, _, _Tail/binary>> ->
            splithost(Host, Idx + 1);
        <<_This:Idx/binary>> ->
            {Host, <<>>}
    end.

find_client(<<>>) ->
    {error, not_found};
find_client(Host) ->
    case ets:lookup(binded_hosts, Host) of
        [{Host, ClientRef}] ->
            {ok, {Host, ClientRef}};
        _ ->
            {_, NextHost} = splithost(Host),
            find_client(NextHost)
    end.

bind_hosts(Hosts, #state{options = Options} = State) ->
    error_logger:info_report(["Bind hosts", {hosts, Hosts}]),
    lists:foreach(fun(Host) -> ets:insert(binded_hosts, {Host, self()}) end, Hosts),
    LocalPort = dict:fetch(local_port, Options),
    send_bert(State#state.socket, {message, list_to_binary(["Local server on port ",
                                                            LocalPort,
                                                            " is now publicly available via:",
                                                            lists:map(fun(Host) -> ["\nhttp://", Host, "/"] end, Hosts)])}),
    {noreply, State#state{hosts = Hosts}}.

temporary_host(Host) ->
    list_to_binary([Host, ".", proxylocal:get_var(domain)]).

generate_random_host() ->
    random:seed(now()),
    Length = 4,
    CharList = lists:flatten([lists:seq($0, $9), lists:seq($a, $z)]),
    CharCount = length(CharList),
    Name = lists:map(fun(_) -> lists:nth(random:uniform(CharCount), CharList) end, lists:seq(1, Length)),
    temporary_host(Name).

host_binded(Host) ->
    case ets:lookup(binded_hosts, Host) of
        [{Host, _ClientRef}] ->
            false;
        _ ->
            true
    end.

unbinded_temporary_hosts(Hosts) ->
    lists:filter(fun ?MODULE:host_binded/1,
                 lists:map(fun ?MODULE:temporary_host/1,
                           lists:sublist(Hosts, ?TEMPORARY_HOSTS_LIMIT))).

allowed_hosts(Hosts, _Options) ->
    unbinded_temporary_hosts(Hosts).
