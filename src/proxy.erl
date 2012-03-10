-module(proxy).

-behaviour(gen_server).

-export([stream/2, close/1]).
-export([start/1, loop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-record(state, {socket, packet, buffer, request, connection, id, empty_response = true}).

stream(Ref, Bin) ->
    gen_server:cast(Ref, {stream, Bin}).

close(Ref) ->
    gen_server:cast(Ref, close).

start(Options) ->
    socket_server:start(?MODULE, Options, {?MODULE, loop}).

loop(Socket) ->
    {ok, State} = init([Socket]),
    gen_server:enter_loop(?MODULE, [], State).

init([Socket]) ->
    inet:setopts(Socket, [{packet, raw}, {active, once}]),
    {ok, #state{socket = Socket, packet = http_bin, buffer = <<>>, request = <<>>}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({stream, Bin}, State) ->
    gen_tcp:send(State#state.socket, Bin),
    {noreply, State#state{empty_response = false}};

handle_cast(close, #state{socket = Socket} = State) ->
    case State#state.empty_response of
        true ->
            gen_tcp:send(Socket, <<"HTTP/1.0 200 OK", "\r\n", "\r\n", "error: empty_response">>);
        _ ->
            ok
    end,
    gen_tcp:close(Socket),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Bin}, State) ->
    inet:setopts(Socket, [{active, once}]),
    case State#state.packet of
        raw ->
            case State#state.connection of
                {ok, Connection} ->
                    client:stream(Connection, Bin),
                    {noreply, State};
                {error, _Reason} ->
                    gen_tcp:close(Socket),
                    {stop, normal, State}
            end;
        _Type ->
            handle_packet(Bin, State)
    end;

handle_info({tcp_closed, _Socket}, State) ->
    case State#state.connection of
        {ok, Connection} ->
            client:close(Connection);
        _ ->
            ok
    end,
    {stop, normal, State};

handle_info(Info, State) ->
    error_logger:info_report([{info, Info}]),
    gen_tcp:close(State#state.socket),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_packet(Bin, #state{socket = Socket, packet = Type, buffer = Buffer, request = Request} = State) ->
    NewBuffer = <<Buffer/binary, Bin/binary>>,
    case erlang:decode_packet(Type, NewBuffer, []) of
        {ok, {http_request, _Method, _Uri, _Vsn}, Rest} ->
            handle_packet(<<>>, State#state{request = Bin, packet = httph_bin, buffer = Rest});
        {ok, {http_header, _, 'Host', _, Host}, Rest} ->
            Connection = client:connection(Host),
            handle_packet(<<>>, State#state{request = <<Request/binary, Bin/binary>>, buffer = Rest, connection = Connection});
        {ok, {http_header, _, _Name, _, _Value}, Rest} ->
            handle_packet(<<>>, State#state{request = <<Request/binary, Bin/binary>>, buffer = Rest});
        {ok, http_eoh, _Rest} ->
            case State#state.connection of
                {ok, Connection} ->
                    client:stream(Connection, <<Request/binary, Bin/binary>>),
                    {noreply, State#state{request = <<>>, packet = raw, buffer = <<>>}};
                {error, Reason} ->
                    Error = list_to_binary(io_lib:format("error: ~s~n", [Reason])),
                    gen_tcp:send(Socket, <<"HTTP/1.0 200 OK", "\r\n", "\r\n", Error/binary>>),
                    gen_tcp:close(Socket),
                    {stop, normal, State};
                undefined ->
                    %% 400
                    gen_tcp:send(Socket, <<"HTTP/1.0 200 OK", "\r\n", "\r\n", "error: undefined">>),
                    gen_tcp:close(Socket),
                    {stop, normal, State}
            end;
        {ok, {http_error, _}, _Rest} ->
            %% 400
            gen_tcp:send(Socket, <<"HTTP/1.0 200 OK", "\r\n", "\r\n", "error: http_error">>),
            gen_tcp:close(Socket),
            {stop, normal, State};
        {more, _Length} ->
            {noreply, State#state{request = <<Request/binary, Bin/binary>>, buffer = NewBuffer}};
        {error, _Reason} ->
            {stop, normal, State}
    end.
