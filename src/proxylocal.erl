-module(proxylocal).

-export([start/0, reload/0, status/0, ctl/0, get_var/1, get_var/2]).

start() ->
    application:start(proxylocal),
    ok.

reload() ->
    application:load(proxylocal),
    {ok, Modules} = application:get_key(proxylocal, modules),
    lists:foreach(fun(Module) ->
                          code:soft_purge(Module),
                          code:load_file(Module)
                  end,
                  lists:subtract(Modules, [socket_server, uuid])),
    ok.

status() ->
    {HostCount, Clients} = ets:foldl(fun({_Host, ClientRef}, {HostCount, Clients}) ->
                                             NewClients = case lists:member(ClientRef, Clients) of
                                                              true -> Clients;
                                                              _    -> [ClientRef|Clients]
                                                          end,
                                             {HostCount + 1, NewClients}
                                     end, {0, []}, binded_hosts),
    io:format("hosts ~.10b~nclients ~.10b~n", [HostCount, length(Clients)]).

ctl() ->
    [NodeStr, Command | Args] = init:get_plain_arguments(),
    Node = list_to_atom(NodeStr),
    handle(Node, list_to_atom(Command), Args),
    halt(0).

get_var(Opt) ->
    get_var(Opt, undefined).

get_var(Opt, Default) ->
    case application:get_env(proxylocal, Opt) of
        {ok, Val} -> Val;
        _ ->
            case init:get_argument(Opt) of
                {ok, [[Val|_]|_]} -> Val;
                error             -> Default
            end
    end.

handle(Node, stop, _Args) ->
    rpc:call(Node, init, stop, []);
handle(Node, reload, _Args) ->
    rpc:call(Node, proxylocal, reload, []);
handle(Node, status, _Args) ->
    rpc:call(Node, proxylocal, status, []).
