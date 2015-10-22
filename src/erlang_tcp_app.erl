-module(erlang_tcp_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    {ok, _} = application:ensure_all_started(erlang_tcp).

start(_StartType, _StartArgs) ->
    {ok, _} = ranch:start_listener(tcp_chain, 10, ranch_tcp, [{port, 5555}], chain_protocol, []),

    erlang_tcp_sup:start_link().

stop(_State) ->
    ok.
