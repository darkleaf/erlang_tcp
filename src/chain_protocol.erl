-module(chain_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API.
-export([start_link/4]).

%% gen_server.
-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {socket, transport, chain}).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
	proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% gen_server.

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = proc_lib:init_ack({ok, self()}),
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),
	gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport, chain=chain:new()}).

handle_info({tcp, Socket, Data}, State=#state{socket=Socket, transport=Transport, chain=Chain}) ->
	Transport:setopts(Socket, [{active, once}]),

	NewNumber = parse_number(Data),
	CanUpdateChain = chain:can_add(Chain, NewNumber),
	NewChain = if
		CanUpdateChain ->
			chain:add(Chain, NewNumber);
		true ->
			Transport:send(Socket, [chain:to_binary(Chain), "\r\n"]),
			Chain
	end,

	NewState = State#state{chain=NewChain},
	{noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{stop, normal, State}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

parse_number(Input) ->
	BinNumber = binary:part(Input, {0, byte_size(Input)-2}),
	binary_to_integer(BinNumber).
