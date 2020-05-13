%% MIT License
%%
%% Copyright (c) 2020 Jack Liu
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(egf_client).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(FIRST_PACKET_TIMEOUT, 2000).

-record(state, {
    transport :: atom(),
    socket :: reference(),
    login_suc = false :: boolean(),
    player_pid :: pid(),
    req_seq :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(atom(), reference()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Transport, Sock) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Transport, Sock]])}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec init(Args :: list()) -> no_return() | {error, Reason :: term()}.
init([Transport, Sock]) ->
    case on_connect(Transport, Sock) of
        true ->
            init2(Transport, Sock);
        false ->
            {error, deny};
        Error ->
            Error
    end.

init2(Transport, Sock) ->
    case Transport:wait(Sock) of
        {ok, NewSock} ->
            Transport:async_recv(Sock, 0, ?FIRST_PACKET_TIMEOUT),
            State = #state{transport = Transport, socket = NewSock},
            gen_server:enter_loop(?MODULE, [], State);
        Error -> Error
    end.

%% @doc Hook the on connect event, Usually request the risk control system to
%%      determine whether to allow this IP login.
-spec on_connect(atom(), reference()) -> boolean() |  {error, inet:posix()}.
on_connect(Transport, Sock) ->
    case Transport:peername(Sock) of
        {ok, {IP, _Port}} ->
            AllowLogin = egf_hook:on_connect(IP),
            AllowLogin;
        Error ->
            Error
    end.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_info({inet_async, Socket, {error, Reason}},
            #state{socket = Socket} = State) ->
    {stop, {error, Reason}, State};

%% Not login yet
handle_info({inet_async, Socket, Seq, {ok, Data}},
            State = #state{transport = Transport,
                           socket = Socket,
                           login_suc = false}) ->
    ConnectProcess = self(),
    case login_port:handle_first_packet(Data, ConnectProcess) of
        {ok, Reply, PlayerPid} ->
            Transport:async_send(Socket, Reply),
            Transport:async_recv(Socket, 0, infinity),
            {noreply, State#state{login_suc = true,
                                  player_pid = PlayerPid,
                                  req_seq = Seq}};
        {error, Reply, Reason} ->
            Transport:async_send(Socket, Reply),
            {stop, {error, Reason}, State}
    end;

handle_info({inet_async, Socket, Seq, {ok, Data}},
            State = #state{transport = Transport,
                           socket = Socket,
                           player_pid = PlayerPid,
                           login_suc = true}) ->
    
    case erlang:is_process_alive(PlayerPid) of
        true ->
            player:handle_msg(PlayerPid, Data),
            Transport:async_recv(Socket, 0, infinity),
            {noreply, State#state{req_seq = Seq}};
        {error, Reason} ->
            {stop, {error, Reason}, State}
    end;

handle_info(_Other, State) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
