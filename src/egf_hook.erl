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

-module(egf_hook).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_on_connect_handler/2]).

-export([on_connect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-define(EGF_HOOK, egf_hook).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Add event callback function
-spec add_event(atom(), atom(), atom()) -> ok.
add_event(EventName, Mod, Func) ->
    gen_server:cast(?MODULE, {add_event, EventName, Mod, Func}).

%% @doc Add event callback function
-spec add_on_connect_handler(atom(), atom()) -> ok.
add_on_connect_handler(Mod, Fun) ->
    add_event(on_connect, Mod, Fun).

%% @doc Handle on connect event
-spec on_connect(inet:ip_address()) -> boolean().
on_connect(Ip) ->
    case ets:lookup(?EGF_HOOK, on_connect) of
        [{on_connect, List}] ->
            on_connect(List, Ip);
        [] ->
            true
    end.

on_connect([{M, F} | T], IP) ->
    case M:F(IP) of
        true ->
            on_connect(T, IP);
        false ->
            false
    end;
on_connect([], _IP) ->
    true.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
%%    ?EGF_HOOK = ets:new(?EGF_HOOK, [named_table, protected]),
    {ok, #state{}}.

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
handle_cast({add_event, EventName, Mod, Func}, State = #state{}) ->
    case ets:lookup(?EGF_HOOK, EventName) of
        [{EventName, MFs}] ->
            NewMFs = insert_update(MFs, {Mod, Func}),
            ets:insert(?EGF_HOOK, {EventName, NewMFs});
        [] ->
            ets:insert(?EGF_HOOK, {EventName, [{Mod, Func}]})
    end,
    {noreply, State};
handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State = #state{}) ->
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

insert_update(MFs, {Mod, Func}) ->
    case lists:keyfind(Mod, 1, MFs) of
        {Mod, _OldFunc} ->
            lists:keyreplace(Mod, 1, MFs, {Mod, Func});
        false ->
            [{Mod, Func} | MFs]
    end.