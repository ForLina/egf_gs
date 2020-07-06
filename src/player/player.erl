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

-module(player).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([
    get_uid_by_aid/1,
    init_player/1,
    handle_msg/2,
    get_pid/1,
    create_process/1,
    update_cpid/2,
    kick/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(player_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_uid_by_aid(iodata()) -> integer() | undefined.
get_uid_by_aid(Aid) ->
    player_db:get_uid_by_aid(Aid).

-spec init_player(iodata()) -> Uid :: integer().
init_player(Aid) ->
    player_db:init_player(Aid).

-spec create_process(integer()) -> {ok, pid()} | {error, term()}.
create_process(Uid) ->
    player_sup:start_child(Uid).

-spec update_cpid(pid(), pid()) -> ok.
update_cpid(PlayerPid, CPid) ->
    gen_server:cast(PlayerPid, {update_cpid, CPid}).

-spec handle_msg(pid(), binary()) -> ok.
handle_msg(PlayerPid, Msg) ->
    gen_server:cast(PlayerPid, {handle_msg, Msg}).

-spec get_pid(integer()) -> pid() | undefined.
get_pid(Uid) ->
    player_db:get_pid(Uid).

-spec kick(pid()) -> ok.
kick(PlayerPid) ->
    gen_server:call(PlayerPid, kick).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #player_state{}} | {ok, State :: #player_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #player_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #player_state{}) ->
                     {reply, Reply :: term(), NewState :: #player_state{}} |
                     {reply, Reply :: term(), NewState :: #player_state{}, timeout() | hibernate} |
                     {noreply, NewState :: #player_state{}} |
                     {noreply, NewState :: #player_state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #player_state{}} |
                     {stop, Reason :: term(), NewState :: #player_state{}}).
handle_call(_Request, _From, State = #player_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #player_state{}) ->
    {noreply, NewState :: #player_state{}} |
    {noreply, NewState :: #player_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #player_state{}}).
handle_cast(_Request, State = #player_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #player_state{}) ->
    {noreply, NewState :: #player_state{}} |
    {noreply, NewState :: #player_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #player_state{}}).
handle_info(_Info, State = #player_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #player_state{}) -> term()).
terminate(_Reason, _State = #player_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #player_state{},
                  Extra :: term()) ->
                     {ok, NewState :: #player_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #player_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
