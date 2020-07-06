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

-module(egf_gs_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("logger.hrl").

start(_StartType, _StartArgs) ->
    init_logger(),
    ?LOG_INFO("Server starting at ~p~n", [calendar:now_to_datetime(os:timestamp())]),
    
    load_config(),
    
    ok = start_db(),
    {ok, Pid} = egf_gs_sup:start_link(),
    start_gateway(),
    
    ?LOG_INFO("Server started at ~p~n", [calendar:now_to_datetime(os:timestamp())]),
    {ok, Pid}.

stop(_State) ->
    ok.

start_gateway() ->
    Path = filename:absname("./config/boot/tcp_config.cfg"),
    ok = egf_config:load_file(Path).

%% the default log level for application is 5 which means notice
init_logger() ->
    logger:remove_handler(default),
    logger:set_application_level(egf_gs, debug).

%% internal functions

load_config() ->
    DevDir = filename:absname("./config/dev"),
    ok = egf_config:add_dir(DevDir).

%% @doc Start database we need, I suggest using mnesia only.
-spec start_db() -> ok.
start_db() ->
    DbDir = filename:absname("./db"),
    filelib:ensure_dir(DbDir),
    application:set_env(mnesia, dir, DbDir),
    mnesia:create_schema([node()]),
    ok = mnesia:start().