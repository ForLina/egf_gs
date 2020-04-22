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

-module(tcp_config).

-behavior(egf_config).

%% API
-export([handle_config/1]).

%% @doc callback of egf_config
-spec handle_config(tuple()) -> ok.
handle_config({?MODULE, ServiceName, Port, TcpOpts,
               [] = _SslOpts, ESockdOpts, ClientMFA})
    when is_atom(ServiceName),
         is_integer(Port),
         is_list(TcpOpts),
         is_list(ESockdOpts),
         is_tuple(ClientMFA) ->
    Opts = [{tcp_options, TcpOpts}| ESockdOpts],
    {ok, _Pid} = esockd:open(ServiceName, Port, Opts, ClientMFA),
    ok;
handle_config({?MODULE, ServiceName, Port, TcpOpts,
               SslOpts, ESockdOpts, ClientMFA})
    when is_atom(ServiceName),
         is_integer(Port),
         is_list(TcpOpts),
         is_list(SslOpts),
         is_list(ESockdOpts),
         is_tuple(ClientMFA) ->
    Opts = [{tcp_options, TcpOpts}, {ssl_options, SslOpts} | ESockdOpts],
    {ok, _Pid} = esockd:open(ServiceName, Port, Opts, ClientMFA),
    ok.