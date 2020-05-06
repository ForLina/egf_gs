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

-module(login_port).

%% API
-export([handle_first_packet/2]).

-include("mod_mapping.hrl").
-include("login.hrl").
-include("pb_login.hrl").

%% @doc Handle the first packet from client, must be a login or reconnect packet
-spec handle_first_packet(binary(), pid()) ->
    {ok, Reply :: binary(), PlayerPid :: pid()} | {error, Reply :: binary()}.
handle_first_packet(<<?MOD_LOGIN:8/integer,
                      ?CS_LOGIN:8/integer,
                      0:16/integer, Proto>>, ConnectProcess) ->
    Req = pb_login:decode_msg(Proto, cs_login),
    login(Req, ConnectProcess);
handle_first_packet(<<?MOD_LOGIN:8/integer,
                      ?CS_RECONNECT:8/integer,
                      0:16/integer, Proto>>, ConnectProcess) ->
    Req = pb_login:decode_msg(Proto, cs_reconnect),
    reconnect(Req, ConnectProcess).

login(#cs_login{aid = Aid}, CPid) ->
    case player:get_uid_by_aid(Aid) of
        Uid when is_integer(Uid) ->
            ok;
        undefined ->
            Uid = player:init_player(Aid)
    end,
    
    RetCode =
    case player:get_pid(Uid) of
        undefined ->
            case player:create_process(Uid) of
                {ok, PlayerPid} ->
                    ok = player:update_cpid(PlayerPid, CPid),
                    0;
                {error, _Reason} ->
                    PlayerPid = undefined,
                    ?CREATE_PROCESS_FAIL
            end;
        PlayerPid ->
            ok = player:kick(PlayerPid),
            ok = player:update_cpid(PlayerPid, CPid),
            0
    end,
    
    RetMsg =  #sc_login{ret_code = RetCode, player_uid = Uid},
    RetBin = pb_login:encode_msg(RetMsg),
    
    case RetCode of
        0 ->
            {ok, RetBin, PlayerPid};
        _Other ->
            {error, RetCode}
    end.
    
%% @todo
reconnect(#cs_reconnect{}, _CPid) ->
    ok.