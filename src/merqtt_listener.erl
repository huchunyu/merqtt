%%%----------------------------------------------------------------------
%%% File    : merqtt_listener.erl
%%% Author  : Bob Dionne <bob@merqtt.org>
%%% Purpose : manage startup config information
%%% Created : June, 2014
%%%
%%%
%%% merqtt, Copyright (C) 2014 Dionne Associates
%%%
%%% This code originated in ejabberd and was authored by
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% It has been modified to suit the needs of merqtt.
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------
-module(merqtt_listener).
-author('alexey@process-one.net').
-author('bob@merqtt.org').

-export([parse_listener_portip/2]).

-include("merqtt.hrl").

strip_ip_option(Opts) ->
    {IPL, OptsNoIP} = lists:partition(
                        fun({ip, _}) -> true;
                           (_) -> false
                        end,
                        Opts),
    case IPL of
        %% Only the first ip option is considered
        [{ip, T1} | _] when is_tuple(T1) ->
            {T1, OptsNoIP};
        [] ->
            {no_ip_option, OptsNoIP}
    end.

add_proto(Port, Opts) when is_integer(Port) ->
    {Port, get_proto(Opts)};
add_proto({Port, Proto}, _Opts) when is_atom(Proto) ->
    {Port, normalize_proto(Proto)};
add_proto({Port, Addr}, Opts) ->
    {Port, Addr, get_proto(Opts)};
add_proto({Port, Addr, Proto}, _Opts) ->
    {Port, Addr, normalize_proto(Proto)}.

get_ip_tuple(no_ip_option, inet) ->
    {0, 0, 0, 0};
get_ip_tuple(no_ip_option, inet6) ->
    {0, 0, 0, 0, 0, 0, 0, 0};
get_ip_tuple(IPOpt, _IPVOpt) ->
    IPOpt.

get_proto(Opts) ->
    case proplists:get_value(proto, Opts) of
        undefined ->
            tcp;
        Proto ->
            normalize_proto(Proto)
    end.

normalize_proto(tcp) -> tcp;
normalize_proto(udp) -> udp;
normalize_proto(ws)  -> ws;
normalize_proto(wss) -> wss;
normalize_proto(UnknownProto) ->
    ?WARN("There is a problem in the configuration: "
                 "~p is an unknown IP protocol. Using tcp as fallback",
                 [UnknownProto]),
    tcp.

parse_listener_portip(PortIP, Opts) ->
    {IPOpt, Opts2} = strip_ip_option(Opts),
    {IPVOpt, OptsClean} = case lists:member(inet6, Opts2) of
                              true -> {inet6, Opts2 -- [inet6]};
                              false -> {inet, Opts2}
                          end,
    {Port, IPT, IPS, Proto} =
        case add_proto(PortIP, Opts) of
            {P, Prot} ->
                T = get_ip_tuple(IPOpt, IPVOpt),
                S = inet_parse:ntoa(T),
                {P, T, S, Prot};
            {P, T, Prot} when is_integer(P) and is_tuple(T) ->
                S = inet_parse:ntoa(T),
                {P, T, S, Prot};
            {P, S, Prot} when is_integer(P) and is_list(S) ->
                [S | _] = string:tokens(S, "/"),
                {ok, T} = inet_parse:address(S),
                {P, T, S, Prot}
        end,
    IPV = case size(IPT) of
              4 -> inet;
              8 -> inet6
          end,
    {Port, IPT, IPS, IPV, Proto, OptsClean}.
