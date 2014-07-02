%%%----------------------------------------------------------------------
%%% File    : config.erl
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
%%% It have been simplified considerably for use in the merqtt. For example it does not
%%% supports macros and recursive include files. It does keep the overall design of
%%% of using mnesia, never editing the config file and distinguishing between global
%%% and local config elements.
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
-module(config).
-author('alexey@process-one.net').
-author('bob@merqtt.org').

-export([start/0, load_file/1,
         add_global_option/2, add_local_option/2,
         get_global_option/1, get_local_option/1]).

-include("merqtt.hrl").

-record(config, {key, value}).

-record(local_config, {key, value}).

-record(state, {opts = [],
                hosts = [],
                override_local = false,
                override_global = false,
                override_acls = false}).

start() ->
    mnesia:create_table(config,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, config)}]),
    mnesia:add_table_copy(config, node(), ram_copies),
    mnesia:create_table(local_config,
                        [{ram_copies, [node()]},
                         {local_content, true},
                         {attributes, record_info(fields, local_config)}]),
    mnesia:add_table_copy(local_config, node(), ram_copies),
    Config = get_config_path(),
    load_file(Config),
    ok.

load_file(File) ->
    Terms = get_terms_file(File),
    State = lists:foldl(fun search_hosts/2, #state{}, Terms),
    Res = lists:foldl(fun process_term/2, State, Terms),
    set_opts(Res).


get_terms_file(File1) ->
    File = get_absolute_path(File1),
    case file:consult(File) of
        {ok, Terms} ->
            Terms;
        {error, {LineNumber, erl_parse, _ParseMessage} = Reason} ->
            ExitText = describe_config_problem(File, Reason, LineNumber),
            ?ERROR(ExitText, []),
            exit_or_halt(ExitText);
        {error, Reason} ->
            ExitText = describe_config_problem(File, Reason),
            ?ERROR(ExitText, []),
            exit_or_halt(ExitText)
    end.

get_absolute_path(File) ->
    case filename:pathtype(File) of
        absolute ->
            File;
        relative ->
            {ok, Cwd} = file:get_cwd(),
            filename:absname_join(Cwd, File)
    end.

search_hosts(Term, State) ->
    case Term of
        {host, Host} ->
            if
                State#state.hosts == [] ->
                    add_hosts_to_option([Host], State);
                true ->
                    ?ERROR("Can't load config file: "
                               "too many hosts definitions", []),
                    exit("too many hosts definitions")
            end;
        {hosts, Hosts} ->
            if
                State#state.hosts == [] ->
                    add_hosts_to_option(Hosts, State);
                true ->
                    ?ERROR("Can't load config file: "
                               "too many hosts definitions", []),
                    exit("too many hosts definitions")
            end;
        _ ->
            State
    end.

add_hosts_to_option(Hosts, State) ->
    add_option(hosts, Hosts, State#state{hosts = Hosts}).

describe_config_problem(Filename, Reason) ->
    Text1 = lists:flatten("Problem loading ejabberd config file " ++ Filename),
    Text2 = lists:flatten(" : " ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    ExitText.

describe_config_problem(Filename, Reason, LineNumber) ->
    Text1 = lists:flatten("Problem loading config file " ++ Filename),
    Text2 = lists:flatten(" approximately in the line "
                          ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    Lines = get_config_lines(Filename, LineNumber, 10, 3),
    ?ERROR("The following lines from your configuration file might be"
               " relevant to the error: ~n~s", [Lines]),
    ExitText.

get_config_lines(Filename, TargetNumber, PreContext, PostContext) ->
    {ok, Fd} = file:open(Filename, [read]),
    LNumbers = lists:seq(TargetNumber-PreContext, TargetNumber+PostContext),
    NextL = io:get_line(Fd, no_prompt),
    R = get_config_lines2(Fd, NextL, 1, LNumbers, []),
    file:close(Fd),
    R.

get_config_lines2(_Fd, eof, _CurrLine, _LNumbers, R) ->
    lists:reverse(R);
get_config_lines2(_Fd, _NewLine, _CurrLine, [], R) ->
    lists:reverse(R);
get_config_lines2(Fd, Data, CurrLine, [NextWanted | LNumbers], R) when is_list(Data) ->
    NextL = io:get_line(Fd, no_prompt),
    if
        CurrLine >= NextWanted ->
            Line2 = [integer_to_list(CurrLine), ": " | Data],
            get_config_lines2(Fd, NextL, CurrLine+1, LNumbers, [Line2 | R]);
        true ->
            get_config_lines2(Fd, NextL, CurrLine+1, [NextWanted | LNumbers], R)
    end.

exit_or_halt(ExitText) ->
    case [Vsn || {merqtt, _Desc, Vsn} <- application:which_applications()] of
        [] ->
            timer:sleep(1000),
            halt(string:substr(ExitText, 1, 199));
        [_] ->
            exit(ExitText)
    end.

process_term(Term, State) ->
    case Term of
        override_global ->
            State#state{override_global = true};
        override_local ->
            State#state{override_local = true};
        {host, _Host} ->
            State;
        {hosts, _Hosts} ->
            State;
        {listen, Listeners} ->
            Listeners2 =
                lists:map(
                  fun({PortIP, Module, Opts}) ->
                          {Port, IPT, _, _, Proto, OptsClean} =
                              merqtt_listener:parse_listener_portip(PortIP, Opts),
                          {{Port, IPT, Proto}, Module, OptsClean}
                  end,
                  Listeners),
            add_option(listen, Listeners2, State);
        {_Opt, _Val} ->
            lists:foldl(fun(Host, S) -> process_host_term(Term, Host, S) end,
                        State, State#state.hosts)
    end.

process_host_term(Term, Host, State) ->
    case Term of
        {host, Host} ->
            State;
        {hosts, _Hosts} ->
            State;
        {Opt, Val} ->
            add_option({Opt, Host}, Val, State)
    end.

add_option(Opt, Val, State) ->
    Table = case Opt of
                hosts ->
                    config;
                _ ->
                    local_config
            end,
    case Table of
        config ->
            State#state{opts = [#config{key = Opt, value = Val} |
                                State#state.opts]};
        local_config ->
            case Opt of
                {{add, OptName}, Host} ->
                    State#state{opts = compact({OptName, Host}, Val,
                                               State#state.opts, [])};
                _ ->
                    State#state{opts = [#local_config{key = Opt, value = Val} |
                                        State#state.opts]}
            end
    end.


compact({OptName, Host} = Opt, Val, [], Os) ->
    ?WARN("The option '~p' is defined for the host ~p using host_config "
                 "before the global '~p' option. This host_config option may "
                 "get overwritten.", [OptName, Host, OptName]),
    [#local_config{key = Opt, value = Val}] ++ Os;
%% Traverse the list of the options already parsed
compact(Opt, Val, [O | Os1], Os2) ->
    case catch O#local_config.key of
        %% If the key of a local_config matches the Opt that wants to be added
        Opt ->
            %% Then prepend the new value to the list of old values
            Os2 ++ [#local_config{key = Opt,
                                  value = Val++O#local_config.value}
                   ] ++ Os1;
        _ ->
            compact(Opt, Val, Os1, Os2++[O])
    end.

set_opts(State) ->
    Opts = lists:reverse(State#state.opts),
    F = fun() ->
                if
                    State#state.override_global ->
                        Ksg = mnesia:all_keys(config),
                        lists:foreach(fun(K) ->
                                              mnesia:delete({config, K})
                                      end, Ksg);
                    true ->
                        ok
                end,
                if
                    State#state.override_local ->
                        Ksl = mnesia:all_keys(local_config),
                        lists:foreach(fun(K) ->
                                              mnesia:delete({local_config, K})
                                      end, Ksl);
                    true ->
                        ok
                end,
                lists:foreach(fun(R) ->
                                      mnesia:write(R)
                              end, Opts)
        end,
    case mnesia:transaction(F) of
        {atomic, _} -> ok;
        {aborted,{no_exists,Table}} ->
            MnesiaDirectory = mnesia:system_info(directory),
            ?ERROR("Error reading Mnesia database spool files:~n"
                       "The Mnesia database couldn't read the spool file for the table '~p'.~n"
                       "ejabberd needs read and write access in the directory:~n   ~s~n"
                       "Maybe the problem is a change in the computer hostname,~n"
                       "or a change in the Erlang node name, which is currently:~n   ~p~n"
                       "Check the ejabberd guide for details about changing the~n"
                       "computer hostname or Erlang node name.~n",
                       [Table, MnesiaDirectory, node()]),
            exit("Error reading Mnesia database")
    end.

add_global_option(Opt, Val) ->
    mnesia:transaction(fun() ->
                               mnesia:write(#config{key = Opt,
                                                    value = Val})
                       end).

add_local_option(Opt, Val) ->
    mnesia:transaction(fun() ->
                               mnesia:write(#local_config{key = Opt,
                                                          value = Val})
                       end).

get_global_option(Opt) ->
    case ets:lookup(config, Opt) of
        [#config{value = Val}] ->
            Val;
        _ ->
            undefined
    end.

get_local_option(Opt) ->
    case ets:lookup(local_config, Opt) of
        [#local_config{value = Val}] ->
            Val;
        _ ->
            undefined
    end.

get_config_path() ->
    case application:get_env(config) of
        {ok, Path} -> Path;
        undefined ->
            case os:getenv("MERQTT_CONFIG_PATH") of
                false ->
                    ?CONFIG_PATH;
                Path ->
                    Path
            end
    end.
