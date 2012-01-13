% -*- tab-width:4; erlang-indent-level: 4; indent-tabs-mode: nil -*-
% ex: ts=4 sw=4 et

%% Copyright (c) 2012, Matteo Panella <morpheus@azzurra.org>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%% 1. Redistributions of source code must retain the above copyright notice,
%%    this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright notice,
%%    this list of conditions and the following disclaimer in the documentation
%%    and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%%-------------------------------------------------------------------
%%% @author Matteo Panella <morpheus@azzurra.org>
%%% @copyright (C) 2012, Matteo Panella
%%% @doc FIXME: write documentation
%%% @end
%%% Created : 12 Jan 2012 by Matteo Panella <morpheus@azzurra.org>
%%%-------------------------------------------------------------------
-module(libirc).

%% ===================================================================
%% Public API
%% ===================================================================
-export([
         parse/1,
         to_rfc1459_upper/1,
         to_rfc1459_lower/1
        ]).

%% -------------------------------------------------------------------
%% Load callback
%% -------------------------------------------------------------------
-on_load(init/0).

%% -------------------------------------------------------------------
%% Placeholder for NIFs
%% -------------------------------------------------------------------
-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% -------------------------------------------------------------------
%% Parser state record
%% -------------------------------------------------------------------
-record(state, {
                last_space = true   :: boolean(),
                prefix = <<>>       :: binary(),
                command = <<>>      :: binary(),
                args = []           :: [binary()],
                accumulator = <<>>  :: binary()
               }).

%%--------------------------------------------------------------------
%% @doc FIXME: write documentation
%% @end
%%--------------------------------------------------------------------
-spec parse(Packet :: iolist() | binary()) -> [proplists:property()].
parse(Packet) when is_list(Packet) ->
    parse(list_to_binary(Packet));
parse(<<":", Packet/binary>>) ->
    % Packet with prefix, extract it
    [Prefix | Tail] = re:split(Packet, " +", [{parts, 2}]),
    case Tail of
        [] ->
            % A single prefix without a command?!?!?!
            throw(malformed_packet);
        _ ->
            parse_command(hd(Tail), Prefix)
    end;
parse(Packet) ->
    % Packet without prefix, fall to parse_command
    parse_command(Packet, <<>>).

%%--------------------------------------------------------------------
%% @doc FIXME: write documentation
%% @end
%%--------------------------------------------------------------------
-spec to_rfc1459_upper(Str :: string() | binary()) -> binary().
to_rfc1459_upper(Str)->
    case rfc1459_upper(Str) of
        {error, Reason} ->
            throw(Reason);
        NewStr when is_list(Str) ->
            binary_to_list(NewStr);
        NewStr ->
            NewStr
    end.

%%--------------------------------------------------------------------
%% @doc FIXME: write documentation
%% @end
%%--------------------------------------------------------------------
-spec to_rfc1459_lower(Str :: string() | binary()) -> binary().
to_rfc1459_lower(Str)->
    case rfc1459_lower(Str) of
        {error, Reason} ->
            throw(Reason);
        NewStr when is_list(Str) ->
            binary_to_list(NewStr);
        NewStr ->
            NewStr
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% -------------------------------------------------------------------
%% Module initializer
%% -------------------------------------------------------------------
-spec init() -> none().
init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

%% -------------------------------------------------------------------
%% IRC parser - second stage: parse command
%% -------------------------------------------------------------------
-spec parse_command(Packet::binary(), Prefix::binary()) -> [proplists:property()].
parse_command(Packet, Prefix) ->
    [Command | Args] = re:split(Packet, " +", [{parts, 2}]),
    case Args of
        [] ->
            parse_args(<<>>, #state{prefix=Prefix, command=Command});
        _ ->
            parse_args(hd(Args), #state{prefix=Prefix, command=Command})
    end.

%% -------------------------------------------------------------------
%% IRC parser - third stage: parse arguments (yikes!)
%% -------------------------------------------------------------------
-spec parse_args(Packet::binary(), State::#state{}) -> [proplists:property()].
parse_args(<<>>, #state{accumulator = Acc} = State) when size(Acc) =:= 0 ->
    % End of arguments, build the final proplist for this packet
    [
     {prefix, State#state.prefix},
     {command, string:to_upper(binary_to_list(State#state.command))},
     {args, lists:reverse(State#state.args)}
    ];
parse_args(<<>>, #state{accumulator = Acc} = State) ->
    % Append leftover argument from accumulator
    parse_args(<<>>, State#state{args=[Acc | State#state.args], accumulator = <<>>});
parse_args(<<":", TrailingArg/binary>>, #state{last_space = LastSpace} = State) when LastSpace =:= true ->
    % Trailing argument handling
    parse_args(<<>>, State#state{args=[TrailingArg | State#state.args]});
parse_args(<<" ", Packet/binary>>, #state{last_space = LastSpace} = State) when LastSpace =:= true ->
    % Skip multiple spaces
    parse_args(Packet, State);
parse_args(<<" ", Packet/binary>>, State) ->
    % End of current argument
    Args = [State#state.accumulator | State#state.args],
    parse_args(Packet, State#state{args=Args, accumulator = <<>>, last_space = true});
parse_args(<<Ch:8, Packet/binary>>, State) ->
    % Default case: append to accumulator
    Acc = <<(State#state.accumulator)/binary, Ch>>,
    parse_args(Packet, State#state{last_space = false, accumulator = Acc}).

%% -------------------------------------------------------------------
%% Stub for internal NIF rfc1459_upper/1
%% -------------------------------------------------------------------
-spec rfc1459_upper(string() | binary()) -> binary()
                                          | {'error', atom()}.
rfc1459_upper(_Str) ->
    ?nif_stub.

%% -------------------------------------------------------------------
%% Stub for internal NIF rfc1459_lower/1
%% -------------------------------------------------------------------
-spec rfc1459_lower(string() | binary()) -> binary()
                                          | {'error', atom()}.
rfc1459_lower(_Str) ->
    ?nif_stub.

%% ===================================================================
%% EUnit Tests
%% ===================================================================

-ifdef(TEST).

%% -------------------------------------------------------------------
%% parse/1 tests
%% -------------------------------------------------------------------
parse_empty_packet_test() ->
    Packet = parse([]),
    ?assertEqual(<<>>,
                 proplists:get_value(prefix, Packet)),
    ?assertEqual([],
                 proplists:get_value(command, Packet)),
    ?assertEqual([],
                 proplists:get_value(args, Packet)),
    ok.

parse_single_command_test() ->
    ?assertEqual("QUIT",
                 proplists:get_value(command, parse("QUIT"))),
    ok.

parse_command_with_prefix_test() ->
    Packet = parse(<<":prefix cmd">>),
    ?assertEqual(<<"prefix">>,
                 proplists:get_value(prefix, Packet)),
    ?assertEqual("CMD",
                 proplists:get_value(command, Packet)),
    ok.

parse_command_with_args_noprefix_test() ->
    Packet = parse(<<"kick #altrove nMe :ciao, papi!">>),
    ?assertEqual(<<>>,
                 proplists:get_value(prefix, Packet)),
    ?assertEqual("KICK",
                 proplists:get_value(command, Packet)),
    ?assertEqual([<<"#altrove">>, <<"nMe">>, <<"ciao, papi!">>],
                 proplists:get_value(args, Packet)),
    ok.

parse_command_with_args_prefix_test() ->
    Packet = parse(<<":roBOTic!bot@localop.azzurra.org KICK #italia Nio :I've got the power!">>),
    ?assertEqual(<<"roBOTic!bot@localop.azzurra.org">>,
                 proplists:get_value(prefix, Packet)),
    ?assertEqual("KICK",
                 proplists:get_value(command, Packet)),
    ?assertEqual([
                  <<"#italia">>,
                  <<"Nio">>,
                  <<"I've got the power!">>
                 ],
                 proplists:get_value(args, Packet)),
    ok.

parse_command_with_no_trailing_arg_test() ->
    Packet = parse(<<":Kab00m!bot@localop.azzurra.org MODE #services +o morph">>),
    ?assertEqual(<<"Kab00m!bot@localop.azzurra.org">>,
                 proplists:get_value(prefix, Packet)),
    ?assertEqual("MODE",
                 proplists:get_value(command, Packet)),
    ?assertEqual([<<"#services">>, <<"+o">>, <<"morph">>],
                 proplists:get_value(args, Packet)),
    ok.

parse_multiple_spaces_between_args_test() ->
    Packet = parse(<<"USER  ident   host  0     :gecos">>),
    ?assertEqual(<<>>,
                 proplists:get_value(prefix, Packet)),
    ?assertEqual("USER",
                 proplists:get_value(command, Packet)),
    ?assertEqual([<<"ident">>, <<"host">>, <<$0>>, <<"gecos">>],
                 proplists:get_value(args, Packet)),
    ok.

parse_prefix_without_command_test() ->
    ?assertThrow(malformed_packet, parse(":loneprefix")),
    ok.

%% -------------------------------------------------------------------
%% to_rfc1459_upper/1 tests
%% -------------------------------------------------------------------
to_rfc1459_upper_identity_test() ->
    ?assertEqual("UPPERCASE",
                 to_rfc1459_upper("UPPERCASE")),
    ?assertEqual(<<"UPPERCASE">>,
                 to_rfc1459_upper(<<"UPPERCASE">>)),
    ?assertEqual("[]\\^",
                 to_rfc1459_upper("[]\\^")),
    ?assertEqual(<<"[]\\^">>,
                 to_rfc1459_upper(<<"[]\\^">>)),
    ok.

to_rfc1459_upper_all_lower_test() ->
    ?assertEqual("ALL LOWER",
                 to_rfc1459_upper("all lower")),
    ?assertEqual(<<"ALL LOWER">>,
                 to_rfc1459_upper(<<"all lower">>)),
    ok.

to_rfc1459_upper_all_special_test() ->
    ?assertEqual("[]\\^",
                 to_rfc1459_upper("{}|~")),
    ?assertEqual(<<"[]\\^">>,
                 to_rfc1459_upper(<<"{}|~">>)),
    ok.

to_rfc1459_upper_mixed_test() ->
    ?assertEqual("MORPH\\AWAY",
                 to_rfc1459_upper("Morph|away")),
    ?assertEqual(<<"MORPH\\AWAY">>,
                 to_rfc1459_upper(<<"Morph|away">>)),
    ok.

%% -------------------------------------------------------------------
%% to_rfc1459_lower/1 tests
%% -------------------------------------------------------------------
to_rfc1459_lower_identity_test() ->
    ?assertEqual("lowercase",
                 to_rfc1459_lower("LOWERCASE")),
    ?assertEqual(<<"lowercase">>,
                 to_rfc1459_lower(<<"LOWERCASE">>)),
    ?assertEqual("{}|~",
                 to_rfc1459_lower("{}|~")),
    ?assertEqual(<<"{}|~">>,
                 to_rfc1459_lower(<<"{}|~">>)),
    ok.

to_rfc1459_lower_all_upper_test() ->
    ?assertEqual("all upper",
                 to_rfc1459_lower("ALL UPPER")),
    ?assertEqual(<<"all upper">>,
                 to_rfc1459_lower(<<"ALL UPPER">>)),
    ok.

to_rfc1459_lower_all_special_test() ->
    ?assertEqual("{}|~",
                 to_rfc1459_lower("[]\\^")),
    ?assertEqual(<<"{}|~">>,
                 to_rfc1459_lower(<<"[]\\^">>)),
    ok.

to_rfc1459_lower_mixed_test() ->
    ?assertEqual("morph|away",
                 to_rfc1459_lower("Morph\\Away")),
    ?assertEqual(<<"morph|away">>,
                 to_rfc1459_lower(<<"Morph\\Away">>)),
    ok.

%% -------------------------------------------------------------------
%% Identity tests for to_rfc1459_upper/1 and to_rfc1459_lower/1
%% -------------------------------------------------------------------
rfc1459_casemapping_identity_test() ->
    TestStringUp = "^\\TESTSTRING\\^",
    TestStringLow = "~|teststring|~",
    TestBinUp = list_to_binary(TestStringUp),
    TestBinLow = list_to_binary(TestStringLow),
    ?assertEqual(TestStringUp,
                 to_rfc1459_upper(to_rfc1459_lower(TestStringUp))),
    ?assertEqual(TestBinUp,
                 to_rfc1459_upper(to_rfc1459_lower(TestBinUp))),
    ?assertEqual(TestStringLow,
                 to_rfc1459_lower(to_rfc1459_upper(TestStringLow))),
    ?assertEqual(TestBinLow,
                 to_rfc1459_lower(to_rfc1459_upper(TestBinLow))),
    ok.

-endif.
