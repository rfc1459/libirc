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
%%% libirc EUnit tests
%%% Created : 13 Jan 2012 by Matteo Panella <morpheus@azzurra.org>
%%%-------------------------------------------------------------------
-module(libirc_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% EUnit generator for PropEr tests
%% ===================================================================
proper_test_() ->
    [{atom_to_list(F),
      fun() -> ?assert(proper:quickcheck(?MODULE:F(), [long_result])) end}
     || {F, 0} <- ?MODULE:module_info(exports), F > 'prop_', F < 'prop`'].

%% ===================================================================
%% libirc:parse/1 tests
%% ===================================================================
parse_empty_packet_test() ->
    Packet = libirc:parse([]),
    ?assertEqual(<<>>,
                 proplists:get_value(prefix, Packet)),
    ?assertEqual([],
                 proplists:get_value(command, Packet)),
    ?assertEqual([],
                 proplists:get_value(args, Packet)),
    ok.

parse_single_command_test() ->
    ?assertEqual("QUIT",
                 proplists:get_value(command, libirc:parse("QUIT"))),
    ok.

parse_command_with_prefix_test() ->
    Packet = libirc:parse(<<":prefix cmd">>),
    ?assertEqual(<<"prefix">>,
                 proplists:get_value(prefix, Packet)),
    ?assertEqual("CMD",
                 proplists:get_value(command, Packet)),
    ok.

parse_command_with_args_noprefix_test() ->
    Packet = libirc:parse(<<"kick #altrove nMe :ciao, papi!">>),
    ?assertEqual(<<>>,
                 proplists:get_value(prefix, Packet)),
    ?assertEqual("KICK",
                 proplists:get_value(command, Packet)),
    ?assertEqual([<<"#altrove">>, <<"nMe">>, <<"ciao, papi!">>],
                 proplists:get_value(args, Packet)),
    ok.

parse_command_with_args_prefix_test() ->
    Packet = libirc:parse(<<":roBOTic!bot@localop.azzurra.org KICK #italia Nio :I've got the power!">>),
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
    Packet = libirc:parse(<<":Kab00m!bot@localop.azzurra.org MODE #services +o morph">>),
    ?assertEqual(<<"Kab00m!bot@localop.azzurra.org">>,
                 proplists:get_value(prefix, Packet)),
    ?assertEqual("MODE",
                 proplists:get_value(command, Packet)),
    ?assertEqual([<<"#services">>, <<"+o">>, <<"morph">>],
                 proplists:get_value(args, Packet)),
    ok.

parse_multiple_spaces_between_args_test() ->
    Packet = libirc:parse(<<"USER  ident   host  0     :gecos">>),
    ?assertEqual(<<>>,
                 proplists:get_value(prefix, Packet)),
    ?assertEqual("USER",
                 proplists:get_value(command, Packet)),
    ?assertEqual([<<"ident">>, <<"host">>, <<$0>>, <<"gecos">>],
                 proplists:get_value(args, Packet)),
    ok.

parse_prefix_without_command_test() ->
    ?assertThrow(malformed_packet, libirc:parse(":loneprefix")),
    ok.

%% ===================================================================
%% libirc:to_rfc1459_upper/1 tests
%% ===================================================================
to_rfc1459_upper_identity_test() ->
    ?assertEqual("UPPERCASE",
                 libirc:to_rfc1459_upper("UPPERCASE")),
    ?assertEqual(<<"UPPERCASE">>,
                 libirc:to_rfc1459_upper(<<"UPPERCASE">>)),
    ?assertEqual("[]\\^",
                 libirc:to_rfc1459_upper("[]\\^")),
    ?assertEqual(<<"[]\\^">>,
                 libirc:to_rfc1459_upper(<<"[]\\^">>)),
    ok.

to_rfc1459_upper_all_lower_test() ->
    ?assertEqual("ALL LOWER",
                 libirc:to_rfc1459_upper("all lower")),
    ?assertEqual(<<"ALL LOWER">>,
                 libirc:to_rfc1459_upper(<<"all lower">>)),
    ok.

to_rfc1459_upper_all_special_test() ->
    ?assertEqual("[]\\^",
                 libirc:to_rfc1459_upper("{}|~")),
    ?assertEqual(<<"[]\\^">>,
                 libirc:to_rfc1459_upper(<<"{}|~">>)),
    ok.

to_rfc1459_upper_mixed_test() ->
    ?assertEqual("MORPH\\AWAY",
                 libirc:to_rfc1459_upper("Morph|away")),
    ?assertEqual(<<"MORPH\\AWAY">>,
                 libirc:to_rfc1459_upper(<<"Morph|away">>)),
    ok.

%% ===================================================================
%% libirc:to_rfc1459_lower/1 tests
%% ===================================================================
to_rfc1459_lower_identity_test() ->
    ?assertEqual("lowercase",
                 libirc:to_rfc1459_lower("LOWERCASE")),
    ?assertEqual(<<"lowercase">>,
                 libirc:to_rfc1459_lower(<<"LOWERCASE">>)),
    ?assertEqual("{}|~",
                 libirc:to_rfc1459_lower("{}|~")),
    ?assertEqual(<<"{}|~">>,
                 libirc:to_rfc1459_lower(<<"{}|~">>)),
    ok.

to_rfc1459_lower_all_upper_test() ->
    ?assertEqual("all upper",
                 libirc:to_rfc1459_lower("ALL UPPER")),
    ?assertEqual(<<"all upper">>,
                 libirc:to_rfc1459_lower(<<"ALL UPPER">>)),
    ok.

to_rfc1459_lower_all_special_test() ->
    ?assertEqual("{}|~",
                 libirc:to_rfc1459_lower("[]\\^")),
    ?assertEqual(<<"{}|~">>,
                 libirc:to_rfc1459_lower(<<"[]\\^">>)),
    ok.

to_rfc1459_lower_mixed_test() ->
    ?assertEqual("morph|away",
                 libirc:to_rfc1459_lower("Morph\\Away")),
    ?assertEqual(<<"morph|away">>,
                 libirc:to_rfc1459_lower(<<"Morph\\Away">>)),
    ok.

%% ===================================================================
%% Identity tests for libirc:to_rfc1459_upper/1 and
%% libirc:to_rfc1459_lower/1
%% ===================================================================
rfc1459_casemapping_identity_test() ->
    TestStringUp = "^\\TESTSTRING\\^",
    TestStringLow = "~|teststring|~",
    TestBinUp = list_to_binary(TestStringUp),
    TestBinLow = list_to_binary(TestStringLow),
    ?assertEqual(TestStringUp,
                 libirc:to_rfc1459_upper(libirc:to_rfc1459_lower(TestStringUp))),
    ?assertEqual(TestBinUp,
                 libirc:to_rfc1459_upper(libirc:to_rfc1459_lower(TestBinUp))),
    ?assertEqual(TestStringLow,
                 libirc:to_rfc1459_lower(libirc:to_rfc1459_upper(TestStringLow))),
    ?assertEqual(TestBinLow,
                 libirc:to_rfc1459_lower(libirc:to_rfc1459_upper(TestBinLow))),
    ok.


%% ===================================================================
%% PropEr tests for irclib:to_rfc1459_upper/1
%% ===================================================================

%% -------------------------------------------------------------------
%% Ensure same length for input and output and correct mapping
%% -------------------------------------------------------------------
prop_to_upper_same_length_correct_mapping() ->
    ?FORALL(Str, union([binary(), list(byte())]),
        begin
                UpStr = libirc:to_rfc1459_upper(Str),
                conjunction([
                             {same_length, subprop_same_length(Str, UpStr)},
                             {correct_mapping, subprop_upper_map(Str, UpStr)}
                            ])
        end).

%% -------------------------------------------------------------------
%% Sub-property: ensure each character translates correctly to upper
%%               case
%% -------------------------------------------------------------------
subprop_upper_map(Original, Upper) when is_binary(Original), is_binary(Upper) ->
    subprop_upper_map(binary_to_list(Original), binary_to_list(Upper));
subprop_upper_map(Original, Upper) when is_list(Original), is_list(Upper) ->
    lists:all(fun upper_check/1, lists:zip(Original, Upper)).

%% RFC1459 to_upper mapping rules
upper_check({Original, Upper}) ->
    Upper =:= case Original of
        Alpha when Alpha >= $a, Alpha =< $z ->
            Alpha band 223;
        Sym when Sym >= ${, Sym =< $~ ->
            Sym band 223;
        Other ->
            Other
    end.


%% ===================================================================
%% PropEr tests for irclib:to_rfc1459_lower/1
%% ===================================================================

%% -------------------------------------------------------------------
%% Ensure same length for input and output and correct mapping
%% -------------------------------------------------------------------
prop_to_lower_same_length_correct_mapping() ->
    ?FORALL(Str, union([binary(), list(byte())]),
        begin
                LowStr = libirc:to_rfc1459_lower(Str),
                conjunction([
                             {same_length, subprop_same_length(Str, LowStr)},
                             {correct_mapping, subprop_lower_map(Str, LowStr)}
                            ])
        end).

%% -------------------------------------------------------------------
%% Sub-property: ensure each character translates correctly to lower
%%               case
%% -------------------------------------------------------------------
subprop_lower_map(Original, Lower) when is_binary(Original), is_binary(Lower) ->
    subprop_lower_map(binary_to_list(Original), binary_to_list(Lower));
subprop_lower_map(Original, Lower) when is_list(Original), is_list(Lower) ->
    lists:all(fun lower_check/1, lists:zip(Original, Lower)).

%% RFC1459 to_lower mapping rules
lower_check({Original, Lower}) ->
    Lower =:= case Original of
        Alpha when Alpha >= $A, Alpha =< $Z ->
            Alpha bor 32;
        Sym when Sym >= $[, Sym =< $^ ->
            Sym bor 32;
        Other ->
            Other
    end.

%% ===================================================================
%% Common sub-properties
%% ===================================================================

%% -------------------------------------------------------------------
%% Ensure both arguments have the same length
%% -------------------------------------------------------------------
subprop_same_length(A, B) when is_binary(A), is_binary(B) ->
    size(A) =:= size(B);
subprop_same_length(A, B) when is_list(A), is_list(B) ->
    length(A) =:= length(B).
