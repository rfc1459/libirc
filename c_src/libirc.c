/*
 * libirc.c: Erlang NIF functions for libirc
 * Copyright (c) 2012, Matteo Panella <morpheus@azzurra.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include "erl_nif.h"

/* Function prototypes */
static ERL_NIF_TERM libirc_rfc1459_upper(ErlNifEnv* env, int argc,
                                         const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM libirc_rfc1459_lower(ErlNifEnv* env, int argc,
                                         const ERL_NIF_TERM argv[]);

/* NIF table */
static ErlNifFunc nif_funcs[] =
{
    {"rfc1459_upper", 1, libirc_rfc1459_upper},
    {"rfc1459_lower", 1, libirc_rfc1459_lower}
};

/* RFC1459 casemapping tables */
static const char rfc1459_lower_tbl[] = {
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
     ' ',  '!',  '"',  '#',  '$',  '%',  '&', '\'',
     '(',  ')',  '*',  '+',  ',',  '-',  '.',  '/',
     '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
     '8',  '9',  ':',  ';',  '<',  '=',  '>',  '?',
     '@',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
     'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
     'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
     'x',  'y',  'z',  '{',  '|',  '}',  '~',  '_',
     '`',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
     'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
     'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
     'x',  'y',  'z',  '{',  '|',  '}',  '~', 0x7f,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
    0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
    0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
    0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
    0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
    0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
    0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
    0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
    0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
    0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
    0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
};

static const char rfc1459_upper_tbl[] = {
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
     ' ',  '!',  '"',  '#',  '$',  '%',  '&', '\'',
     '(',  ')',  '*',  '+',  ',',  '-',  '.',  '/',
     '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
     '8',  '9',  ':',  ';',  '<',  '=',  '>',  '?',
     '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
     'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
     'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
     'X',  'Y',  'Z',  '[', '\\',  ']',  '^',  '_',
     '`',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
     'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
     'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
     'X',  'Y',  'Z',  '[', '\\',  ']',  '^', 0x7f,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
    0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
    0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
    0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
    0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
    0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
    0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
    0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
    0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
    0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
    0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
};

/* Utility functions for returning errors */
static inline
ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM atom;
    if (enif_make_existing_atom(env, name, &atom, ERL_NIF_LATIN1))
    {
        return atom;
    }
    return enif_make_atom(env, name);
}

static inline
ERL_NIF_TERM make_error(ErlNifEnv* env, const char* reason)
{
    ERL_NIF_TERM error = make_atom(env, "error");
    return enif_make_tuple2(env, error, make_atom(env, reason));
}

/*
 * Cast a given string or binary to upper/lower case following RFC1459 rules.
 * Arguments    : Str :: binary(), ToUpper :: 1 | 0
 * Returns      : binary() | {'error', Reason::atom()}
 */
static
ERL_NIF_TERM libirc_rfc1459_case(char ToUpper, ErlNifEnv* env,
                                 int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary str, rv;
    const char* trans_tbl = ToUpper ? rfc1459_upper_tbl : rfc1459_lower_tbl;
    size_t i;

    /* Do we have a binary? */
    if (!enif_inspect_binary(env, argv[0], &str))
    {
        /* Try again as an iolist */
        if (!enif_inspect_iolist_as_binary(env, argv[0], &str))
        {
            /* Nope, throw a badarg */
            return enif_make_badarg(env);
        }
    }

    /* Create a new binary with the same size of the incoming argument */
    if (!enif_alloc_binary(str.size, &rv))
    {
        /* erl_nif does not implement a way to throw an arbitrary exception
         * other than badarg, so we have to return {error, bad_alloc} to the
         * Erlang caller and have it throw an exception in our stead.
         */
        return make_error(env, "bad_alloc");
    }

    /* Pretty self-explanatory */
    for (i = 0; i < str.size; i++)
        rv.data[i] = trans_tbl[str.data[i]];

    /* Transfer ownership of the target binary to Erlang */
    return enif_make_binary(env, &rv);
}

/*
 * Cast a given string or binary to upper case following RFC1459 rules.
 * Arguments    : Str :: binary()
 * Returns      : binary() | {'error', Reason::atom()}
 */
static
ERL_NIF_TERM libirc_rfc1459_upper(ErlNifEnv* env, int argc,
                                  const ERL_NIF_TERM argv[])
{
    return libirc_rfc1459_case(1, env, argc, argv);
}


/*
 * Cast a given string or binary to lower case following RFC1459 rules.
 * Arguments    : Str :: binary()
 * Returns      : binary() | {'error', Reason::atom()}
 */
static
ERL_NIF_TERM libirc_rfc1459_lower(ErlNifEnv* env, int argc,
                                  const ERL_NIF_TERM argv[])
{
    return libirc_rfc1459_case(0, env, argc, argv);
}

/* Standard erl_nif initializer */
ERL_NIF_INIT(libirc, nif_funcs, NULL, NULL, NULL, NULL);
