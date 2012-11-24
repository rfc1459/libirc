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
#include "libirc.h"

/* NIF table */
static ErlNifFunc nif_funcs[] =
{
    {"rfc1459_upper", 1, libirc_rfc1459_upper},
    {"rfc1459_lower", 1, libirc_rfc1459_lower}
};

/* Utility functions for returning errors */
ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM atom;
    if (enif_make_existing_atom(env, name, &atom, ERL_NIF_LATIN1))
    {
        return atom;
    }
    return enif_make_atom(env, name);
}

ERL_NIF_TERM
make_error(ErlNifEnv* env, const char* reason)
{
    ERL_NIF_TERM error = make_atom(env, "error");
    return enif_make_tuple2(env, error, make_atom(env, reason));
}

/* Defined so that EUnit tests don't crap out */
static int
libirc_load_stub(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

/* Ditto */
static int
libirc_upgrade_stub(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

/* Likewise */
static void
libirc_unload_stub(ErlNifEnv* env, void* priv_data)
{
    // No-op
    ;
}

/* Standard erl_nif initializer */
ERL_NIF_INIT(libirc, nif_funcs, libirc_load_stub, NULL, libirc_upgrade_stub, libirc_unload_stub);
