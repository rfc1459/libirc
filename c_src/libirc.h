/*
 * libirc.h: Erlang NIF functions for libirc
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

#ifndef LIBIRC_H
#define LIBIRC_H 1

/* Function prototypes */

/* libirc.c */
extern ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
extern ERL_NIF_TERM make_error(ErlNifEnv* env, const char* reason);

/* libirc_casemapping.c */
extern ERL_NIF_TERM libirc_rfc1459_upper(ErlNifEnv* env, int argc,
                                         const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM libirc_rfc1459_lower(ErlNifEnv* env, int argc,
                                         const ERL_NIF_TERM argv[]);

#endif /* LIBIRC_H */
