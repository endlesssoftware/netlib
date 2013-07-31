#ifndef __NETLIB_SSL_H__
#define __NETLIB_SSL_H__
/*
** NETLIB_SSL.H
**
**  Internal #include file for SSL driver.
**
**  Copyright (c) 2013, Endless Software Solutions.
**
**  All rights reserved.
**
**  Redistribution and use in source and binary forms, with or without
**  modification, are permitted provided that the following conditions
**  are met:
**
**      * Redistributions of source code must retain the above
**        copyright notice, this list of conditions and the following
**        disclaimer.
**      * Redistributions in binary form must reproduce the above
**        copyright notice, this list of conditions and the following
**        disclaimer in the documentation and/or other materials provided
**        with the distribution.
**      * Neither the name of the copyright owner nor the names of any
**        other contributors may be used to endorse or promote products
**        derived from this software without specific prior written
**        permission.
**
**  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
**  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
**  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
**  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
**  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
**  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
**  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
**  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
**  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
**  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
**  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
**  AUTHOR:	    Tim Sneddon
**
**  CREATION DATE:  06-Feb-2013
**
**  MODIFICATION HISTORY:
**
**  06-Feb-2013     Sneddon     Initial coding.
**  31-Jul-2013     Sneddon   	Changes to implement IOR queue.
*/
#include "netlibdef.h"
#ifndef __NETLIB_BUILD__
#define __NETLIB_BUILD__
#endif
#include <descrip.h>
#include <errno.h>
#include <rmsdef.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <openssl/bio.h>
#include <openssl/err.h>
#include <openssl/ssl.h>

#define BUF_MAX  16384

/*
** Connect-level SSL specific fields.
*/
#define spec_socket	specctx->socket
#define spec_ssl	specctx->ssl
#define spec_inbio	specctx->inbio
#define spec_outbio	specctx->outbio
#define spec_data	specctx->data
#define spec_flags	specctx->flags
#define spec_iorque	specctx->iorque

#define IOR_M_COMPLETE  (1<<16)

    struct SPECCTX {
	void *socket;
	SSL *ssl;
	BIO *inbio, *outbio;
	struct dsc$descriptor data;
	unsigned flags;
	QUEUE iorque;
    };
#define __SPECCTX struct SPECCTX
#define SPECCTX_SIZE sizeof(__SPECCTX)

/*
** I/O-level SSL specific fields.
*/

#define spec_args	arg
#define spec_argc	arg[0].longword
#define spec_argv(i)	arg[(i)+1]
#define spec_call	specior.call

    struct SPECIOR {
	int (*call)();
    };
#define __SPECIOR struct SPECIOR

#endif /* __NETLIB_SSL_H__ */
