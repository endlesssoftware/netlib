/*
**++
**  FACILITY:	NETLIB
**
**  ABSTRACT:	Routines specific to OpenSSL.
**
**  MODULE DESCRIPTION:
**
**  	The problem 
**
**  AUTHOR: 	    Tim Sneddon
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
**  CREATION DATE:  07-FEB-2013
**
**  MODIFICATION HISTORY:
**
**  	07-FEB-2013 V1.0    Sneddon 	Initial coding.
**--
*/
#ifdef __DECC
#pragma module NETLIB_SSL "V1.0"
#else
#module NETLIB_SSL "V1.0"
#endif
#include "netlib_ssl.h"
#include "netlib.h"

/*
**  Forward declarations
*/

    unsigned int netlib_ssl_socket(struct CTX **xctx, void **xsocket,
                                   void **xssl);
    unsigned int netlib_ssl_shutdown(struct CTX **xctx);
    static unsigned int perform_io(struct IOR *IOR);
    /*
    ** These functions are needed by the DNS module
    */
    int netlib___get_nameservers(QUEUE *nsq) { return 0; }
    int netlib___get_domain(char *buf, unsigned short bufsize,
			    unsigned short *relenp) { return 0; }


/*
**  OWN storage
*/

/*
**  External references
*/

/*
**++
**  ROUTINE:	netlib_ssl_socket
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Create an SSL "socket".
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	tbs
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib_ssl_socket (struct CTX **xctx, void **xsocket,
				void **xssl) {

    int argc;
    struct CTX *ctx;
    unsigned int status = SS$_INSFMEM;

    SETARGCOUNT(argc);
    if (argc < 3) return SS$_INSFARG;
    if (xsocket == 0 || xssl == 0) return SS$_BADPARAM;
    if (*xsocket == 0 || *xssl == 0) return SS$_BADPARAM;

    status = netlib___alloc_ctx(&ctx, SPECCTX_SIZE);
    if (!OK(status)) return status;

    if ((ctx->specctx->rbio = BIO_new(BIO_s_mem())) != 0) {
	if ((ctx->specctx->wbio = BIO_new(BIO_s_mem())) != 0) {
	    if ((ctx->specctx->ssl = SSL_new(*xssl)) != 0) {
		SSL_set_bio(ctx->specctx->ssl, ctx->specctx->rbio,
			    ctx->specctx->wbio); 
		status = SS$_NORMAL;
	    }
	}
    }

    if (!OK(status)) {
	if (ctx->specctx->rbio != 0) BIO_free(ctx->specctx->rbio);
	if (ctx->specctx->wbio != 0) BIO_free(ctx->specctx->wbio);
	if (ctx->specctx->ssl != 0) SSL_free(ctx->specctx->ssl);
	netlib___free_ctx(ctx);
    } else {
	*xctx = ctx;
    }
    return status;
} /* netlib_ssl_socket */

/*
**++
**  ROUTINE:	netlib_ssl_shutdown
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Shutdown an SSL connection.  This does not close the underlying
**  NETLIB socket.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	tbs
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib_ssl_shutdown (struct CTX **xctx,
				  struct NETLIBIOSBDEF *iosb,
				  void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    unsigned int status;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(arc);

    if (argc < 1) return SS$_INSFARG;

    if (argc > 2 && astadr != 0) {
	struct IOR *ior;
	GET_IOR(ior, ctx, iosb, astadr, (argc > 3) ? astprm : 0);
	argv = malloc(1 + 1);
	if (argv == 0) {
	    status = SS$_INSFMEM;
	} else {
	    argv[0] = 1;
	    argv[1] = ctx->specctx->ssl;
	    ior->spec_argv = argv;
	    ior->spec_call = SSL_shutdown;
	    status = sys$dclast(io_perform, ior, 0);
	}
	if (!OK(status)) {
	    if (ior->spec_argv != 0) free(ior->spec_argv);
	    FREE_IOR(ior);
	}
    } else {
	// we don't do anything here yet...how are we going to handle this?
    }

    return status;
} /* netlib_ssl_shutdown */

static unsigned int io_perform(struct IOR *ior) {

    int ret, status;
    struct CTX *ctx = ior->ctx;

    if (SSL_want(ctx->spec_ssl) == SSL_WANT_READ) {
	// write into bio the contents of buffer...
	ret = BIO_write(ctx->spec_ssl, buf, len);
	// what do we do with ret?
    }

    ret = lib$callg(ior->spec_argv, ior->spec_call);
    switch (status = SSL_get_error(ctx->spec_ssl, ret)) {
    case SSL_ERROR_NONE:
    case SSL_ERROR_ZERO_RETURN:
	if (ior->iosbp != 0) {
	    ior->iosbp->iosb_w_status = SS$_NORMAL;
	    ior->iosbp->iosb_w_unused = 0;
	    ior->iosbp->iosb_w_count = ret;
	}

	// cancel timer AST too...

	if (ior->spec_argv != 0) free(ior->spec_argv);
	if (ior->astadr != 0) (*(ior->astadr))(ior->astprm);
	FREE_IOR(ior);
	break;

    case SSL_ERROR_WANT_READ:
	status = netlib_read(ctx->spec_socket, buffer, 0, 0,
			     0, timeout, &ior->iosb, io_perform, ior);
	if (!OK(status)) {
	    // error, this needs to be passed to iosbp...
	}
	break;

    case SSL_ERROR_WANT_WRITE:
	// read out of the BIO and in to the buffer...

	status = netlib_write(ctx->spec_socket, buffer, 0, 0,
			      &iot->iosb, io_perform, ior);
	if (!OK(status)) {
	    // error, this needs to be passed to iosbp...
	}
	break;

    default:
	// fall through here for the moment...
	break;
    }

    // need a flag in the contect that specifies if we need to clean up the
    // socket after.  However...we can't do it until the pending I/O has
    // been completed...

    // SSL_free, BIO_free, etc...

    return ...;
} /* io_perform */

#if 0
/*
**++
**  ROUTINE:	netlib_socket
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Create a "socket".
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	tbs
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib_ssl_accept (struct CTX **xctx, void *

    struct CTX *ctx;

} /* netlib_ssl_accept */

netlib_ssl_accept
netlib_ssl_write
netlib_ssl_read
netlib_ssl_shutdown

// LIB$INITIALIZE...
/*

This bit need to load the SSL RTL in question and find us the entry points.
It also needs to call SSL_library_init() and SSL_load_error_strings()

This way we can code around differences in API, rather than being stuck
linking to a specific version.

Probably need a static variable to test upon entry to all SSL functions
to return some sort of SS$_ status that indicates that SSL is not available.

Again, this all comes later.  Let's get the API working first.

netlib_ssl_setup...do some generic configuration that means the user does not
  need to call the SSL API directly.

Don't forget to add the public API into the NETLIBDEF header file...
*/
#endif
