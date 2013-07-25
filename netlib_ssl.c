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
    unsigned int netlib_ssl_accept(struct CTX **xctx,
				   struct NETLIBIOSBDEF *iosb,
			           void (*astadr)(), void *astprm);
    unsigned int netlib_ssl_connect(struct CTX **xctx,
				   struct NETLIBIOSBDEF *iosb,
			           void (*astadr)(), void *astprm);
    unsigned int netlib_ssl_shutdown(struct CTX **xctx,
				     struct NETLIBIOSBDEF *iosb,
			             void (*astadr)(), void *astprm);
    static unsigned int io_perform(struct IOR *IOR);
    int netlib___cvt_status(int err, ...);

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
**
**  SS$_INSFMEM - unable to allocate internal SSL structures
**  SS$_BADCHECKSUM - key and cert do not match
*/
unsigned int netlib_ssl_context (void **xssl, unsigned int method,
				 struct dsc$descriptor *cert_d, int *cert_type,
				 struct dsc$descriptor *key_d, int *key_type,
				 unsigned int *verify) {

    SSL_CTX *ssl;
    int argc, ret, status = SS$_NORMAL;
    char *cert = 0, *key = 0, *ptr;
    unsigned short len;

    SETARGCOUNT(argc);

    ssl = SSL_CTX_new((method == NETLIB_K_METHOD_SSL2) ? SSLv2_method() :
		      (method == NETLIB_K_METHOD_SSL3) ? SSLv3_method() :
		      (method == NETLIB_K_METHOD_TLS1) ? TLSv1_method() :
		       SSLv23_method());
    if (ssl == 0) return SS$_INSFMEM;

    status = lib$analyze_sdesc(cert_d, &len, &ptr);
    if (OK(status)) {
	cert = malloc(len+1);
	if (cert == 0) {
	    status = SS$_INSFMEM;
	} else {
	    memcpy(cert, ptr, len);
	    cert[len] = '\0';

	    status = lib$analyze_sdesc(key_d, &len, &ptr);
	    if (OK(status)) {
		key = malloc(len+1);
		if (key == 0) {
		    status = SS$_INSFMEM;
		} else {
		    memcpy(key, ptr, len);
		    key[len] = '\0';
		}
	    }
	}
    }

    if (OK(status)) {
	ret = SSL_CTX_use_certificate_file(ssl, cert, *cert_type);
	if (ret <= 0) {
	    status = vaxc$errno;
printf("use_cer,ret=%d,errno=%d,vaxc$errno=%d,%s\n",ret,errno,vaxc$errno,
		strerror(errno,vaxc$errno));
ret = ERR_peek_error();
printf("SSL = %d\n", ret);
printf("LIB = %d, FUNC = %d, REASON = %d\n", ERR_GET_LIB(ret),
		ERR_GET_FUNC(ret), ERR_GET_REASON(ret));
	} else {
	    ret = SSL_CTX_use_PrivateKey_file(ssl, key, *key_type);
	    if (ret <= 0) {
		status = vaxc$errno;
printf("use_priv,ret=%d,errno=%d,vaxc$errno=%d,%s\n",ret,errno,vaxc$errno,
		strerror(errno,vaxc$errno));
ret = ERR_peek_error();
printf("SSL = %d\n", ret);
printf("LIB = %d, FUNC = %d, REASON = %d\n", ERR_GET_LIB(ret),
		ERR_GET_FUNC(ret), ERR_GET_REASON(ret));
printf("\n");
	    } else {
		ret = SSL_CTX_check_private_key(ssl);
		if (ret <= 0) {
		    printf("errno=%d,ret=%d,vaxc$errno=%d,%s\n",
				errno,ret,vaxc$errno,
				strerror(errno,vaxc$errno));
		    status = SS$_BADCHECKSUM;
		}
	    }
	}
    }

    if (cert != 0) free(cert);
    if (key != 0) free(key);

    if (!OK(status)) {
	SSL_CTX_free(ssl);
    } else {
	*xssl = ssl;
    }
    return status;
} /* netlib_ssl_context */

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
**	SS$_BADCONTEXT - SSL context is bad
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

    if ((ctx->spec_rbio = BIO_new(BIO_s_mem())) != 0) {
	if ((ctx->spec_wbio = BIO_new(BIO_s_mem())) != 0) {
	    if ((ctx->spec_ssl = SSL_new(*xssl)) != 0) {
		SSL_set_bio(ctx->spec_ssl, ctx->spec_rbio, ctx->spec_wbio); 
		status = SS$_NORMAL;
	    } else {
		status = SS$_BADCONTEXT;
	    }
	}
    }

    if (!OK(status)) {
	if (ctx->spec_rbio != 0) BIO_free(ctx->spec_rbio);
	if (ctx->spec_wbio != 0) BIO_free(ctx->spec_wbio);
	if (ctx->spec_ssl != 0) SSL_free(ctx->spec_ssl);
	netlib___free_ctx(ctx);
    } else {
	*xctx = ctx;
    }
    return status;
} /* netlib_ssl_socket */

#if 0
/*
**++
**  ROUTINE:	netlib_ssl_accept
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Server side that accepts and incoming SSL connection.
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
unsigned int netlib_ssl_accept (struct CTX **xctx,
			        struct NETLIBIOSBDEF *iosb,
			        void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    unsigned int status;
    int argc, *argv;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 1) return SS$_INSFARG;

    if (argc > 2 && astadr != 0) {
	struct IOR *ior;
	GET_IOR(ior, ctx, iosb, astadr, (argc > 3) ? astprm : 0);
	argv = malloc(1 + 1);
	if (argv == 0) {
	    status = SS$_INSFMEM;
	} else {
	    argv[0] = 1;
	    argv[1] = (int) ctx->spec_ssl;
	    ior->spec_argv = argv;
	    ior->spec_call = SSL_accept;
	    //status = sys$dclast(io_perform, ior, 0);
	}
	if (!OK(status)) {
	    if (ior->spec_argv != 0) free(ior->spec_argv);
	    FREE_IOR(ior);
	}
    } else {
	// we don't do anything here yet...how are we going to handle this?
    }

    return status;
} /* netlib_ssl_accept */
#endif

/*
**++
**  ROUTINE:	netlib_ssl_connect
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Client side routine that makes an outgoing SSL connection.
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
unsigned int netlib_ssl_connect (struct CTX **xctx,
			         struct NETLIBIOSBDEF *iosb,
			         void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    unsigned int status;
    int argc, *argv;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 1) return SS$_INSFARG;

    if (argc > 2 && astadr != 0) {
	struct IOR *ior;
	GET_IOR(ior, ctx, iosb, astadr, (argc > 3) ? astprm : 0);
	ior->spec_argc = 1;
	ior->spec_argv(0).address = ctx->spec_ssl;
	ior->spec_call = SSL_connect;
	status = sys$dclast(io_perform, ior, 0);
	if (!OK(status)) FREE_IOR(ior);
    } else {
	// we don't do anything here yet...how are we going to handle this?
    }

    return status;
} /* netlib_ssl_connect */

#if 0
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
    int argc, *argv;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 1) return SS$_INSFARG;

    if (argc > 2 && astadr != 0) {
	struct IOR *ior;
	GET_IOR(ior, ctx, iosb, astadr, (argc > 3) ? astprm : 0);
	argv = malloc(1 + 1);
	if (argv == 0) {
	    status = SS$_INSFMEM;
	} else {
	    argv[0] = 1;
	    argv[1] = (int) ctx->spec_ssl;
	    ior->spec_argv = argv;
	    ior->spec_call = SSL_shutdown;
	    //status = sys$dclast(io_perform, ior, 0);
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
#endif

#if 0
unsigned int netlib_ssl_write (struct CTX **xctx, struct dsc$descriptor *dsc,
			       struct NETLIBIOSBDEF *iosb,
			       void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    void *bufptr;
    unsigned int status;
    unsigned short buflen;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    status = lib$analyze_sdesc(dsc, &buflen, &bufptr);
    if (!OK(status)) return status;

    if (argc > 3) {
	struct IOR *ior;
	GET_IOR(ior, ctx, iosb, astadr, (argc > 4) ? astprm : 0);
	ctx->spec_bptr = malloc(buflen);
	if (ctx->spec_bptr != 0) {
	    memcpy(ctx->spec_bptr, bufptr, buflen);
	    ctx->spec_blen = buflen;
	    argv = malloc(3 + 1);
	    if (argv != 0) {
	    	argv[0] = 3;
	    	argv[1] = ctx->spec_ssl;
	    	argv[2] = bufptr;
	    	argv[3] = buflen;
	    	ior->spec_argv = argv;
	    	ior->spec_call = SSL_write;
	    	status = sys$dclast(io_perform, ior, 0);
	    } else {
	    	status = SS$_INSFMEM;
	    }
	} else {
	    status = SS$_INSFMEM;
	}
	if (!OK(status)) {
	    if (ior->spec_bptr != 0) free(ior->spec_bptr);
	    if (ior->spec_argv != 0) free(ior->spec_argv);
	    FREE_IOR(ior);
	}
    } else {
	// not handling sychronous stuff right now...
    }
}
#endif

#if 0
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
	// cancel Timer?
	if (ior->spec_argv != 0) free(ior->spec_argv);
	if (ior->astadr != 0) (*(ior->astadr))(ior->astprm);
	FREE_IOR(ior);
	break;

    case SSL_ERROR_WANT_READ:
	ior->spec_rdsc.dsc$w_length = BUF_MAX;
	ior->spec_rdsc.dsc$b_dtype = DSC$K_DTYPE_T;
	ior->spec_rdsc.dsc$b_class = DSC$K_CLASS_S;
	ior->spec_rdsc.dsc$a_pointer = ior->rbuf;
	status = netlib_read(ctx->spec_socket, &ior->rdsc, 0, 0,
			     0, timeout, &ior->iosb, io_perform, ior);
	if (!OK(status)) {
	    if (ior->iosbp != 0) {
		ior->iosbp->iosb_w_status = status;
		ior->iosbp->iosb_w_count = 0;
		ior->iosbp->iosb_l_unsused = 0;
	    }
	    if (ior->spec_argv != 0) free(ior->spec_argv);
	    if (ior->astadr != 0) (*(ior->astadr))(ior->astprm);
	    FREE_IOR(ior);
	}
	break;

    case SSL_ERROR_WANT_WRITE:
	ret = BIO_read(ctx->wbio, b, l);
	if (ret < 1) {
	    // houston we have a problem
	} else {
	    struct dsc$descriptor buf;

	    status = netlib_write(ctx->spec_socket, &buf, 0, 0, &ior->iosb,
				  io_perform, ior);
	    if (!OK(status)) {
	    	// error, this needs to be passed to iosbp...
	    }
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
#endif

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

/*

Translate SSL status codes to OpenVMS ones...

Should we store these somewhere and provide an interface to fetch the
full status via a netlib_ssl_xxx routine?  This way you could find out what
was really going on.  Although, we still need to document the kind of
errors that occur when processing the errors...

*/
int netlib___cvt_status(int err,
			...) {

    va_list argptr;
    int argc;
    int _errno = EVMSERR, _vaxc$errno = SS$_NORMAL;
    int lib, func, reason, status;

    SETARGCOUNT(argc);

    if (argc > 1) {
	va_start(argptr, err);
	_errno = va_arg(argptr, int);
	if (argc > 2) _vaxc$errno = va_arg(argptr, int);
	va_end(argptr);
    }

    lib = ERR_GET_LIB(err);
    func = ERR_GET_FUNC(err);
    reason = ERR_GET_REASON(err);

    switch (lib) {
    case ERR_LIB_SYS:
	status = _vaxc$errno;
	break;

    case ERR_LIB_X509:
	switch (reason) {
	case X509_R_INVALID_DIRECTORY:
	    status = RMS$_DNF;
	    break;
	case X509_R_KEY_TYPE_MISMATCH:
	case X509_R_KEY_VALUES_MISMATCH:
	    status = RMS$_KEY_MISMATCH;
	    break;
	case X509_R_UNSUPPORTED_ALGORITHM:
	    status = SS$_UNSUPPORTED;
	    break;
	default:
	    status = SS$_BADCHECKSUM;
	    break;
	}
	break;
    }

    return status;
}
