/*
**++
**  FACILITY:	NETLIB
**
**  ABSTRACT:	Routines specific to OpenSSL.
**
**  MODULE DESCRIPTION:
**
**  	This module contains the SSL implementation for NETLIB, including
**  AST support for OpenSSL.
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

    unsigned int netlib_ssl_context (void **xssl, unsigned int *method,
                                     struct dsc$descriptor *cert_d,
				     int *cert_type,
                                     struct dsc$descriptor *key_d,
				     int *key_type, unsigned int *verify);
    unsigned int netlib_ssl_socket(struct CTX **xctx, void **xsocket,
                                   void **xssl);
    unsigned int netlib_ssl_accept(struct CTX **xctx, TIME *timeout,
				   struct NETLIBIOSBDEF *iosb,
			           void (*astadr)(), void *astprm);
    unsigned int netlib_ssl_connect(struct CTX **xctx, TIME *timeout,
				    struct NETLIBIOSBDEF *iosb,
			            void (*astadr)(), void *astprm);
    unsigned int netlib_ssl_shutdown (struct CTX **xctx,
                                      struct NETLIBIOSBDEF *iosb,
                                      void (*astadr)(), void *astprm);
    unsigned int netlib_ssl_read (struct CTX **xctx,
				  struct dsc$descriptor *dsc, TIME *timeout,
				  struct NETLIBIOSBDEF *iosb,
				  void (*astadr)(), void *astprm);
    unsigned int netlib_ssl_write (struct CTX **xctx,
				   struct dsc$descriptor *dsc, TIME *timeout,
				   struct NETLIBIOSBDEF *iosb,
                                   void (*astadr)(), void *astprm);
    static unsigned int io_queue(struct IOR *IOR);
    static unsigned int io_start(struct IOR *IOR);
    static unsigned int io_perform(struct IOR *IOR);
    static unsigned int io_read(struct IOR *ior);
    static unsigned int io_write(struct IOR *ior);
    static long outbio_callback(BIO *b, int oper, const char *argp, int argi,
				long argl, long retvalue);
    int netlib___cvt_status(int err, ...);

    /*
    ** These functions are needed by the DNS module
    */
    int netlib___get_nameservers(QUEUE *nsq) { return 0; }
    int netlib___get_domain(char *buf, unsigned short bufsize,
			    unsigned short *relenp) { return 0; }

/*
**  External functions
*/

    extern unsigned int netlib_read(), netlib_write();

/*
**  OWN storage
*/

    volatile unsigned netlib_ssl_efn = 0xffffffff;


#define netlib___cvt_iosb(_dst, _src) {\
            (_dst)->iosb_w_status = (_src)->iosb_w_status;\
            (_dst)->iosb_w_count =  (_src)->iosb_w_count;\
            (_dst)->iosb_l_unused = 0;}

	// the above macro needs to include the ior
	// that way we can say what we wrote out...in trems of the
	// buffer...


/*
**
**  SS$_INSFMEM - unable to allocate internal SSL structures
**  SS$_BADCHECKSUM - key and cert do not match
*/
unsigned int netlib_ssl_context (void **xssl, unsigned int *method,
				 struct dsc$descriptor *cert_d, int *cert_type,
				 struct dsc$descriptor *key_d, int *key_type,
				 unsigned int *verify) {

    SSL_CTX *ssl;
    int argc, ret, status = SS$_NORMAL;
    char *cert = 0, *key = 0, *ptr;
    unsigned short len;

printf("netlib_ssl_context\n");
    SETARGCOUNT(argc);
SSL_library_init();
SSL_load_error_strings();
    ssl = SSL_CTX_new((*method == NETLIB_K_METHOD_SSL2) ? SSLv2_method() :
		      (*method == NETLIB_K_METHOD_SSL3) ? SSLv3_method() :
		      (*method == NETLIB_K_METHOD_TLS1) ? TLSv1_method() :
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

    // if status OK and verify location path was supplied
	// SSL_CTX_load_verify_locations
	// if works
	    // SSL_CTX_set_erify(ssl, SSL_VERIFY_PEER, 0);
	    // SSL_CT_set_verify_depth(ssl, 1);
	// else
	    // RMS$_DNF?

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
    unsigned int aststat, status;

printf("netlib_ssl_socket\n");
    SETARGCOUNT(argc);
    if (argc < 3) return SS$_INSFARG;
    if (xsocket == 0 || xssl == 0) return SS$_BADPARAM;
    if (*xsocket == 0 || *xssl == 0) return SS$_BADPARAM;

    BLOCK_ASTS(aststat);
    if (netlib_ssl_efn == 0xffffffff) {
    	status = lib$get_ef(&netlib_ssl_efn);
    	if (!OK(status)) {
    	    UNBLOCK_ASTS(aststat);
    	    return status;
    	}
    }
    UNBLOCK_ASTS(aststat);

    status = netlib___alloc_ctx(&ctx, SPECCTX_SIZE);
    if (!OK(status)) return status;

    ctx->spec_socket = *xsocket;
    INIT_QUEUE(ctx->spec_iorque);

    status = SS$_INSFMEM;
    if ((ctx->spec_inbio = BIO_new(BIO_s_mem())) != 0) {
	if ((ctx->spec_outbio = BIO_new(BIO_s_mem())) != 0) {
	    BIO_set_callback(ctx->spec_outbio, outbio_callback);
	    BIO_set_callback_arg(ctx->spec_outbio, ctx);
	    if ((ctx->spec_ssl = SSL_new(*xssl)) != 0) {
	    	SSL_set_bio(ctx->spec_ssl, ctx->spec_inbio, ctx->spec_outbio);

	    	ctx->spec_data.dsc$w_length = 0;
	    	ctx->spec_data.dsc$b_dtype = DSC$K_DTYPE_T;
	    	ctx->spec_data.dsc$b_class = DSC$K_CLASS_S;
	    	ctx->spec_data.dsc$a_pointer = malloc(BUF_MAX);
	    	if (ctx->spec_data.dsc$a_pointer != 0) {
		    status = SS$_NORMAL;
	    	}
	    } else {
	    	status = SS$_BADCONTEXT;
	    }
	}
    }

    if (!OK(status)) {
	if (ctx->spec_inbio != 0) BIO_free(ctx->spec_inbio);
	if (ctx->spec_outbio != 0) BIO_free(ctx->spec_outbio);
	if (ctx->spec_ssl != 0) SSL_free(ctx->spec_ssl);
	if (ctx->spec_data.dsc$a_pointer != 0)
	    free(ctx->spec_data.dsc$a_pointer);
	netlib___free_ctx(ctx);
    } else {
	*xctx = ctx;
    }
    return status;
} /* netlib_ssl_socket */

/*
**++
**  ROUTINE:	netlib_ssl_accept
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
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
unsigned int netlib_ssl_accept (struct CTX **xctx, TIME *timeout,
			        struct NETLIBIOSBDEF *iosb,
			        void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    struct IOR *ior;
    unsigned int status;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 1) return SS$_INSFARG;

    GET_IOR(ior, ctx, (argc > 2) ? iosb : 0, (argc > 3) ? astadr : 0,
	    (argc > 4) ? astprm : 0);
    ior->spec_argc = 1;
    ior->spec_argv(0).address = ctx->spec_ssl;
    ior->spec_call = SSL_accept;
    status = io_queue(ior);

    if (ior->astadr == 0) {
printf("  waiting!\n");
	sys$waitfr(netlib_ssl_efn);
    }

    return status;

} /* netlib_ssl_accept */

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
unsigned int netlib_ssl_connect (struct CTX **xctx, TIME *timeout,
			         struct NETLIBIOSBDEF *iosb,
			         void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    struct IOR *ior;
    unsigned int status;
    int argc;

printf("netlib_ssl_connect\n");
    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 1) return SS$_INSFARG;

    GET_IOR(ior, ctx, (argc > 2) ? iosb : 0, (argc > 3) ? astadr : 0,
	    (argc > 4) ? astprm : 0);
    ior->spec_argc = 1;
    ior->spec_argv(0).address = ctx->spec_ssl;
    ior->spec_call = SSL_connect;
    status = io_queue(ior);

    if (ior->astadr == 0) {
printf("  waiting!\n");
	sys$waitfr(netlib_ssl_efn);
    }

    return status;

} /* netlib_ssl_connect */

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
    struct IOR *ior;
    unsigned int status;
    int argc;

printf("netlib_ssl_shutdown\n");
    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 1) return SS$_INSFARG;

    GET_IOR(ior, ctx, (argc > 1) ? iosb : 0, (argc > 2) ? astadr : 0,
	    (argc > 3) ? astprm : 0);
    ior->spec_argc = 1;
    ior->spec_argv(0).address = ctx->spec_ssl;
    ior->spec_call = SSL_shutdown;
    status = io_queue(ior);

    if (ior->astadr == 0) {
printf("  waiting!\n");
	sys$waitfr(netlib_ssl_efn);
    }

    return status;
} /* netlib_ssl_shutdown */

/*
**++
**  ROUTINE:	netlib_ssl_read
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
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
unsigned int netlib_ssl_read (struct CTX **xctx, struct dsc$descriptor *dsc,
			      TIME *timeout, struct NETLIBIOSBDEF *iosb,
			      void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    struct IOR *ior;
    unsigned int status;
    int argc;

printf("netlib_ssl_read\n");
    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 1) return SS$_INSFARG;

    if (dsc->dsc$b_dtype != DSC$K_DTYPE_T && dsc->dsc$b_dtype != 0)
        return SS$_BADPARAM;
    if (dsc->dsc$b_class == DSC$K_CLASS_D) {
        if (dsc->dsc$w_length == 0) return SS$_BADPARAM;
    } else {
        if (dsc->dsc$b_class != DSC$K_CLASS_S && dsc->dsc$b_class != 0)
        return SS$_BADPARAM;
    }

    GET_IOR(ior, ctx, (argc > 3) ? iosb : 0, (argc > 4) ? astadr : 0,
	    (argc > 5) ? astprm : 0);
    ior->spec_argc = 1;
    ior->spec_argv(0).address = ctx->spec_ssl;
    ior->spec_argv(1).address = dsc->dsc$a_pointer;
    ior->spec_argv(2).longword = dsc->dsc$w_length;
    ior->spec_call = SSL_read;
    status = io_queue(ior);

    if (ior->astadr == 0) {
printf("  waiting!\n");
	sys$waitfr(netlib_ssl_efn);
    }

    return status;
} /* netlib_ssl_read */

/*
**++
**  ROUTINE:	netlib_ssl_write
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
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
unsigned int netlib_ssl_write (struct CTX **xctx, struct dsc$descriptor *dsc,
			       TIME *timeout, struct NETLIBIOSBDEF *iosb,
			       void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    struct IOR *ior;
    void *bufptr;
    unsigned short buflen;
    unsigned int status;
    int argc;

printf("netlib_ssl_write\n");
    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 1) return SS$_INSFARG;

    status = lib$analyze_sdesc(dsc, &buflen, &bufptr);
    if (!OK(status)) return status;

    GET_IOR(ior, ctx, (argc > 3) ? iosb : 0, (argc > 4) ? astadr : 0,
	    (argc > 5) ? astprm : 0);

    ior->spec_argc = 1;
    ior->spec_argv(0).address = ctx->spec_ssl;
    ior->spec_argv(1).address = bufptr;
    ior->spec_argv(2).longword = buflen;
    ior->spec_call = SSL_write;
    status = io_queue(ior);

    if (ior->astadr == 0) {
printf("  waiting!\n");
	sys$waitfr(netlib_ssl_efn);
    }

    return status;
} /* netlib_ssl_write */

static unsigned int io_queue (struct IOR *ior) {

    int aststat, status = SS$_NORMAL;
    struct CTX *ctx = ior->ctx;

printf("io_queue [%p]\n", ior->spec_call);

    /*
    ** Block ASTs and load the IOR into the IOR queue
    ** for this socket.
    */
    BLOCK_ASTS(aststat);
    queue_insert(ior, ctx->spec_iorque.tail);
printf("  iorque.head=%p,iorque.tail=%p\n",ctx->spec_iorque.head,ctx->spec_iorque.head);
    if (queue_single(ctx->spec_iorque)) {
	/*
	** The IOR we just inserted is the only one in
	** the queue, so fire up the IO handler.
	*/
	status = sys$dclast(io_start, ior, 0);
    }
    UNBLOCK_ASTS(aststat);

    return status;
}

static unsigned int io_start (struct IOR *ior) {

    struct CTX *ctx = ior->ctx;

    if (ior->astadr == 0) sys$clref(netlib_ssl_efn);
    ior->iosb.iosb_w_status = SS$_NORMAL;

    sys$dclast(io_perform, ior, 0);

    return SS$_NORMAL;
} /* io_start */

static unsigned int io_perform (struct IOR *ior) {

    int ret, status;
    struct CTX *ctx = ior->ctx;

printf("io_perform [%p]; status=%d\n", ior->spec_call,ior->iosb.iosb_w_status);
    if (OK(ior->iosb.iosb_w_status)) {
	/*
	** Execute the SSL call and retrieve the actual error.
	*/
    	ret = lib$callg(ior->arg, ior->spec_call);
    	status = SSL_get_error(ctx->spec_ssl, ret);
printf("  SSL ret=%d,status=%d\n",ret,status);
	switch (status) {
	default:
	    // something bad happened in here...
	    break;

	case SSL_ERROR_NONE:
	    status = ior->iosb.iosb_w_status = SS$_NORMAL;
	    ior->iosb.iosb_w_count = ret;
	    break;

	case SSL_ERROR_WANT_READ:
	    /*
	    ** The SSL software wants more input, so queue the real
	    ** I/O.  The data will be copied into the BIO and the SSL
	    ** call requeued by the io_read AST.
	    */
	    status = netlib_read(&ctx->spec_socket, &ctx->spec_data, 0, 0,
			     	 0, 0, &ior->iosb, io_read, ior);
printf("netlib_read=%d\n",status);
	    if (OK(status)) return SS$_NORMAL;
	    break;

	case SSL_ERROR_WANT_WRITE:
	    /*
	    ** The SSL software wants us to write out the buffer.
	    */
	    status = sys$dclast(io_write, ior, 0);
	    if (OK(status)) return SS$_NORMAL;
	    break;
	}
	ior->iosb.iosb_w_status = status;
    }

    /*
    ** If the SSL call required more I/O, then (assuming it was a
    ** successful call) the code will return immediately after
    ** queueing the I/O.  We only get down here if there was an
    ** error of the SSL routine completed without requiring further
    ** assistance.
    **
    **
    ** Copy the IOSB, if supplied and call any supplied AST routines.
    */
    if (ior->iosbp != 0) netlib___cvt_iosb(ior->iosbp, &ior->iosb);
    if (ior->astadr != 0) {
	printf("  call AST routine\n");
	((ior->astadr))(ior->astprm);
    } else {
	printf("  set ef %d\n", netlib_ssl_efn);
	sys$setef(netlib_ssl_efn);
    }

    /*
    ** Clean up the IOR.  Check if we've got another and queue it, if we do.
    */
    queue_remove(ior, &ior);
    FREE_IOR(ior);
    if (!queue_empty(ctx->spec_iorque)) {
	status = sys$dclast(io_start, ctx->spec_iorque.head, 0);
    }

    return SS$_NORMAL;
} /* io_perform */

static unsigned int io_read (struct IOR *ior) {

    char *ptr;
    int size, status;
    struct CTX *ctx = ior->ctx;

printf("io_read\n");
    if (OK(ior->iosb.iosb_w_status)) {
	status = BIO_write(ctx->spec_inbio, ctx->spec_data.dsc$a_pointer,
			   ior->iosb.iosb_w_count);
	if (status > 0) {
	    status = SS$_NORMAL;
	} else {
	    /*
	    ** According to the SSL documentation the only thing that
	    ** can cause a BIO_s_mem to fail is a lack of VM.
	    */
	    status = SS$_INSFMEM;
        }
	ior->iosb.iosb_w_status = status;
printf("  status=%d\n", status);
    }
printf("  iosb=%d\n", ior->iosb.iosb_w_status);
    sys$dclast(io_perform, ior, 0);

    return SS$_NORMAL;
} /* io_read */

static unsigned int io_write (struct IOR *ior) {

    int ret, status;
    struct CTX *ctx = ior->ctx;

printf("io_write\n");
    if (OK(ior->iosb.iosb_w_status)) {
	/*
	** Read the output BIO and if we found anything, queue the I/O
	** and requeue ourselves as the completion AST (in case the BIO
	** held more than a descriptors-worth.
	*/
    	ret = BIO_read(ctx->spec_outbio, ctx->spec_data.dsc$a_pointer,
		       BUF_MAX);
	if (ret > 0) {
	    ctx->spec_data.dsc$w_length = ret;
	    status = netlib_write(&ctx->spec_socket, &ctx->spec_data,
				  0, 0, &ior->iosb, io_write, ior);
	    if (OK(status)) return SS$_NORMAL;
	} else {
	    ctx->spec_flags |= IOR_M_COMPLETE;
	}
    }

    sys$dclast(io_perform, ior, 0);

    return SS$_NORMAL;
} /* io_write */

static long outbio_callback (BIO *b, int oper, const char *argp, int argi,
			     long argl, long retvalue) {

    struct CTX *ctx = (struct CTX *)BIO_get_callback_arg(b);

    switch (oper) {
    default:
	break;

    case BIO_CB_WRITE|BIO_CB_RETURN:
	if (ctx->spec_flags & IOR_M_COMPLETE) {
	    ctx->spec_flags &= ~IOR_M_COMPLETE;
	    BIO_reset(b);
	} else {
	    BIO_set_retry_write(b);
	    retvalue = -1;
	}
	break;

    }

    return retvalue;
} /* outbio_callback */

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
