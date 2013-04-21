#ifndef __NETLIB_SSL_H__
#define __NETLIB_SSL_H__
#include "netlibdef.h"
#ifndef __NETLIB_BUILD__
#define __NETLIB_BUILD__
#endif
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <openssl/bio.h>
#include <openssl/err.h>
#include <openssl/ssl.h>

#define BUF_MAX  16384

/*
** Socket-level SSL specific fields.
*/
#define spec_socket	specctx->socket
#define spec_ssl	specctx->ssl
#define spec_rbio	specctx->rbio
#define spec_wbio	specctx->wbio

    struct SPECCTX {
	void *socket;
	SSL *ssl;
	BIO *rbio, *wbio;
	struct {
	    char buf[BUF_MAX], *ptr;
	    int alloc, len;
	} rbuf, wbuf;
    };
#define __SPECCTX struct SPECCTX
#define SPECCTX_SIZE sizeof(__SPECCTX)

/*
** I/O-level SSL specific fields.
*/
#define spec_call	arg[0].address
#define spec_argv	arg[1].address
#define spec_bptr	arg[2].address
#define spec_blen	arg[3].longword

    struct SPECIOR {
    	unsigned short fromlen;
    };
#define __SPECIOR struct SPECIOR

#endif /* __NETLIB_SSL_H__ */
