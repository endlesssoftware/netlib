#ifndef __NETLIB_SSL_H__
#define __NETLIB_SSL_H__
#ifndef __NETLIB_BUILD__
#define __NETLIB_BUILD__
#endif
#include <openssl/bio.h>
#include <openssl/ssl.h>
#include "netlibdef.h"

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
    };
#define __SPECCTX struct SPECCTX
#define SPECCTX_SIZE sizeof(__SPECCTX)

/*
** I/O-level SSL specific fields.
*/
#define spec_call	arg[0].address
#define spec_argv	arg[1].address

    struct SPECIOR {
    	unsigned short fromlen;
    };
#define __SPECIOR struct SPECIOR

#endif /* __NETLIB_SSL_H__ */
