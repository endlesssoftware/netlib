#ifndef __NETLIB_SSL_H__
#define __NETLIB_SSL_H__
#ifndef __NETLIB_BUILD__
#define __NETLIB_BUILD__
#endif
#include <openssl/bio.h>
#include <openssl/ssl.h>
#include "netlibdef.h"

    struct SPECCTX {
	void *socket;
	SSL *ssl;
	BIO *rbio, *wbio;
    };
#define __SPECCTX struct SPECCTX
#define SPECCTX_SIZE sizeof(__SPECCTX)

    struct SPECIOR {
    	unsigned short fromlen;
    };
#define __SPECIOR struct SPECIOR

#endif /* __NETLIB_SSL_H__ */
