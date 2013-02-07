#ifndef __NETLIB_SSL_H__
#define __NETLIB_SSL_H__
#ifndef __NETLIB_BUILD__
#define __NETLIB_BUILD__
#endif
#include "netlibdef.h"

#define IOR_M_COPY_LENGTH  	(1<<16)
#define IOR_M_COPY_FROM    	(1<<17)
#define	IOR_M_NEW_CONTEXT  	(1<<18)
#define IOR_M_COPY_ADDRS   	(1<<19)
#define IOR_M_COPY_HOSTNAME	(1<<20)

#define __SPECCTX void
#define SPECCTX_SIZE 0

    struct SPECIOR {
    	unsigned short fromlen;
    	unsigned int subfunction;
    	struct SINDEF from;
    	ITMLST2 sockopt;
    	struct {
    	    unsigned short bufsiz, itmcod;
    	    void *bufadr, *retlen;
    	} sockopt_get;
    };
#define __SPECIOR struct SPECIOR

#endif /* __NETLIB_SSL_H__ */
