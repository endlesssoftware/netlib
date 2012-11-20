/*[[ CMS REPLACEMENT HISTORY, Element NETLIB_UCX.H]]*/
/*[[ *1    12-MAR-1998 16:41:09 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element NETLIB_UCX.H]]*/
#ifndef __NETLIB_UCX_H__
#define __NETLIB_UCX_H__
#ifndef __NETLIB_BUILD__
#define __NETLIB_BUILD__
#endif
#include "netlibdef.h"
#include <iodef.h>
#include "ucx_inetdef.h"

    typedef struct item_list_2 {
    	unsigned short bufsiz, itmcod;
    	void *bufadr;
    } ITMLST2;

#define ITMLST2_INIT(_i, _c, _s, _a) {\
    _i.bufsiz = (_s); _i.itmcod = (_c); _i.bufadr = (_a);}

#define spec_retlen 	    	arg[0].address
#define spec_length 	    	arg[1].word
#define spec_userfrom	    	arg[2].address
#define spec_newctx    	    	arg[3].address
#define spec_xnewctx	    	arg[4].address
#define spec_hostent	    	arg[4].address
#define spec_useralist	    	arg[5].address
#define spec_usrdsc 	    	arg[5].address

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

    struct HOSTENT {
    	unsigned int name_offset;
    	unsigned int alias_list_offset;
    	unsigned int addrtype;
    	unsigned int addrlen;
    	unsigned int addrlist_offset;
    	unsigned char buffer[492];
    };

#endif /* __NETLIB_UCX_H__ */
