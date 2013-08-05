/*[[ CMS REPLACEMENT HISTORY, Element NETLIB.H]]*/
/*[[ *2     7-NOV-2004 15:37:57 MADISON ""]]*/
/*[[ *1    12-MAR-1998 16:41:05 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element NETLIB.H]]*/
#ifndef NETLIB_H_LOADED
#define NETLIB_H_LOADED
/*
** NETLIB.H
**
**  Main #include file for NETLIB.
**
**  COPYRIGHT © 1993,1997,2004 MADGOAT SOFTWARE.  ALL RIGHTS RESERVED.
**
**  MODIFICATION HISTORY:
**
**  29-Sep-1993	    Madison 	Initial commenting.
**  31-May-1997	    Madison 	Fix queue access.
**  07-Nov-2004     Madison     IA64 support.
**  25-Jul-2013     Sneddon     Swap varargs.h for stdarg.h.
**  29-Jul-2013	    Sneddon	Moved *BLOCK_ASTS in here.
**  30-Jul-2013     Sneddon	GET_IOR now initializes the internal IOSB.
**  02-Aug-2013	    Sneddon	Moved queue definitions into quedef.h
**  05-Aug-2013	    Sneddon	Moved INIT_QUEUE into quedef.h
*/
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <socket.h>
#include <in.h>
#include <netdb.h>
#include <descrip.h>
#include <stsdef.h>
#include <ssdef.h>
#include <lib$routines.h>
#include <str$routines.h>
#include <starlet.h>
#ifdef __DECC
#include <builtins.h>
#else
#pragma builtins
#endif
#ifndef __NETLIB_BUILD__
#define __NETLIB_BUILD__
#endif
#include "netlibdef.h"
#include "quedef.h"

/*
** Handy macros
*/
#define OK(s) $VMS_STATUS_SUCCESS(s)
#define INIT_DYNDSCPTR(str) {str->dsc$w_length = 0; str->dsc$a_pointer = (void *) 0;\
    	    str->dsc$b_class = DSC$K_CLASS_D; str->dsc$b_dtype = DSC$K_DTYPE_T;}
#define INIT_DYNDESC(str) {str.dsc$w_length = 0; str.dsc$a_pointer = (void *) 0;\
    	    str.dsc$b_class = DSC$K_CLASS_D; str.dsc$b_dtype = DSC$K_DTYPE_T;}
#define INIT_SDESC(str,len,ptr) {str.dsc$w_length=(len);str.dsc$a_pointer=(void *)(ptr);\
    	    str.dsc$b_class=DSC$K_CLASS_S; str.dsc$b_dtype=DSC$K_DTYPE_T;}
#define ITMLST_INIT(itm,c,s,a,r) {itm.bufsiz=(s); itm.itmcod=(c);\
    	    itm.bufadr=(a); itm.retlen=(r);}
#define SETARGCOUNT(x) va_count(x)
#define VERIFY_CTX(x,c) {if ((x) == 0) return SS$_BADPARAM; c = *x;}
#define GET_IOR(x, _ctx, _iosb, _astadr, _astprm) do {unsigned int status;\
    	    status = netlib___alloc_ior(&x);\
    	    if (!OK(status)) return status; (x)->ctx = (_ctx);\
	    (x)->iosb.iosb_w_status = SS$_NORMAL; (x)->iosb.iosb_w_count = 0;\
    	    (x)->iosbp=(_iosb); (x)->astadr=(_astadr); (x)->astprm=(_astprm);\
	    } while (0)
#define FREE_IOR(x) netlib___free_ior(x);
#define GET_DNSREQ(x, _ctx, _iosb, _astadr, _astprm) {unsigned int status;\
    	    status = netlib___alloc_dnsreq(&x);\
    	    if (!OK(status)) return status;\
    	    status = netlib___alloc_ior(&((x)->ior));\
    	    if (!OK(status)) {netlib___free_dnsreq(x); return status;}\
    	    (x)->ior->ctx = (_ctx);\
    	    (x)->ior->iosbp=(_iosb); (x)->ior->astadr=(_astadr);\
    	    (x)->ior->astprm=(_astprm);}
#define FREE_DNSREQ(x) {netlib___free_ior(((x)->ior));netlib___free_dnsreq(x);}

/*
**  Generic context structure
*/

#ifndef __SPECCTX
#define __SPECCTX void
#endif

    struct NAMESERVER {
    	struct NAMESERVER *flink, *blink;
    	struct INADDRDEF addr;
    };

    struct DOMAIN {
    	struct DOMAIN *flink, *blink;
    	int length;
    	char name[1];
    };

    struct DNSCTX {
    	QUEUE nsq;
    	QUEUE domq;
    	TIME  timeout;
    	int   retry_count;
    	unsigned int flags;
    	unsigned short queryid;
    };

#define CTX_S_LINEBUF	32768

    struct CTX {
    	struct CTX *flink, *blink;

    	TIME exptime;

    	unsigned int flags;
#define CTX_M_USER_SET_REUSEADDR    (1<<0)
#define CTX_M_LINE_FOUND_CR 	    (1<<1)
#define CTX_M_NO_DNS	    	    (1<<2)
    	unsigned short chan;
    	__SPECCTX *specctx;
    	unsigned int specctx_size;

    	unsigned char *linebuf, *linebufp, *lineanchor;
    	struct dsc$descriptor *line_dsc;
    	unsigned short *line_retlen;
    	unsigned int line_flags;
    	int line_remain;
    	TIME *line_tmo;

    	unsigned char *wlinebuf;
    	int wlinesize;

    	struct DNSCTX *dnsctx;

    };

#define SPECIOR_SPACE 64
#ifndef __SPECIOR
#define __SPECIOR void *
#endif

#pragma nostandard
    struct IOR {
    	struct IOR *flink, *blink;
    	struct NETLIBIOSBDEF iosb;
    	TIME   timeout;
    	struct CTX *ctx;
    	struct NETLIBIOSBDEF *iosbp;
    	unsigned int iorflags;
#define IOR_M_IO_COMPLETED  	    (1<<0)
#define IOR_M_IO_TIMED	    	    (1<<1)
#define IOR_M_IO_TIMEOUT    	    (1<<2)
    	void (*astadr)();
    	void *astprm;
    	union {
    	    void *address;
    	    unsigned int longword;
    	    unsigned short word;
    	} arg[8];
    	variant_union {
    	    unsigned char specior_space[SPECIOR_SPACE];
    	    __SPECIOR specior;
    	} specior_overlay;
    };
#pragma standard

    struct DNSREQ {
    	struct DNSREQ *flink, *blink;
    	struct IOR *ior;
    	struct CTX *ctx;
    	struct NAMESERVER *curns;
    	struct DOMAIN *curdom;
    	char *query_name;
    	unsigned short query_namlen;
    	unsigned int query_flags;
    	unsigned char *query_rbuf;
    	unsigned int query_rbufsize;
    	unsigned int query_class;
    	unsigned int query_type;
    	unsigned int buflen, replylen;
    	int retries;
    	struct SINDEF sin;
    	struct SINDEF rsin;
    	unsigned char buf[1024];
    };

#ifndef __NETLIB_MODULE_MEM__
#pragma nostandard
    globalref unsigned int netlib_synch_efn;
    globalref unsigned int netlib_asynch_efn;
#pragma standard
    unsigned int netlib___alloc_ctx(struct CTX **, unsigned int);
    unsigned int netlib___free_ctx(struct CTX *);
    unsigned int netlib___alloc_ior(struct IOR **);
    unsigned int netlib___free_ior(struct IOR *);
    unsigned int netlib___alloc_dnsreq(struct DNSREQ **);
    unsigned int netlib___free_dnsreq(struct DNSREQ *);
#endif

#define BLOCK_ASTS(stat_) do { \
        if (lib$ast_in_prog()) \
            (stat_) = 0; \
        else \
            (stat_) = sys$setast(0); \
        } while (0)

#define UNBLOCK_ASTS(stat_) do { \
        if ((stat_) == SS$_WASSET) \
            sys$setast(1); \
        } while (0)

#endif /* NETLIB_H_LOADED */
