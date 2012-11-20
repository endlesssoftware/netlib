/*[[ CMS REPLACEMENT HISTORY, Element MEM.C]]*/
/*[[ *2    25-NOV-2000 10:35:52 MADISON "V2.2G updates"]]*/
/*[[ *1    12-MAR-1998 16:41:00 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element MEM.C]]*/
/*
**++
**  FACILITY:	NETLIB
**
**  ABSTRACT:	Memory management routines
**
**  MODULE DESCRIPTION:
**
**  	tbs
**
**  AUTHOR: 	    M. Madison
**  	    	    COPYRIGHT © 1994,2000 MADGOAT SOFTWARE.
**  	    	    ALL RIGHTS RESERVED.
**
**  CREATION DATE:  02-APR-1994
**
**  MODIFICATION HISTORY:
**
**  	02-APR-1994 V1.0    Madison 	Initial coding.
**  	09-NOV-1994 V1.1    Madison 	Add IOR, DNSREQ.
**  	31-AUG-1995 V1.1-1  Madison 	Update free_ctx() to free wlinebuf.
**  	05-FEB-2000 V1.1-2  Madison 	Improve AST/non-AST synchronization.
**--
*/
#define __NETLIB_MODULE_MEM__
#include "netlib.h"
#include <libvmdef.h>
/*
** Forward declarations
*/
    unsigned int netlib___alloc_ctx(struct CTX **, unsigned int);
    unsigned int netlib___free_ctx(struct CTX *);
    unsigned int netlib___alloc_ior(struct IOR **);
    unsigned int netlib___free_ior(struct IOR *);
    unsigned int netlib___alloc_dnsreq(struct DNSREQ **);
    unsigned int netlib___free_dnsreq(struct DNSREQ *);
/*
** OWN and GLOBAL storage
*/
    static volatile unsigned int ctxzone = 0;
    static const unsigned int CTX_S_CTXDEF = sizeof(struct CTX);
    static volatile unsigned int iorzone = 0;
    static const int IOR_S_IORDEF = sizeof(struct IOR);
    static volatile int dnsreqzone = 0;
    static const unsigned int DNSREQ_S_DNSREQDEF = sizeof(struct DNSREQ);
#pragma nostandard
    globaldef volatile unsigned int netlib_synch_efn = 0xffffffff;
    globaldef volatile unsigned int netlib_asynch_efn = 0xffffffff;
#pragma standard
    static volatile QUEUE iorque = {(void *) &iorque, (void *) &iorque};
    static volatile QUEUE dnsreqque = {(void *) &dnsreqque, (void *) &dnsreqque};

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

/*
**  External references
*/
    void netlib___free_dns_context(struct CTX *);


/*
**++
**  ROUTINE:	netlib___alloc_ctx
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Allocates some memory.
**
**  RETURNS:	cond_value
**
**  PROTOTYPE:
**
**  	netlib___alloc_ctx(unsigned int size, void *ptr)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	None.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib___alloc_ctx (struct CTX **ctxp, unsigned int specsize) {

    struct CTX *ctx;
    unsigned int status, fullsize, aststat;

    BLOCK_ASTS(aststat);
    if (netlib_synch_efn == 0xffffffff) {
    	status = lib$get_ef(&netlib_synch_efn);
    	if (!OK(status)) {
    	    UNBLOCK_ASTS(aststat);
    	    return status;
    	}
    }
    if (netlib_asynch_efn == 0xffffffff) {
    	status = lib$get_ef(&netlib_asynch_efn);
    	if (!OK(status)) {
    	    UNBLOCK_ASTS(aststat);
    	    return status;
    	}
    }

    fullsize = specsize + CTX_S_CTXDEF;
    if (ctxzone == 0) {
    	unsigned int algorithm=LIB$K_VM_FIXED;
    	unsigned int flags=LIB$M_VM_GET_FILL0|LIB$M_VM_EXTEND_AREA;
    	status = lib$create_vm_zone(&ctxzone, &algorithm, &fullsize, &flags);
    	if (!OK(status)) {
    	    UNBLOCK_ASTS(aststat);
    	    return status;
    	}
    }

    status = lib$get_vm(&fullsize, &ctx, &ctxzone);
    if (OK(status)) {
    	ctx->specctx = ctx + 1;
    	ctx->specctx_size = specsize;
    	if (!OK(status)) lib$free_vm(&fullsize, &ctx, &ctxzone);
    	*ctxp = ctx;

    }

    UNBLOCK_ASTS(aststat);

    return status;

} /* netlib___alloc_ctx */

/*
**++
**  ROUTINE:	netlib___free_ctx
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Frees a block of memory.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	netlib___free_ctx(struct CMD *c)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	None.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib___free_ctx (struct CTX *ctx) {

    unsigned int fullsize;

    if (ctx->linebuf != 0) {
    	fullsize = CTX_S_LINEBUF;
    	lib$free_vm(&fullsize, &ctx->linebuf);
    }

    if (ctx->wlinebuf != 0 && ctx->wlinesize != 0) {
    	lib$free_vm(&ctx->wlinesize, &ctx->wlinebuf);
    }

    netlib___free_dns_context(ctx);

    fullsize = ctx->specctx_size + CTX_S_CTXDEF;
    return lib$free_vm(&fullsize, &ctx, &ctxzone);

} /* netlib___free_ctx */

/*
**++
**  ROUTINE:	netlib___alloc_ior
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Allocates some memory.
**
**  RETURNS:	cond_value
**
**  PROTOTYPE:
**
**  	netlib___alloc_ior(unsigned int size, void *ptr)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	None.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib___alloc_ior (struct IOR **iorp) {

    struct IOR *ior;
    unsigned int status, fullsize, aststat;

    BLOCK_ASTS(aststat);

    if (iorzone == 0) {
    	unsigned int algorithm=LIB$K_VM_FIXED;
    	unsigned int flags=LIB$M_VM_GET_FILL0|LIB$M_VM_EXTEND_AREA;
    	int i;
    	status = lib$create_vm_zone(&iorzone, &algorithm, &IOR_S_IORDEF, &flags);
    	if (!OK(status)) {
    	    UNBLOCK_ASTS(aststat);
    	    return status;
    	}
    	for (i = 0; i < 8; i++) {
    	    if (!OK(lib$get_vm(&IOR_S_IORDEF, &ior))) break;
    	    queue_insert(ior, iorque.tail);
    	}
    }

    if (queue_remove(iorque.head, &ior)) {
    	memset(ior, 0, IOR_S_IORDEF);
    	*iorp = ior;
    	status = SS$_NORMAL;
    } else 
    	status = lib$get_vm(&IOR_S_IORDEF, &ior, &iorzone);

    if (OK(status))
    	*iorp = ior;

    UNBLOCK_ASTS(aststat);

    return status;

} /* netlib___alloc_ior */

/*
**++
**  ROUTINE:	netlib___free_ior
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Frees a block of memory.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	netlib___free_ior(struct CMD *c)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	None.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib___free_ior (struct IOR *ior) {

    unsigned int aststat;

    BLOCK_ASTS(aststat);
    queue_insert(ior, iorque.tail);
    UNBLOCK_ASTS(aststat);

    return SS$_NORMAL;

} /* netlib___free_ior */

/*
**++
**  ROUTINE:	netlib___alloc_dnsreq
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Allocates some memory.
**
**  RETURNS:	cond_value
**
**  PROTOTYPE:
**
**  	netlib___alloc_dnsreq(unsigned int size, void *ptr)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	None.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib___alloc_dnsreq (struct DNSREQ **dnsreqp) {

    struct DNSREQ *dnsreq;
    unsigned int status, fullsize, aststat;

    BLOCK_ASTS(aststat);
    if (dnsreqzone == 0) {
    	unsigned int algorithm=LIB$K_VM_FIXED;
    	unsigned int flags=LIB$M_VM_GET_FILL0|LIB$M_VM_EXTEND_AREA;
    	status = lib$create_vm_zone(&dnsreqzone, &algorithm, &DNSREQ_S_DNSREQDEF, &flags);
    	if (!OK(status)) {
    	    UNBLOCK_ASTS(aststat);
    	    return status;
    	}
    }

    if (queue_remove(dnsreqque.head, &dnsreq)) {
    	memset(dnsreq, 0, DNSREQ_S_DNSREQDEF);
    	status = SS$_NORMAL;
    } else
    	status = lib$get_vm(&DNSREQ_S_DNSREQDEF, &dnsreq, &dnsreqzone);

    if (OK(status))
    	*dnsreqp = dnsreq;

    UNBLOCK_ASTS(aststat);

    return status;

} /* netlib___alloc_dnsreq */

/*
**++
**  ROUTINE:	netlib___free_dnsreq
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Frees a block of memory.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	netlib___free_dnsreq(struct CMD *c)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	None.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib___free_dnsreq (struct DNSREQ *dnsreq) {

    unsigned int aststat;

    BLOCK_ASTS(aststat);
    queue_insert(dnsreq, dnsreqque.tail);
    UNBLOCK_ASTS(aststat);

    return SS$_NORMAL;

} /* netlib___free_dnsreq */
