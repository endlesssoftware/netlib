/*[[ CMS REPLACEMENT HISTORY, Element CONNECT.C]]*/
/*[[ *2     1-JAN-1999 11:38:30 MADISON "Fix return of status code in do_connect"]]*/
/*[[ *1    12-MAR-1998 16:40:56 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element CONNECT.C]]*/
/*
**++
**  FACILITY:	NETLIB
**
**  ABSTRACT:	Connect by name.
**
**  MODULE DESCRIPTION:
**
**  	This module contains the NETLIB_CONNECT_BY_NAME routine,
**  which does establishes an active TCP connect to a remote host and
**  port by name, rather than address.
**
**  AUTHOR: 	    M. Madison
**  	    	    COPYRIGHT © 1994,1995,1998  MADGOAT SOFTWARE.  ALL RIGHTS RESERVED.
**
**  CREATION DATE:  25-OCT-1994
**
**  MODIFICATION HISTORY:
**
**  	25-OCT-1994 V1.0    Madison 	Initial coding.
**  	17-NOV-1994 V1.0-1  Madison 	Don't use _both_ DNS & host tables!
**  	19-JAN-1994 V1.0-2  Madison 	Fix connect() call completion args.
**  	28-DEC-1998 V1.0-3  Madison 	Fix connect() return in case 1.
**--
*/
#include "netlib.h"

/*
**  Context structure used to track our progress
*/
    struct Connect_Context {
    	struct NETLIBIOSBDEF iosb;
    	struct dsc$descriptor dsc;
    	unsigned int ctxsize;
    	char *name;
    	struct CTX *ctx;
    	int state;
    	unsigned int adrcnt, htadrcnt, nsadrcnt;
    	int curadr;
    	void (*astadr)();
    	void *astprm;
    	struct NETLIBIOSBDEF *user_iosb;
    	struct SINDEF sin;
    	struct INADDRDEF *adrlst;
    	struct INADDRDEF htadrlst[8], nsadrlst[8];
    };
/*
**  Forward declarations
*/
    unsigned int netlib_connect_by_name(struct CTX **xctx,
    	    	    struct dsc$descriptor *dsc, unsigned short *port,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm);
    static unsigned int do_connect(struct Connect_Context *con);
/*
**  OWN storage
*/
    static unsigned int usedns = NETLIB_K_LOOKUP_DNS;
    static unsigned int useht  = NETLIB_K_LOOKUP_HOST_TABLE;
    static unsigned int sinsize = sizeof(struct SINDEF);
/*
**  External references
*/
    unsigned int netlib_name_to_address(struct CTX **, unsigned int *,
    	    	    struct dsc$descriptor *, struct INADDRDEF *,
    	    	    unsigned int *, ...);
    unsigned int netlib_connect(struct CTX **, struct SINDEF *,
    	    	    unsigned int *, ...);
    unsigned int netlib_strtoaddr(struct dsc$descriptor *, struct INADDRDEF *);

/*
**++
**  ROUTINE:	netlib_connect_by_name
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Connects to a remote host/port by name.  The name is looked up,
**  then a connection is tried to each address until a connection is
**  established, or we run out of addresses.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NETLIB_CONNECT_BY_NAME  ctxptr, namdsc, port [,iosb] [,astadr] [,astprm]
**
**  ctxptr: 	NETLIB context, longword (unsigned), read only, by reference
**  namdsc: 	char_string, character string, read only, by descriptor
**  port:   	word_unsigned, word (unsigned), read only, by reference
**  iosb:   	io_status_block, quadword (unsigned), write only, by reference
**  astadr: 	ast_procedure, procedure value, call, by reference
**  astprm: 	user_arg, longword (unsigned), read only, by value
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	SS$_NORMAL: 	    normal successful completion
**  	SS$_INSFARG:	    not enough arguments
**  	SS$_BADPARAM:	    invalid argument
**  	Codes from LIB$GET_VM, LIB$ANALYZE_SDESC, and
**  	other NETLIB network status codes.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib_connect_by_name (struct CTX **xctx,
    	    	    struct dsc$descriptor *dsc, unsigned short *port,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm) {

    struct Connect_Context *con;
    struct CTX *ctx;
    struct INADDRDEF addr;
    unsigned int status, size;
    unsigned short namlen;
    char *namp;
    int argc;

/*
**  Verify the arguments
*/
    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 3) return SS$_INSFARG;
    if (dsc == 0 || port == 0) return SS$_BADPARAM;

/*
**  Allocate and fill in the connection context
*/
    status = lib$analyze_sdesc(dsc, &namlen, &namp);
    if (!OK(status)) return status;
    size = namlen + sizeof(struct Connect_Context);
    status = lib$get_vm(&size, &con);
    if (!OK(status)) return status;
    memset(con, 0, size);
    con->name = (char *) (con + 1);
    memcpy(con->name, namp, namlen);
    INIT_SDESC(con->dsc, namlen, con->name);
    con->ctx = ctx;
    con->sin.sin_w_family = NETLIB_K_AF_INET;
    con->sin.sin_w_port = netlib_word_swap(*port);
    con->ctxsize = size;

    size = sizeof(con->htadrlst)/sizeof(con->htadrlst[0]);
    if (argc > 3 && iosb != 0) con->user_iosb = iosb;

    if (argc > 4 && astadr != 0) {
    	con->astadr = astadr;
    	if (argc > 5) con->astprm = astprm;
    }
/*
**  If they provided us with a dotted-decimal IP address, fake
**  out do_connect to make it look like we looked up the address
**  via DNS.
*/
    if (OK(netlib_strtoaddr(&con->dsc, &addr))) {
    	con->iosb.iosb_w_status = SS$_NORMAL;
    	con->nsadrcnt = 1;
    	con->nsadrlst[0] = addr;
    	if (con->astadr != 0) return sys$dclast(do_connect, con, 0);
    	return do_connect(con);
    }

/*
**  Make lookup via host table synchronous and in main-line thread
**  because we can't call it from AST level for all packages
*/
    status = netlib_name_to_address(&con->ctx, &useht, &con->dsc,
    	    	    con->htadrlst, &size, &con->htadrcnt, &con->iosb);
    if (!OK(status)) con->htadrcnt = 0;
    size = sizeof(con->nsadrlst)/sizeof(con->nsadrlst[0]);

/*
**  For an asynch call, do the DNS lookup and have DO_CONNECT invoked
**  as the AST routine
*/
    if (argc > 4 && astadr != 0) {
    	status = netlib_name_to_address(&con->ctx, &usedns, &con->dsc,
    	    	    con->nsadrlst, &size, &con->nsadrcnt, &con->iosb,
    	    	    do_connect, con);
    	if (!OK(status)) lib$free_vm(&con->ctxsize, &con);
    	return status;
    }

/*
**  Synchronous call: do the DNS lookup...
*/
    status = netlib_name_to_address(&con->ctx, &usedns, &con->dsc,
    	    	    con->nsadrlst, &size, &con->nsadrcnt, &con->iosb);

/*
**  ... if it failed, fall back on the host table lookup info we got
*/
    if (!OK(status)) {
    	con->iosb.iosb_w_status = SS$_ENDOFFILE;
    	con->nsadrcnt = 0;
    }

/*
**  Just call DO_CONNECT to complete this for us
*/
    return do_connect(con);

} /* netlib_connect_by_name */

/*
**++
**  ROUTINE:	do_connect
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Completion routine for NETLIB_CONNECT_BY_NAME.  Can be
**  invoked as a regular main-line routine or an AST completion.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	DO_CONNECT  connection-context
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	Any NETLIB network status code.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static unsigned int do_connect (struct Connect_Context *con) {

    unsigned int status;
    int done;

/*
**  We implement our FSM as a loop for the synchronous case
*/
    done = 0;
    while (!done) {

    	status = con->iosb.iosb_w_status;

    	switch (con->state) {

/*
**  Initial state -- if the DNS lookup failed, fall back on host table
**  entry.  Otherwise, start trying the connections.
*/
    	    case 0:
    	    	if (con->nsadrcnt == 0) {
    	    	    if (con->htadrcnt == 0) {
    	    	    	con->iosb.iosb_w_status = SS$_ENDOFFILE;
    	    	    	done = 1;
    	    	    	break;
    	    	    }
    	    	    con->adrlst = con->htadrlst;
    	    	    con->adrcnt = con->htadrcnt;
    	    	} else {
    	    	    con->adrlst = con->nsadrlst;
    	    	    con->adrcnt = con->nsadrcnt;
    	    	}
    	    	con->state = 1;
    	    	/* and fall through */
/*
**  State 1: Attempt a connection
*/
    	    case 1:
    	    	con->sin.sin_x_addr = con->adrlst[con->curadr++];
    	    	con->state = 2;
    	    	status = netlib_connect(&con->ctx, &con->sin, &sinsize,
    	    	    	    &con->iosb, (con->astadr == 0) ? 0 : do_connect,
    	    	    	    	    	(con->astadr == 0) ? 0 : con);
    	    	if (!OK(status)) done = 1;
    	    	else if (con->astadr != 0) return status;
    	    	break;

/*
**  State 2: connect() completion status check.  If we're successful
**  or we've run out of addresses, we're done.  Otherwise, we loop
**  back up and try again.
*/
    	    case 2:
    	    	if (OK(status) || con->curadr >= con->adrcnt) done = 1;
    	    	con->state = 1;
    	    	break;
    	}

    }

/*
**  We're done, one way or another.  Fill in the caller's IOSB and
**  call back the AST, if there was one.
*/
    if (con->user_iosb != 0)
    	memcpy(con->user_iosb, &con->iosb, sizeof(con->iosb));
    if (con->astadr != 0) (*con->astadr)(con->astprm);

/*
**  We're done with this context -- free it
*/
    lib$free_vm(&con->ctxsize, &con);

/*
**  Synchronous completion occurs here.
*/
    return status;

} /* do_connect */
