/*[[ CMS REPLACEMENT HISTORY, Element DNS_QUERY.C]]*/
/*[[ *3    16-FEB-2002 12:04:07 MADISON ""]]*/
/*[[ *2    19-JAN-2001 08:26:25 MADISON "Updates for V2.3A"]]*/
/*[[ *1    12-MAR-1998 16:40:57 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element DNS_QUERY.C]]*/
/*
**++
**  FACILITY:	NETLIB
**
**  ABSTRACT:	Formulates and sends DNS queries, preprocesses replies.
**
**  MODULE DESCRIPTION:
**
**  	This module is the core of NETLIB's DNS resolver.
**
**  AUTHOR: 	    M. Madison
**  	    	    COPYRIGHT © 1994,2001,2002 MADGOAT SOFTWARE.  ALL RIGHTS RESERVED.
**
**  CREATION DATE:  24-OCT-1994
**
**  MODIFICATION HISTORY:
**
**  	24-OCT-1994 V1.0    Madison 	Initial coding.
**  	17-NOV-1994 V1.1    Madison 	Connect to nameserver.
**  	18-NOV-1994 V1.2    Madison 	Restructure asynch queries.
**  	18-JAN-2001 V1.2-1  Madison 	Only return EOF if NAMERR & AA set.
**      04-FEB-2002 V1.2-2  Madison     Fix handling of retry count.
**--
*/
#include "netlib.h"

/*
**  Forward routines
*/
    unsigned int netlib_dns_query(struct CTX **xctx, struct dsc$descriptor *namdsc,
    	    	    unsigned int *class, unsigned int *type,
    	    	    unsigned char *buf, unsigned short *bufsize,
    	    	    unsigned int *flags, struct NETLIBIOSBDEF *iosb,
    	    	    void (*astadr)(), void *astprm);
    static unsigned int do_query(struct DNSREQ *dnsreq);
    static unsigned int query_nameserver(struct DNSREQ *dnsreq);
    static void query_completion(struct DNSREQ *dnsreq);
    static void do_write(struct DNSREQ *dnsreq);
    static void do_read(struct DNSREQ *dnsreq);
/*
**  External references
*/
    unsigned int netlib___dns_init(struct CTX *ctx);
    unsigned int netlib_socket(struct CTX **, ...);
    unsigned int netlib_connect(struct CTX **, void *, unsigned int *,...);
    unsigned int netlib_write(struct CTX **, struct dsc$descriptor *, ...);
    unsigned int netlib_read(struct CTX **, struct dsc$descriptor *, ...);
    unsigned int netlib_close(struct CTX **);

/*
**++
**  ROUTINE:	netlib_dns_query
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Formulates a DNS query, sends it out, and obtains a reply (if
**  possible).
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NETLIB_DNS_QUERY  ctx, namdsc, [class], type, buf, bufsize
**  	    	    	    	[,flags] [,iosb] [,astadr] [,astprm]
**
**  ctx:    	NETLIB context address, longword_unsigned, read only, by reference
**  namdsc: 	char_string, character string, read only, by descriptor
**  class:  	longword_unsigned, longword (unsigned), read only, by reference
**  type:   	longword_unsigned, longword (unsigned), read only, by reference
**  buf:    	varying_arg, longword (unsigned), write only, by reference
**  bufsize:	word_unsigned, word (unsigned), read only, by reference
**  flags:  	mask_longword, longword (unsigned), read only, by reference
**  iosb:   	io_status_block, quadword (unsigned), write only, by reference
**  astadr: 	ast_procedure, procedure value, call, by reference
**  astprm: 	user_arg, longword (unsigned), read only, by value
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	See code.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib_dns_query(struct CTX **xctx, struct dsc$descriptor *namdsc,
    	    	    unsigned int *xclass, unsigned int *type,
    	    	    unsigned char *buf, unsigned short *bufsize,
    	    	    unsigned int *xflags, struct NETLIBIOSBDEF *iosb,
    	    	    void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    struct DNSREQ *dnsreq;
    struct IOR *ior;
    struct NETLIB_DNS_HEADER *hp;
    unsigned int class, flags, status;
    unsigned short namlen;
    char *name;
    int argc;

/*
**  Sanity-check the argument list
*/
    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 6) return SS$_INSFARG;
    if (namdsc == 0 || type == 0 || buf == 0 || bufsize == 0) return SS$_BADPARAM;

/*
**  Make sure we're doing DNS, and initialize the resolver context
**  if needed
*/
    if (ctx->flags & CTX_M_NO_DNS) return SS$_UNSUPPORTED;

    if (ctx->dnsctx == 0) {
    	status = netlib___dns_init(ctx);
    	if (!OK(status)) return SS$_UNSUPPORTED;
    }

/*
**  Process the input arguments
*/
    status = lib$analyze_sdesc(namdsc, &namlen, &name);
    if (!OK(status)) return status;

    if (xclass == 0) class = NETLIB_K_DNS_CLASS_IN;
    else class = *xclass;

    if (argc < 7 || xflags == 0) flags = NETLIB_M_DOMAIN_SEARCH;
    else flags = *xflags;

/*
**  Allocate DNS & I/O request blocks
*/
    GET_DNSREQ(dnsreq, ctx, (argc > 7) ? iosb : 0,
    	    	    	    (argc > 8) ? astadr : 0,
    	    	    	    (argc > 9) ? astprm : 0);

/*
**  Initialize the domain search stuff
*/
    if ((flags & NETLIB_M_DOMAIN_SEARCH) && memchr(name, '.', namlen) == 0) {
    	dnsreq->curdom = ctx->dnsctx->domq.head;
    } else {
    	dnsreq->curdom = 0;
    }

/*
**  Fill in the DNS request block and call DO_QUERY to do the work
*/
    dnsreq->query_name 	= name;
    dnsreq->query_namlen = namlen;	
    dnsreq->query_flags	= flags;
    dnsreq->query_rbuf 	= buf;
    dnsreq->query_rbufsize = *bufsize;
    dnsreq->query_class	= class;
    dnsreq->query_type 	= *type;
    dnsreq->ctx         = ctx;
    dnsreq->retries     = ctx->dnsctx->retry_count;

    return do_query(dnsreq);

} /* netlib_dns_query */

/*
**++
**  ROUTINE:	do_query
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Does the actual work of formatting a query.  Calls on QUERY_NAMESERVER
**  to send the query out and get the reply.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	DO_QUERY  dnsrequest
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	See code.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static unsigned int do_query (struct DNSREQ *dnsreq) {

    struct IOR *ior = dnsreq->ior;
    struct CTX *ctx = dnsreq->ctx;
    struct DNSCTX *dc = ctx->dnsctx;
    struct NETLIB_DNS_HEADER *hp;
    char *anchor, *cp;
    unsigned char *bp;
    int i, j;
    unsigned int status;

/*
**  This is set up as a loop for synchronous queries.  For asynchronous
**  queries, we return as soon as the query has been sent out.
*/
    while (1) {

/*
**  Format the query header
*/
    	hp = (struct NETLIB_DNS_HEADER *) dnsreq->buf;
    	memset(hp, 0, sizeof(struct NETLIB_DNS_HEADER));
    	hp->dns_w_queryid = ++dc->queryid;
    	hp->dns_v_opcode  = NETLIB_K_DNS_OP_STDQ;
    	hp->dns_v_recursion_desired = (dnsreq->query_flags & NETLIB_M_NO_RECURSION) ? 0 : 1;
    	hp->dns_w_qdcount = netlib_word_swap(1);

/*
**  Fill in the question -- the name part first
*/
    	bp = dnsreq->buf + sizeof(struct NETLIB_DNS_HEADER);
    	anchor = dnsreq->query_name;
    	i = dnsreq->query_namlen;
    	while (i > 0) {
    	    cp = memchr(anchor, '.', i);
    	    if (cp == 0) {
    	    	*bp++ = i;
    	    	memcpy(bp, anchor, i); bp += i;
    	    	break;
    	    } else {
    	    	j = cp - anchor;
    	    	*bp++ = j;
    	    	memcpy(bp, anchor, j);
    	    	bp += j;
    	    	i -= j + 1;
    	    }
    	    anchor = cp + 1;
    	}

/*
**  And the search domain, if we're doing search domains.
*/
    	if (dnsreq->curdom == 0) {
    	    dnsreq->curdom = (dnsreq->query_flags & NETLIB_M_DOMAIN_SEARCH) ?
    	    	    	    dc->domq.head : (struct DOMAIN *) &dc->domq;
    	} else {
    	    anchor = dnsreq->curdom->name;
    	    i = dnsreq->curdom->length;
    	    if (i >= 1 && *anchor == '.') {
    	    	anchor++;
    	    	i--;
    	    }
    	    while (i > 0) {
    	    	cp = memchr(anchor, '.', i);
    	    	if (cp == 0) {
    	    	    *bp++ = i;
    	    	    memcpy(bp, anchor, i); bp += i;
    	    	    break;
    	    	} else {
    	    	    j = cp - anchor;
    	    	    *bp++ = j;
    	    	    memcpy(bp, anchor, j);
    	    	    bp += j;
    	    	    i -= j + 1;
    	    	}
    	    	anchor = cp + 1;
    	    }
    	    dnsreq->curdom = dnsreq->curdom->flink;
    	}
/*
**  Terminate the name, insert the type and class
*/
    	*bp++ = '\0';
    	*(unsigned short *) bp = netlib_word_swap(dnsreq->query_type); bp += 2;
    	*(unsigned short *) bp = netlib_word_swap(dnsreq->query_class); bp += 2;
    	dnsreq->buflen = bp - dnsreq->buf;

/*
**  Start with the first name server
*/
    	dnsreq->curns = dc->nsq.head;

/*
**  If ASTADR is 0, this is synchronous.
*/
    	if (ior->astadr == 0) {

/*
**  Loop through all the nameservers until we get a successful response
**  back (either RC_SUCCESS or RC_NAMERR && AA set), or we run out of nameservers.
*/
    	    while (dnsreq->curns != (struct NAMESERVER *) &dc->nsq) {
    	    	status = query_nameserver(dnsreq);
    	    	if (OK(status) && dnsreq->replylen > sizeof(struct NETLIB_DNS_HEADER)) {
    	    	    hp = (struct NETLIB_DNS_HEADER *) dnsreq->query_rbuf;
    	    	    if (hp->dns_v_reply_code == NETLIB_K_DNS_RC_SUCCESS)
    	    	    	break;
    	    	    if (hp->dns_v_reply_code == NETLIB_K_DNS_RC_NAMERR &&
    	    	    	    hp->dns_v_authoritative)
    	    	    	break;
    	    	}
    	    	dnsreq->curns = dnsreq->curns->flink;
    	    }
/*
**  Check status and verify the reply code from the server.  If it's
**  not a successful reply, move on to the next search domain; if we've
**  run through all the search domains, return an error: SS$_ENDOFFFILE
**  if no such host or domain, or SS$_ABORT otherwise.
*/
    	    if (!OK(status))
    	    	status = SS$_UNREACHABLE;
    	    else if (dnsreq->replylen > sizeof(struct NETLIB_DNS_HEADER)) {
    	    	hp = (struct NETLIB_DNS_HEADER *) dnsreq->query_rbuf;
    	    	if (hp->dns_v_reply_code == NETLIB_K_DNS_RC_NAMERR &&
    	    	    	hp->dns_v_authoritative &&
    	    	    	dnsreq->curdom != (struct DOMAIN *) &dc->domq)
    	    	    continue;  /* back to top of while(1) loop */
    	    } else
    	    	status = SS$_PROTOCOL;

/*
**  Return the results to the caller (still in synch I/O processing)
*/
    	    if (ior->iosbp != 0) {
    	    	ior->iosbp->iosb_w_status = status;
    	    	ior->iosbp->iosb_w_count = dnsreq->replylen;
    	    	ior->iosbp->iosb_l_unused = 0;
    	    }
    	    FREE_DNSREQ(dnsreq);
    	    return status;

/*
**  For asynchronous calls, just fire off a query and do the rest of
**  the processing in the AST completion routine.
*/
    	} else {
    	    return query_nameserver(dnsreq);
    	}

    } /* while (1) */

    return SS$_DATACHECK; /* should never reach here */

} /* do_query */

/*
**++
**  ROUTINE:	query_nameserver
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Sends a query to a name server.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	QUERY_NAMESERVER  dnsrequest
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	See code.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static unsigned int query_nameserver (struct DNSREQ *dnsreq) {

    struct IOR *ior = dnsreq->ior;
    struct CTX *ctx = dnsreq->ctx;
    struct DNSCTX *dc = dnsreq->ctx->dnsctx;
    struct NETLIB_DNS_HEADER *hp1, *hp2;
    struct dsc$descriptor dsc;
    unsigned int status, sinsize;
    static unsigned int dgram = NETLIB_K_TYPE_DGRAM;

/*
**  Set up the socket address
*/
    memset(&dnsreq->sin, 0, sizeof(dnsreq->sin));
    dnsreq->sin.sin_w_family = NETLIB_K_AF_INET;
    dnsreq->sin.sin_w_port = netlib_word_swap(53);
    dnsreq->sin.sin_x_addr = dnsreq->curns->addr;

/*
**  Create the socket and "connect" it to the server.  We do this
**  so we can find out right away whether or not the server is listening
**  on that UDP port, rather than waiting for the reply timeout if we
**  were to use sendto/recvfrom.  It's also more efficient to do a connect
**  in BSD-based TCP/IP implementations.
*/
    dnsreq->retries = ctx->dnsctx->retry_count;  /* init per name server */
    status = ior->iosb.iosb_w_status = netlib_socket(&ior->ctx, &dgram);
    if (OK(status)) {
    	sinsize = sizeof(dnsreq->sin);
    	if (ior->astadr != 0) {
    	    status = netlib_connect(&ior->ctx, &dnsreq->sin, &sinsize,
    	    	    	    	    &ior->iosb, do_write, dnsreq);
    	    if (OK(status)) return status;
    	    ior->iosb.iosb_w_status = status;
    	} else {
    	    status = netlib_connect(&ior->ctx, &dnsreq->sin, &sinsize);
    	    if (OK(status)) {
    	    	INIT_SDESC(dsc, dnsreq->buflen, dnsreq->buf);
    	    	status = netlib_write(&ior->ctx, &dsc, 0, 0, &ior->iosb);
    	    	if (OK(status)) status = ior->iosb.iosb_w_status;
    	    	else ior->iosb.iosb_w_status = status;
    	    }
    	}

    	if (!OK(status)) netlib_close(&ior->ctx);
    }

    if (!OK(status)) {
    	ior->ctx = 0;
    	if (ior->astadr == 0) return status;
    	query_completion(dnsreq);
    	return SS$_NORMAL;
    }

/*
**  OK, we've got ourselves connected and sent off the query.  Now read
**  the reply.  This loop handles synchronous reads; asynch reply processing
**  is handled in QUERY_COMPLETION.
*/
    while (1) {
    	INIT_SDESC(dsc, dnsreq->query_rbufsize, dnsreq->query_rbuf);
    	status = netlib_read(&ior->ctx, &dsc, 0, 0, 0, &dc->timeout, &ior->iosb);

/*
**  Make sure that the reply is to the query we sent out
*/
    	if (OK(status)) {
    	    if (ior->iosb.iosb_w_count < sizeof(struct NETLIB_DNS_HEADER))
    	    	continue;
    	    hp1 = (struct NETLIB_DNS_HEADER *) dnsreq->buf;
    	    hp2 = (struct NETLIB_DNS_HEADER *) dnsreq->query_rbuf;
    	    if (hp1->dns_w_queryid == hp2->dns_w_queryid) {
    	    	dnsreq->replylen = ior->iosb.iosb_w_count;
    	    	netlib_close(&ior->ctx);
    	    	ior->ctx = 0;
    	    	return SS$_NORMAL;
    	    }

/*
**  If the reply timed out, try again.  This is UDP, after all.
*/
    	} else {
    	    if ((status == SS$_TIMEOUT) && (--dnsreq->retries > 0)) {
    	    	INIT_SDESC(dsc, dnsreq->buflen, dnsreq->buf);
    	    	status = netlib_write(&ior->ctx, &dsc, 0, 0, &ior->iosb);
    	    	if (OK(status)) continue;
    	    }
    	    netlib_close(&ior->ctx);
    	    ior->ctx = 0;
    	    return status;
    	}
    }

    return SS$_DATACHECK; /* should never reach here */

} /* query_nameserver */

/*
**++
**  ROUTINE:	query_completion
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Completion processing for a reply to a query.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	QUERY_COMPLETION  dnsrequest
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	See code.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static void query_completion (struct DNSREQ *dnsreq) {

    struct IOR *ior = dnsreq->ior;
    struct CTX *ctx = dnsreq->ctx;
    struct DNSCTX *dc = ctx->dnsctx;
    struct NETLIB_DNS_HEADER *hp1, *hp2;
    struct dsc$descriptor dsc;
    unsigned int status;

/*
**  If completion status was OK, verify that the response is to the query
**  we sent out.  If it wasn't queue up another read.
*/
    status = ior->iosb.iosb_w_status;
    if (OK(status) && ior->iosb.iosb_w_count >= sizeof(struct NETLIB_DNS_HEADER)) {
    	hp1 = (struct NETLIB_DNS_HEADER *) dnsreq->buf;
    	hp2 = (struct NETLIB_DNS_HEADER *) dnsreq->query_rbuf;
    	if (hp1->dns_w_queryid != hp2->dns_w_queryid) {
    	    INIT_SDESC(dsc, dnsreq->query_rbufsize, dnsreq->query_rbuf);
    	    status = netlib_read(&ior->ctx, &dsc, 0, 0, 0, &dc->timeout,
    	    	    	    &ior->iosb, query_completion, dnsreq);
    	    if (OK(status))
    	    	return;
    	} else if (hp2->dns_v_reply_code == NETLIB_K_DNS_RC_SUCCESS ||
    	    	    (hp2->dns_v_reply_code == NETLIB_K_DNS_RC_NAMERR && hp2->dns_v_authoritative) ||
    	    	    dnsreq->curns->flink == (struct NAMESERVER *) &dc->nsq) {
    	    if (ior->ctx != 0)
    	    	netlib_close(&ior->ctx);
    	    ior->ctx = 0;
    	    if (ior->iosbp != 0)
    	    	memcpy(ior->iosbp, &ior->iosb, sizeof(ior->iosb));
    	    dnsreq->replylen = ior->iosb.iosb_w_count;
    	    (*ior->astadr)(ior->astprm);
    	    FREE_DNSREQ(dnsreq);
    	    return;
    	}
    }

/*
**  If we timed out, re-send the query.
*/
    if ((status == SS$_TIMEOUT) && (--dnsreq->retries > 0)) {
    	INIT_SDESC(dsc, dnsreq->buflen, dnsreq->buf);
    	status = netlib_write(&ior->ctx, &dsc, 0, 0,
    	    	    	    	&ior->iosb, do_read, dnsreq);
    	if (OK(status)) return;
    }

/*
**  Done with this connection.  Don't need the socket any more.
*/
    if (ior->ctx != 0) {
    	netlib_close(&ior->ctx);
    	ior->ctx = 0;
    }

/*
**  Set things up for moving on to next server; if we've run out
**  of servers, move on to next search domain.  If we've run out of
**  those, we report the last error we got to the caller.
*/
    dnsreq->curns = dnsreq->curns->flink;

    if (dnsreq->curns == (struct NAMESERVER *) &dc->nsq) {
    	if (dnsreq->curdom == (struct DOMAIN *) &dc->domq) {
    	    if (ior->iosbp != 0)
    	    	memcpy(ior->iosbp, &ior->iosb, sizeof(ior->iosb));
    	    (*ior->astadr)(ior->astprm);
    	    FREE_DNSREQ(dnsreq);
    	    return;
    	}
/*
**  Next query
*/
    	do_query(dnsreq);
    	return;
    }

/*
**  Next nameserver
*/
    query_nameserver(dnsreq);

} /* query_completion */

/*
**++
**  ROUTINE:	do_write
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Queue a write to the nameserver (for asynch queries).  Called
**  as completion AST on the connect.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	DO_WRITE  dnsreq
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	See code.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static void do_write (struct DNSREQ *dnsreq) {

    struct IOR *ior = dnsreq->ior;
    struct dsc$descriptor dsc;
    unsigned int status;

    status = ior->iosb.iosb_w_status;
    if (OK(status)) {
    	INIT_SDESC(dsc, dnsreq->buflen, dnsreq->buf);
    	status = netlib_write(&ior->ctx, &dsc, 0, 0, &ior->iosb, do_read, dnsreq);
    }

    if (OK(status)) return;

    ior->iosb.iosb_w_status = status;
    query_completion(dnsreq);

} /* do_write */

/*
**++
**  ROUTINE:	do_read
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Queues a read to the nameserver (for asynch queries).  Called
**  as completion AST to DO_WRITE.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	DO_READ  dnsrequest
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	See code.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static void do_read (struct DNSREQ *dnsreq) {

    struct IOR *ior = dnsreq->ior;
    struct DNSCTX *dc = dnsreq->ctx->dnsctx;
    struct dsc$descriptor dsc;
    unsigned int status;

    status = ior->iosb.iosb_w_status;
    if (OK(status)) {
    	INIT_SDESC(dsc, dnsreq->query_rbufsize, dnsreq->query_rbuf);
    	status = netlib_read(&ior->ctx, &dsc, 0, 0, 0, &dc->timeout,
    	    	    	    	&ior->iosb, query_completion, dnsreq);
    }

    if (OK(status)) return;

    ior->iosb.iosb_w_status = status;
    query_completion(dnsreq);

} /* do_read */
