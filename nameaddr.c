/*[[ CMS REPLACEMENT HISTORY, Element NAMEADDR.C]]*/
/*[[ *2    19-JAN-2001 08:26:26 MADISON "Updates for V2.3A"]]*/
/*[[ *1    12-MAR-1998 16:41:01 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element NAMEADDR.C]]*/
/*
**++
**  FACILITY:	NETLIB
**
**  ABSTRACT:	DNS resolver routines - support for doing our own
**  	    	DNS gethostbyname/gethostbyaddr support.
**
**  MODULE DESCRIPTION:
**
**  	tbs
**
**  AUTHOR: 	    M. Madison
**  	    	    COPYRIGHT © 1994,2001 MADGOAT SOFTWARE.  ALL RIGHTS RESERVED.
**
**  CREATION DATE:  22-OCT-1994
**
**  MODIFICATION HISTORY:
**
**  	22-OCT-1994 V1.0    Madison 	Initial coding.
**  	17-NOV-1994 V1.1    Madison 	Fixed handling of DNS lookup errors.
**  	18-JAN-2001 V1.1-1  Madison 	Tighten up DNS RC checks.
**--
*/
#include "netlib.h"
/*
**  Forward declarations
*/
    unsigned int netlib___dns_name_to_addr(struct CTX *ctx,
    	    	    struct dsc$descriptor *namdsc, struct INADDRDEF *addrlist,
    	    	    unsigned int listsize, unsigned int *count,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm);
    static unsigned int name_to_addr_completion(struct DNSREQ *dnsreq);
    unsigned int netlib___dns_addr_to_name(struct CTX *ctx,
    	    	    struct INADDRDEF *addrlist, unsigned int addrsize,
    	    	    struct dsc$descriptor *namdsc, unsigned short *retlen,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm);
    static unsigned int addr_to_name_completion(struct DNSREQ *dnsreq);

/*
**  External references
*/
    unsigned int netlib_dns_query(struct CTX **xctx, struct dsc$descriptor *namdsc,
    	    	    unsigned int *class, unsigned int *type,
    	    	    unsigned char *buf, unsigned short *bufsize,
    	    	    unsigned int *flags, struct NETLIBIOSBDEF *iosb,
    	    	    void (*astadr)(), void *astprm);
    int netlib_dns_skipname(unsigned char *bufp, unsigned short *buflen);
    unsigned int netlib_dns_expandname(unsigned char *buf, unsigned short *buflen,
    	    	    unsigned char *bufp, struct dsc$descriptor *namdsc,
    	    	    unsigned short *retlen, unsigned short *bufchrs);
    unsigned int netlib___dns_init(struct CTX *ctx);
    

/*
**++
**  ROUTINE:	netlib___dns_name_to_addr
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
unsigned int netlib___dns_name_to_addr (struct CTX *ctx,
    	    	    struct dsc$descriptor *namdsc, struct INADDRDEF *addrlist,
    	    	    unsigned int listsize, unsigned int *count,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm) {

    unsigned short bufsize;
    unsigned int status;
    struct DNSREQ *dnsreq;
    struct IOR *ior;

    static unsigned int class = NETLIB_K_DNS_CLASS_IN;
    static unsigned int type  = NETLIB_K_DNS_TYPE_A;
    static unsigned int queryflags = NETLIB_M_DOMAIN_SEARCH;

    if (ctx->flags & CTX_M_NO_DNS) return SS$_UNSUPPORTED;

    if (ctx->dnsctx == 0) {
    	status = netlib___dns_init(ctx);
    	if (!OK(status)) return SS$_UNSUPPORTED;
    }

    GET_DNSREQ(dnsreq, ctx, iosb, astadr, astprm);
    ior = dnsreq->ior;
    ior->arg[0].address = addrlist;
    ior->arg[1].longword = listsize;
    ior->arg[2].address = count;
    bufsize = sizeof(dnsreq->buf);
    status = netlib_dns_query(&ctx, namdsc, &class, &type,
    	    	    	dnsreq->buf, &bufsize,
    	    	    	&queryflags,
    	    	    	&ior->iosb,
    	    	    	(astadr == 0) ? 0 
    	    	    	    : (void (*)()) name_to_addr_completion, dnsreq);
    if (astadr != 0) return status;

    return name_to_addr_completion(dnsreq);

} /* netlib___dns_name_to_addr */

/*
**++
**  ROUTINE:	name_to_addr_completion
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
static unsigned int name_to_addr_completion (struct DNSREQ *dnsreq) {

    struct IOR *ior = dnsreq->ior;
    struct CTX *ctx = ior->ctx;
    struct INADDRDEF *alist = ior->arg[0].address;
    unsigned int *count = ior->arg[2].address;
    struct dsc$descriptor dsc;
    struct NETLIB_DNS_HEADER *hdr;
    unsigned char *cp;
    char name[256];
    unsigned short retlen, remain, buflen, skip;
    int i, n, ancount, qdcount, type, class;

    if (!OK(ior->iosb.iosb_w_status)) {
    	unsigned int status;

    	if (ior->iosbp != 0) {
    	    ior->iosbp->iosb_w_status = ior->iosb.iosb_w_status;
    	    ior->iosbp->iosb_w_count  = 0;
    	    ior->iosbp->iosb_l_unused = 0;
    	}
    	if (count != 0) *count = 0;
    	if (ior->astadr != 0) (*ior->astadr)(ior->astprm);
    	status = ior->iosb.iosb_w_status;
    	FREE_DNSREQ(dnsreq);
    	return status;
    }

    hdr = (struct NETLIB_DNS_HEADER *) dnsreq->buf;
    ancount = netlib_word_swap(hdr->dns_w_ancount);
    qdcount = netlib_word_swap(hdr->dns_w_qdcount);

    if (hdr->dns_v_reply_code != NETLIB_K_DNS_RC_SUCCESS) {
    	unsigned int status;
    	status = ((hdr->dns_v_reply_code == NETLIB_K_DNS_RC_NAMERR &&
    	    	   hdr->dns_v_authoritative) ? SS$_ENDOFFILE : SS$_ABORT);
    	if (ior->iosbp != 0) {
    	    ior->iosbp->iosb_w_status = status;
    	    ior->iosbp->iosb_w_count = 0;
    	    ior->iosbp->iosb_l_unused = 0;
    	}
    	if (count != 0) *count = 0;
    	if (ior->astadr != 0) (*ior->astadr)(ior->astprm);
    	FREE_DNSREQ(dnsreq);
    	return status;
    }

    buflen = ior->iosb.iosb_w_count;
    remain =  buflen - sizeof(*hdr);
    cp = dnsreq->buf + sizeof(*hdr);
    while (qdcount-- > 0) {
    	i = netlib_dns_skipname(cp, &remain) + 4;
    	cp += i;
    	remain -= i;
    }

    INIT_SDESC(dsc, sizeof(name), name);
    n = 0;

    while (ancount-- > 0 && remain > 0 && n < ior->arg[1].longword) {
    	if (!OK(netlib_dns_expandname(dnsreq->buf, &buflen, cp, &dsc, &retlen, &skip))) break;
    	cp += skip;
    	remain -= skip;
    	type = netlib_word_swap(*(unsigned short *)cp);
    	cp += 2; remain -= 2;
    	class = netlib_word_swap(*(unsigned short *)cp);
    	cp += 6; remain -= 6; /* skip over TTL, too */
    	i = netlib_word_swap(*(unsigned short *)cp);
    	cp += 2; remain -= 2;
    	if (type == NETLIB_K_DNS_TYPE_A) {
    	    alist[n++].inaddr_l_addr = *(unsigned long *)cp;
    	}
    	cp += i;
    	remain -= i;
    }
    
    if (count != 0) *count = n;
    if (ior->iosbp != 0) {
    	ior->iosbp->iosb_w_status = SS$_NORMAL;
    	ior->iosbp->iosb_w_count  = n;
    	ior->iosbp->iosb_l_unused = 0;
    }
    if (ior->astadr) (*ior->astadr)(ior->astprm);
    FREE_DNSREQ(dnsreq);
    return SS$_NORMAL;

} /* name_to_addr_completion */

/*
**++
**  ROUTINE:	netlib___dns_addr_to_name
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
unsigned int netlib___dns_addr_to_name (struct CTX *ctx,
    	    	    struct INADDRDEF *xaddr, unsigned int addrsize,
    	    	    struct dsc$descriptor *namdsc, unsigned short *retlen,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm) {

    struct dsc$descriptor dsc;
    struct IOR *ior;
    struct DNSREQ *dnsreq;
    char tmp[256];
    unsigned short bufsize;
    unsigned int status;
    unsigned char *addr;

    static unsigned int class = NETLIB_K_DNS_CLASS_IN;
    static unsigned int type  = NETLIB_K_DNS_TYPE_PTR;
    static unsigned int queryflags = NETLIB_M_DOMAIN_SEARCH;

    if (ctx->flags & CTX_M_NO_DNS) return SS$_UNSUPPORTED;

    if (ctx->dnsctx == 0) {
    	status = netlib___dns_init(ctx);
    	if (!OK(status)) return SS$_UNSUPPORTED;
    }

    GET_DNSREQ(dnsreq, ctx, iosb, astadr, astprm);
    ior = dnsreq->ior;
    ior->arg[0].address = namdsc;
    ior->arg[1].address = retlen;
    addr = (unsigned char *) xaddr;
    INIT_SDESC(dsc, sprintf(tmp, "%d.%d.%d.%d.in-addr.arpa.",
    	    	    	addr[3], addr[2], addr[1], addr[0]), tmp);
    bufsize = sizeof(dnsreq->buf);
    status = netlib_dns_query(&ctx, &dsc, &class, &type,
    	    	    	dnsreq->buf, &bufsize,
    	    	    	&queryflags,
    	    	    	&ior->iosb,
    	    	    	(astadr == 0) ? 0 
    	    	    	    : (void (*)()) addr_to_name_completion, dnsreq);
    if (astadr != 0) return status;

    return addr_to_name_completion(dnsreq);

} /* netlib___dns_addr_to_name */

/*
**++
**  ROUTINE:	addr_to_name_completion
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
static unsigned int addr_to_name_completion (struct DNSREQ *dnsreq) {

    struct IOR *ior = dnsreq->ior;
    struct CTX *ctx = ior->ctx;
    struct dsc$descriptor *namdsc = ior->arg[0].address;
    unsigned short *retlenp = ior->arg[1].address;
    struct dsc$descriptor dsc;
    struct NETLIB_DNS_HEADER *hdr;
    unsigned char *cp;
    unsigned short retlen, remain, buflen, n, skip;
    char name[256];
    int i, ancount, qdcount, type, class;
    unsigned int status;

    if (!OK(ior->iosb.iosb_w_status)) {
    	if (ior->iosbp != 0) {
    	    ior->iosbp->iosb_w_status = ior->iosb.iosb_w_status;
    	    ior->iosbp->iosb_w_count  = 0;
    	    ior->iosbp->iosb_l_unused = 0;
    	}
    	if (retlenp != 0) *retlenp = 0;
    	if (ior->astadr != 0) (*ior->astadr)(ior->astprm);
    	status = ior->iosb.iosb_w_status;
    	FREE_DNSREQ(dnsreq);
    	return status;
    }

    hdr = (struct NETLIB_DNS_HEADER *) dnsreq->buf;
    ancount = netlib_word_swap(hdr->dns_w_ancount);
    qdcount = netlib_word_swap(hdr->dns_w_qdcount);

    if (hdr->dns_v_reply_code != NETLIB_K_DNS_RC_SUCCESS) {
    	unsigned int status;
    	status = ((hdr->dns_v_reply_code == NETLIB_K_DNS_RC_NAMERR &&
    	    	   hdr->dns_v_authoritative) ? SS$_ENDOFFILE : SS$_ABORT);
    	if (ior->iosbp != 0) {
    	    ior->iosbp->iosb_w_status = SS$_ENDOFFILE;
    	    ior->iosbp->iosb_w_count = 0;
    	    ior->iosbp->iosb_l_unused = 0;
    	}
    	if (retlenp != 0) *retlenp = 0;
    	if (ior->astadr != 0) (*ior->astadr)(ior->astprm);
    	FREE_DNSREQ(dnsreq);
    	return status;
    }

    buflen = ior->iosb.iosb_w_count;
    remain =  buflen - sizeof(*hdr);
    cp = dnsreq->buf + sizeof(*hdr);
    while (qdcount-- > 0) {
    	i = netlib_dns_skipname(cp, &remain) + 4;
    	cp += i;
    	remain -= i;
    }

    INIT_SDESC(dsc, sizeof(name), name);
    status = SS$_NORMAL;

    while (ancount-- > 0 && remain > 0) {
    	if (!OK(netlib_dns_expandname(dnsreq->buf, &buflen, cp, &dsc, &retlen, &skip))) break;
    	cp += skip;
    	remain -= skip;
    	type = netlib_word_swap(*(unsigned short *)cp);
    	cp += 2; remain -= 2;
    	class = netlib_word_swap(*(unsigned short *)cp);
    	cp += 6; remain -= 6; /* skip over TTL, too */
    	i = netlib_word_swap(*(unsigned short *)cp);
    	cp += 2; remain -= 2;
    	if (type == NETLIB_K_DNS_TYPE_PTR) {
    	    status = netlib_dns_expandname(dnsreq->buf, &buflen, cp, namdsc, &n, 0);
    	    break;
    	}
    	cp += i;
    	remain -= i;
    }
    
    if (OK(status) && retlenp != 0) *retlenp = n;
    if (ior->iosbp != 0) {
    	ior->iosbp->iosb_w_status = status;
    	ior->iosbp->iosb_w_count  = OK(status) ? n: 0;
    	ior->iosbp->iosb_l_unused = 0;
    }
    if (ior->astadr) (*ior->astadr)(ior->astprm);
    FREE_DNSREQ(dnsreq);
    return status;

} /* addr_to_name_completion */
