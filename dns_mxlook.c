/*[[ CMS REPLACEMENT HISTORY, Element DNS_MXLOOK.C]]*/
/*[[ *4    19-JAN-2001 08:26:24 MADISON "Updates for V2.3A"]]*/
/*[[ *3    18-JAN-1999 21:04:45 MADISON "Fix handling of . as mail exchanger"]]*/
/*[[ *2    15-NOV-1998 09:33:08 MADISON "Fix interpretation of DNS return code"]]*/
/*[[ *1    12-MAR-1998 16:40:57 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element DNS_MXLOOK.C]]*/
/*
**++
**  FACILITY:	NETLIB
**
**  ABSTRACT:	DNS MX RR lookup routines
**
**  MODULE DESCRIPTION:
**
**  	This module contains routines for implementing MX resource
**  record lookups in the DNS using NETLIB's resolver.
**
**  AUTHOR: 	    M. Madison
**  	    	    COPYRIGHT © 1994,1998,1999 MADGOAT SOFTWARE.
**  	    	    ALL RIGHTS RESERVED.
**
**  CREATION DATE:  25-OCT-1994
**
**  MODIFICATION HISTORY:
**
**  	25-OCT-1994 V1.0    Madison 	Initial coding.
**  	01-NOV-1998 V1.0-1  Madison 	Fix handling of DNS reply code.
**  	18-JAN-1999 V1.0-2  Madison 	Ignore "." as mail exchanger.
**--
*/
#include "netlib.h"

/*
**  Forward declarations
*/
    unsigned int netlib___dns_mx_lookup(struct CTX **xctx,
    	    	    struct dsc$descriptor *namdsc, struct MXRRDEF *mxrr,
    	    	    unsigned int *mxrrsize, unsigned int *mxrrcount,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm);
    static unsigned int mxlook_completion(struct DNSREQ *dnsreq);
/*
**  External references
*/
    unsigned int netlib___dns_init(struct CTX *ctx);
    unsigned int netlib_dns_query(struct CTX **xctx, struct dsc$descriptor *namdsc,
    	    	    unsigned int *class, unsigned int *type,
    	    	    unsigned char *buf, unsigned short *bufsize,
    	    	    unsigned int *flags, struct NETLIBIOSBDEF *iosb,
    	    	    void (*astadr)(), void *astprm);
    int netlib_dns_skipname(unsigned char *bufp, unsigned short *buflen);
    unsigned int netlib_dns_expandname(unsigned char *buf, unsigned short *buflen,
    	    	    unsigned char *bufp, struct dsc$descriptor *namdsc,
    	    	    unsigned short *retlen, unsigned short *bufchrs);

/*
**++
**  ROUTINE:	netlib___dns_mx_lookup
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Performs an MX RR lookup, returning the information in the
**  special MXRRDEF structure.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NETLIB___DNS_MX_LOOKUP  ctx, namdsc, mxrr, mxrrsize [,mxrrcount]
**  	    	    	    	    	[,iosb] [,astadr] [,astprm]
**
**  ctx:    	NETLIB context address, longword (unsigned), read only, by reference
**  namdsc: 	char_string, character string, read only, by descriptor
**  mxrr:   	MXRRDEF structure, longword (unsigned), write only, by reference
**  mxrrsize:	longword_unsigned, longword (unsigned), read only, by reference
**  mxrrcount:	longword_unsigned, longword (unsigned), write only, by reference
**  iosb:   	I/O status block, quadword (unsigned), write only, by reference
**  astadr: 	ast_procedure, procedure mask, call, by reference
**  astprm: 	user_arg, longword (unsigned), read only, by value
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
unsigned int netlib___dns_mx_lookup(struct CTX **xctx,
    	    	    struct dsc$descriptor *namdsc, struct MXRRDEF *mxrr,
    	    	    unsigned int *mxrrsize, unsigned int *mxrrcount,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    struct DNSREQ *dnsreq;
    struct IOR *ior;
    unsigned short bufsize;
    unsigned int status;
    int argc;

    static unsigned int class = NETLIB_K_DNS_CLASS_IN;
    static unsigned int type  = NETLIB_K_DNS_TYPE_MX;
    static unsigned int queryflags = NETLIB_M_DOMAIN_SEARCH;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

/*
**  Check arguments
*/
    if (argc < 4) return SS$_INSFARG;
    if (namdsc == 0 || mxrr == 0 || mxrrsize == 0) return SS$_BADPARAM;

/*
**  If NETLIB___DNS_INIT couldn't initialize the resolver, we don't
**  do DNS stuff
*/
    if (ctx->flags & CTX_M_NO_DNS) return SS$_UNSUPPORTED;

/*
**  Initialize the resolver, if necessary
*/
    if (ctx->dnsctx == 0) {
    	status = netlib___dns_init(ctx);
    	if (!OK(status)) return SS$_UNSUPPORTED;
    }

/*
**  Allocate a DNS request block (also gets us a generic I/O request block)
*/
    GET_DNSREQ(dnsreq, ctx, (argc > 5) ? iosb : 0,
    	    	    	    (argc > 6) ? astadr : 0,
    	    	    	    (argc > 7) ? astprm : 0);

/*
**  Fill in the request and start the query
*/
    ior = dnsreq->ior;
    ior->arg[0].address = mxrr;
    ior->arg[1].longword = *mxrrsize;
    ior->arg[2].address = (argc > 4) ? mxrrcount : 0;
    bufsize = sizeof(dnsreq->buf);
    status = netlib_dns_query(&ctx, namdsc, &class, &type,
    	    	    	dnsreq->buf, &bufsize,
    	    	    	&queryflags,
    	    	    	&ior->iosb,
    	    	    	(ior->astadr == 0) ? 0 
    	    	    	    : (void (*)()) mxlook_completion, dnsreq);
    if (ior->astadr != 0) return status;

    if (!OK(status)) {
    	FREE_DNSREQ(dnsreq);
    	return status;
    }

/*
**  We get to this point if the query completed synchronously.
*/
    return mxlook_completion(dnsreq);

} /* netlib___dns_mx_lookup */

/*
**++
**  ROUTINE:	mxlook_completion
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Completion routine for NETLIB___DNS_MXLOOKUP.  Can either
**  be called as an AST routine or in the main-line thread if the
**  DNS query completed synchronously.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	MXLOOK_COMPLETION  dnsrequest
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
static unsigned int mxlook_completion (struct DNSREQ *dnsreq) {

    struct IOR *ior = dnsreq->ior;
    struct MXRRDEF *mxrr = ior->arg[0].address;
    unsigned int *count = ior->arg[2].address;
    struct dsc$descriptor dsc, dsc2;
    struct NETLIB_DNS_HEADER *hdr;
    unsigned char *cp;
    char name[256];
    unsigned short retlen, remain, buflen, skip;
    int i, n, ancount, qdcount, type, class;

/*
**  Check the completion status first.
*/
    if (!OK(ior->iosb.iosb_w_status)) {
    	if (ior->iosbp != 0) {
    	    ior->iosbp->iosb_w_status = ior->iosb.iosb_w_status;
    	    ior->iosbp->iosb_w_count  = 0;
    	    ior->iosbp->iosb_l_unused = 0;
    	}
    	if (count != 0) *count = 0;
    	if (ior->astadr != 0) (*ior->astadr)(ior->astprm);
    	FREE_DNSREQ(dnsreq);
    	return ior->iosb.iosb_w_status;
    }

/*
**  Status was OK, so let's parse out the reply packet.
*/
    hdr = (struct NETLIB_DNS_HEADER *) dnsreq->buf;
    ancount = netlib_word_swap(hdr->dns_w_ancount);
    qdcount = netlib_word_swap(hdr->dns_w_qdcount);

    if (hdr->dns_v_reply_code != NETLIB_K_DNS_RC_SUCCESS) {
    	unsigned int status;
    	status = ((hdr->dns_v_reply_code == NETLIB_K_DNS_RC_NAMERR
    	    	    	&& hdr->dns_v_authoritative) ? SS$_ENDOFFILE : SS$_ABORT);
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

/*
**  Skip over the question
*/
    buflen = ior->iosb.iosb_w_count;
    remain =  buflen - sizeof(*hdr);
    cp = dnsreq->buf + sizeof(*hdr);
    while (qdcount-- > 0) {
    	i = netlib_dns_skipname(cp, &remain) + 4;
    	cp += i;
    	remain -= i;
    }

/*
**  Now parse out the answers, looking for the MX RRs.
*/
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
    	if (type == NETLIB_K_DNS_TYPE_MX) {
    	    mxrr[n].mxrr_l_preference = netlib_word_swap(*(unsigned short *)cp);
    	    INIT_SDESC(dsc2, NETLIB_S_MXRR_NAME, mxrr[n].mxrr_t_name);
    	    netlib_dns_expandname(dnsreq->buf, &buflen, cp + 2, &dsc2, &retlen, &skip);
    	    if (retlen != 0) {
    	    	mxrr[n].mxrr_l_length = retlen;
    	    	n++;
    	    }
    	}
    	cp += i;
    	remain -= i;
    }

/*
**  Fill in the status and RR count information and either call back
**  (for an AST) or return to the caller.
*/
    if (count != 0) *count = n;
    if (ior->iosbp != 0) {
    	ior->iosbp->iosb_w_status = SS$_NORMAL;
    	ior->iosbp->iosb_w_count  = n;
    	ior->iosbp->iosb_l_unused = 0;
    }
    if (ior->astadr) (*ior->astadr)(ior->astprm);
    FREE_DNSREQ(dnsreq);
    return SS$_NORMAL;

} /* mxlook_completion */
