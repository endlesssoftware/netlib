/*[[ CMS REPLACEMENT HISTORY, Element NETLIB_UCX.C]]*/
/*[[ *4     1-JAN-1999 11:42:05 MADISON "Update for UCX V5.0"]]*/
/*[[ *3    13-DEC-1998 07:13:27 MADISON "Complete the fix for TCPware address-to-name workaround"]]*/
/*[[ *2    12-MAR-1998 17:06:17 MADISON "Work around TCPware bug in NETLIB_ADDRESS_TO_NAME"]]*/
/*[[ *1    12-MAR-1998 16:41:04 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element NETLIB_UCX.C]]*/
/*
**++
**  FACILITY:	NETLIB
**
**  ABSTRACT:	Routines specific to UCX.
**
**  MODULE DESCRIPTION:
**
**  	tbs
**
**  AUTHOR: 	    M. Madison
**
**  Copyright (c) 2008, Matthew Madison.
**  Copyright (c) 2012, Endless Software Solutions.
**
**  All rights reserved.
**
**  Redistribution and use in source and binary forms, with or without
**  modification, are permitted provided that the following conditions
**  are met:
**
**      * Redistributions of source code must retain the above
**        copyright notice, this list of conditions and the following
**        disclaimer.
**      * Redistributions in binary form must reproduce the above
**        copyright notice, this list of conditions and the following
**        disclaimer in the documentation and/or other materials provided
**        with the distribution.
**      * Neither the name of the copyright owner nor the names of any
**        other contributors may be used to endorse or promote products
**        derived from this software without specific prior written
**        permission.
**
**  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
**  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
**  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
**  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
**  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
**  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
**  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
**  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
**  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
**  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
**  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
**  CREATION DATE:  26-OCT-1994
**
**  MODIFICATION HISTORY:
**
**  	26-OCT-1994 V1.0    Madison 	Initial coding.
**  	09-JAN-1995 V1.0-1  Madison 	Fix some missing args on some QIOs.
**  	19-JAN-1995 V1.0-2  Madison 	Handle 0-length writes specially.
**  	28-FEB-1995 V1.0-3  Madison 	Fix missing arg on $DCLAST call.
**  	24-JUL-1995 V1.0-4  Madison 	Fix netlib___get_nameservers().
**  	13-OCT-1995 V1.0-5  Madison 	Free IORs on $QIO failure.
**  	11-FEB-1996 V1.0-6  Madison 	Fix argc check in netlib_shutdown.
**  	01-JUN-1997 V1.0-7  Madison 	Use a flag to indicate a timeout happened.
**  	10-JUN-1997 V1.0-8  Goatley 	Fix dumb netlib___cvt_status bug.
**  	12-MAR-1998 V1.0-9  Madison 	Work around TCPware address-to-name bug.
**  	12-DEC-1998 V1.0-10 Madison 	Missed one change for V1.0-9.
**  	22-DEC-1998 V1.1    Madison 	Add support for UCX V5.0.
**  	29-DEC-1998 V1.1-1  Madison 	accept() did not use V5 name; make ordering
**  	    	    	    	    	of V5/V4 checks consistent throughout.
**	20-NOV-2012 V1.2    Sneddon	Add support for dynamic strings in
**					NETLIB_READ.
**--
*/
#ifdef TCPWARE
#ifdef __DECC
#pragma module NETLIB_TCPWARE
#else
#module NETLIB_TCPWARE
#endif
#endif /* TCPWARE */
#include "netlib_ucx.h"
#include "netlib.h"
#include <lnmdef.h>
/*
**  We don't actually do any cond_value conversions
**  because we're using the UCX ones.
*/
#define netlib___cvt_status(_iosb) ((_iosb)->iosb_w_status)
#define netlib___cvt_iosb(_dst, _src) {\
    	    (_dst)->iosb_w_status = (_src)->iosb_w_status;\
    	    (_dst)->iosb_w_count =  (_src)->iosb_w_count;\
    	    (_dst)->iosb_l_unused = 0;}
/*
**  This is so we don't need UCX$IPC.OLB when we link with VAX C
*/
#ifdef __DECC
#define gethostname decc$gethostname
#else
#define gethostname vaxc$gethostname
#endif

/*
**  Forward declarations
*/
    unsigned int netlib_socket(struct CTX **xctx, unsigned int *type,
    	    	    	    	unsigned int *family);
    unsigned int netlib_server_setup(struct CTX **xctx, struct SINDEF *sa,
    	    	    	    	unsigned int *salen);
    unsigned int netlib_bind(struct CTX **xctx, struct SINDEF *sa,
    	    	    	    unsigned int *salen, struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm);
    static unsigned int io_completion(struct IOR *ior);
    unsigned int netlib_getsockname(struct CTX **ctx, struct SINDEF *sa,
    	    	    	    	 unsigned int *sasize, unsigned int *salen,
    	    	    	    	 struct NETLIBIOSBDEF *iosb,
    	    	    	    	 void (*astadr)(), void *astprm);
    unsigned int netlib_getpeername(struct CTX **ctx, struct SINDEF *sa,
    	    	    	    	 unsigned int *sasize, unsigned int *salen,
    	    	    	    	 struct NETLIBIOSBDEF *iosb,
    	    	    	    	 void (*astadr)(), void *astprm);
    unsigned int netlib_connect(struct CTX **ctx, struct SINDEF *sa,
    	    	    	    	 unsigned int *salen,
    	    	    	    	 struct NETLIBIOSBDEF *iosb,
    	    	    	    	 void (*astadr)(), void *astprm);
    unsigned int netlib_write(struct CTX **ctx, struct dsc$descriptor *dsc,
    	    	    	    struct SINDEF *sa, unsigned int *salen,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm);
    unsigned int netlib_read(struct CTX **ctx, struct dsc$descriptor *dsc,
    	    	    	    struct SINDEF *sa, unsigned int *sasize,
    	    	    	    unsigned int *salen, TIME *tmo,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm);
    static unsigned int io_timeout(struct IOR *ior);
    unsigned int netlib_shutdown(struct CTX **xctx, unsigned int *shuttype,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm);
    unsigned int netlib_close(struct CTX **xctx);
    unsigned int netlib_listen(struct CTX **xctx, unsigned int *backlog,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm);
    unsigned int netlib_accept(struct CTX **ctx, struct CTX **xnewctx,
    	    	    	    struct SINDEF *sa, unsigned int *sasize,
    	    	    	    unsigned int *salen, struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm);
    unsigned int netlib_setsockopt(struct CTX **xctx,
    	    	    	    unsigned int *level, unsigned int *option,
    	    	    	    void *value, unsigned int *vallen,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm);
    unsigned int netlib_getsockopt(struct CTX **xctx,
    	    	    	    unsigned int *level, unsigned int *option,
    	    	    	    void *value, unsigned int *valsize,
    	    	    	    unsigned int *vallen,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm);
    unsigned int netlib_name_to_address(struct CTX **xctx, unsigned int *whichp,
    	    	    	    struct dsc$descriptor *namdsc,
    	    	    	    struct INADDRDEF *addrlist,
    	    	    	    unsigned int *listsize, unsigned int *count,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm);
    unsigned int netlib_address_to_name(struct CTX **xctx, unsigned int *whichp,
    	    	    	    struct INADDRDEF *addr,
    	    	    	    unsigned int *addrsize,
    	    	    	    struct dsc$descriptor *namdsc,
    	    	    	    unsigned short *retlen,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm);
    unsigned int netlib_get_hostname(void *bufdsc, unsigned short *retlenp);
    int netlib___get_nameservers(QUEUE *nsq);
    int netlib___get_domain(char *buf, unsigned short bufsize, unsigned short *retlen);
    unsigned int netlib_dns_mx_lookup(struct CTX **xctx,
    	    	    struct dsc$descriptor *namdsc, struct MXRRDEF *mxrr,
    	    	    unsigned int *mxrrsize, unsigned int *mxrrcount,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm);
#ifdef TCPWARE
    unsigned int netlib_addrtostr(struct INADDRDEF *, struct dsc$descriptor *, unsigned short *);
#endif /* TCPWARE */
/*
**  OWN storage
*/
    static $DESCRIPTOR(inetdevice, "UCX$DEVICE");
    static $DESCRIPTOR(inetdevice_v5, "TCPIP$DEVICE");
    static $DESCRIPTOR(nameserver_tabnam, "LNM$FILE_DEV");
    static unsigned int hostent_size = sizeof(struct HOSTENT);
/*
**  External references
*/
    unsigned int netlib_strtoaddr(struct dsc$descriptor *, struct INADDRDEF *);
    int gethostname(void *, int);
    unsigned int netlib___dns_mx_lookup(struct CTX **xctx,
    	    	    struct dsc$descriptor *namdsc, struct MXRRDEF *mxrr,
    	    	    unsigned int *mxrrsize, unsigned int *mxrrcount,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm);

/*
**++
**  ROUTINE:	netlib_socket
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Create a "socket".
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
unsigned int netlib_socket (struct CTX **xctx, unsigned int *type,
    	    	    	    	unsigned int *family) {

    struct CTX *ctx;
    struct NETLIBIOSBDEF myiosb;
    struct {
    	unsigned short protocol;
    	unsigned char  type;
    	unsigned char  domain;
    } sockdef;
    unsigned int status, af, ty;
    int argc;

    SETARGCOUNT(argc);

    if (argc < 1) return SS$_INSFARG;

    status = netlib___alloc_ctx(&ctx, SPECCTX_SIZE);
    if (!OK(status)) return status;

    status = sys$assign(&inetdevice_v5, &ctx->chan, 0, 0);
    if (!OK(status)) {
    	status = sys$assign(&inetdevice, &ctx->chan, 0, 0);
    }
    if (!OK(status)) {
    	netlib___free_ctx(ctx);
    	return status;
    }

    sockdef.type = (argc >= 2 && type != 0) ? *type : NETLIB_K_TYPE_STREAM;
    sockdef.domain = (argc >= 3 && family != 0) ? *family : NETLIB_K_AF_INET;
    sockdef.protocol = (sockdef.type == NETLIB_K_TYPE_DGRAM) ? UCX$C_UDP
    	    	    	    	    	    	    	     : UCX$C_TCP;

    status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_SETMODE,
    	    	    	&myiosb, 0, 0,
    	    	    	&sockdef, 0, 0, 0, 0, 0);
    if (OK(status)) status = netlib___cvt_status(&myiosb);

    if (!OK(status)) {
    	sys$dassgn(ctx->chan);
    	netlib___free_ctx(ctx);
    	return status;
    }

    *xctx = ctx;
    return SS$_NORMAL;

} /* netlib_socket */

/*
**++
**  ROUTINE:	netlib_server_setup
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Create a "server_setup".
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
unsigned int netlib_server_setup (struct CTX **xctx, struct SINDEF *sa,
    	    	    	    	    	unsigned int *salen) {

    struct CTX *ctx;
    struct NETLIBIOSBDEF myiosb;
    struct {
    	unsigned short protocol;
    	unsigned char  type;
    	unsigned char  domain;
    } sockdef;
    unsigned int status;

    static $DESCRIPTOR(device, "SYS$NET");

    status = netlib___alloc_ctx(&ctx, SPECCTX_SIZE);
    if (!OK(status)) return status;

    status = sys$assign(&device, &ctx->chan, 0, 0);
    if (!OK(status)) {
    	netlib___free_ctx(ctx);
    	return status;
    }

    sockdef.protocol = UCX$C_TCP;
    sockdef.type = INET_PROTYP$C_STREAM;
    sockdef.domain = UCX$C_AUXS;

    status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_SETMODE,
    	    	    	&myiosb, 0, 0,
    	    	    	&sockdef, 0, 0, 0, 0, 0);
    if (OK(status)) status = netlib___cvt_status(&myiosb);
    if (!OK(status)) {
    	sys$dassgn(ctx->chan);
    	netlib___free_ctx(ctx);
    	return status;
    }

    *xctx = ctx;
    return SS$_NORMAL;

} /* netlib_server_setup */

/*
**++
**  ROUTINE:	netlib_bind
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
unsigned int netlib_bind (struct CTX **xctx, struct SINDEF *sa,
    	    	    	    unsigned int *salen, struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    unsigned int status;
    ITMLST2 sockdsc;
    int argc;
    static unsigned int one = 1;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 3) return SS$_INSFARG;
    if (sa == 0 || salen == 0) return SS$_BADPARAM;
    if (*salen < sizeof(struct SINDEF)) return SS$_BADPARAM;

    if (!(ctx->flags & CTX_M_USER_SET_REUSEADDR)) {
    	ITMLST2 rulst, sockopt;
    	ITMLST2_INIT(rulst, UCX$C_SOCKOPT, sizeof(sockopt), &sockopt);
    	ITMLST2_INIT(sockopt, NETLIB_K_OPTION_REUSEADDR, sizeof(one), &one);
    	sys$qiow(netlib_synch_efn, ctx->chan, IO$_SETMODE, 0, 0, 0,
    	    	    0, 0, 0, 0, &rulst, 0);
    }

    ITMLST2_INIT(sockdsc, 0, *salen, sa);

    if (argc > 4 && astadr != 0) {
    	struct IOR *ior;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 5) ? astprm : 0);
    	status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_SETMODE, &ior->iosb,
    	    	    	    io_completion, ior, 0, 0, &sockdsc, 0, 0, 0);
    	if (!OK(status)) FREE_IOR(ior);
    } else {
    	struct NETLIBIOSBDEF myiosb;
    	status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_SETMODE, &myiosb,
    	    	    	    0, 0, 0, 0, &sockdsc, 0, 0, 0);
    	if (OK(status)) status = netlib___cvt_status(&myiosb);
    	if (argc > 3 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    }

    return status;

} /* netlib_bind */

/*
**++
**  ROUTINE:	io_completion
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
static unsigned int io_completion (struct IOR *ior) {

    struct CTX *ctx = ior->ctx;

    ior->iorflags |= IOR_M_IO_COMPLETED;

    if (ior->iorflags & IOR_M_IO_TIMED) sys$cantim(ior, 0);

    if (ior->iosb.iosb_w_status == SS$_CANCEL && (ior->iorflags & IOR_M_IO_TIMEOUT))
    	ior->iosb.iosb_w_status = SS$_TIMEOUT;

    if (ior->iosbp != 0) netlib___cvt_iosb(ior->iosbp, &ior->iosb);


    if (ior->iorflags & IOR_M_COPY_LENGTH) {
    	if (OK(ior->iosb.iosb_w_status) && ior->spec_retlen != 0)
    	    	*(unsigned int *) ior->spec_retlen = ior->spec_length;
    	ior->iorflags &= ~IOR_M_COPY_LENGTH;
    }

    if (ior->iorflags & IOR_M_COPY_FROM) {
    	if (OK(ior->iosb.iosb_w_status) && ior->spec_userfrom != 0) {
    	    unsigned int len;
    	    len = ior->specior.fromlen;
    	    if (len > ior->spec_length) len = ior->spec_length;
    	    memcpy(ior->spec_userfrom, &ior->specior.from, len);
    	    if (ior->spec_retlen != 0) *(unsigned int *)ior->spec_retlen = len;
    	 }
    	 ior->iorflags &= ~IOR_M_COPY_FROM;
    }

    if (ior->iorflags & IOR_M_COPY_ADDRS) {
    	struct HOSTENT *h;

    	h = ior->spec_hostent;
    	if (OK(ior->iosb.iosb_w_status)) {
    	    char *base;
    	    unsigned int *offlst;
    	    int i;
    	    base = (char *) h;
    	    i = 0;
    	    if (h->addrlist_offset != 0) {
    	    	struct INADDRDEF *alist = ior->spec_useralist;
    	    	offlst = (unsigned int *) (base+h->addrlist_offset);
    	    	while (i < ior->spec_length && offlst[i] != 0) {
    	    	    alist[i] = *(struct INADDRDEF *) (base + offlst[i]);
    	    	    i++;
    	    	}
    	    }
    	    if (ior->spec_retlen != 0) *(unsigned int *)ior->spec_retlen = i;
    	}
    	lib$free_vm(&hostent_size, &h);
    	ior->iorflags &= ~IOR_M_COPY_ADDRS;
    }

    if (ior->iorflags & IOR_M_COPY_HOSTNAME) {
    	struct HOSTENT *h;
    	h = ior->spec_hostent;
    	if (OK(ior->iosb.iosb_w_status)) {
    	    str$copy_r(ior->spec_usrdsc, &ior->specior.fromlen,
    	    	    	    	    	h->buffer);
    	    if (ior->spec_retlen != 0)
    	    	*(unsigned short *)ior->spec_retlen =
    	    	    	    	    	    	ior->specior.fromlen;
    	}
    	lib$free_vm(&hostent_size, &h);
    	ior->iorflags &= ~IOR_M_COPY_HOSTNAME;
    }

    if (ior->iorflags & IOR_M_NEW_CONTEXT) {
    	if (OK(ior->iosb.iosb_w_status)) {
    	    *(struct CTX **) ior->spec_xnewctx = ior->spec_newctx;
    	} else {
    	    sys$dassgn(((struct CTX *)ior->spec_newctx)->chan);
    	    netlib___free_ctx((struct CTX *) ior->spec_newctx);
    	}
    	   ior->iorflags &= ~IOR_M_NEW_CONTEXT;
    }

    if (ior->astadr != 0) (*(ior->astadr))(ior->astprm);

    FREE_IOR(ior);

    return SS$_NORMAL;

} /* io_completion */

/*
**++
**  ROUTINE:	netlib_getsockname
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
unsigned int netlib_getsockname (struct CTX **xctx, struct SINDEF *sa,
    	    	    	    	 unsigned int *sasize, unsigned int *salen,
    	    	    	    	 struct NETLIBIOSBDEF *iosb,
    	    	    	    	 void (*astadr)(), void *astprm) {
    struct CTX *ctx;
    unsigned int status;
    ITMLST slst;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (sa == 0 || sasize == 0) return SS$_BADPARAM;

    if (argc > 5 && astadr != 0) {
    	struct IOR *ior;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 6) ? astprm : 0);
    	ITMLST_INIT(slst, 0, *sasize, sa, &ior->spec_length);
    	ior->spec_retlen = salen;
    	ior->iorflags = IOR_M_COPY_LENGTH;
    	status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_SENSEMODE, &ior->iosb,
    	    	    	    io_completion, ior, 0, 0, &slst, 0, 0, 0);
    	if (!OK(status)) FREE_IOR(ior);
    } else {
    	struct NETLIBIOSBDEF myiosb;
    	unsigned short length;
    	ITMLST_INIT(slst, 0, *sasize, sa, &length);
    	status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_SENSEMODE,
    	    	    	    &myiosb,
    	    	    	    0, 0, 0, 0, &slst, 0, 0, 0);
    	if (argc > 3 && salen != 0) *salen = length;
    	if (OK(status)) status = netlib___cvt_status(&myiosb);
    	if (argc > 4 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    }

    return status;

} /* netlib_getsockname */

/*
**++
**  ROUTINE:	netlib_getpeername
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
unsigned int netlib_getpeername (struct CTX **xctx, struct SINDEF *sa,
    	    	    	    	 unsigned int *sasize, unsigned int *salen,
    	    	    	    	 struct NETLIBIOSBDEF *iosb,
    	    	    	    	 void (*astadr)(), void *astprm) {
    struct CTX *ctx;
    ITMLST slst;
    unsigned int status;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 3) return SS$_INSFARG;
    if (sa == 0 || sasize == 0) return SS$_BADPARAM;

    if (argc > 5 && astadr != 0) {
    	struct IOR *ior;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 6) ? astprm : 0);
    	ITMLST_INIT(slst, 0, *sasize, sa, &ior->spec_length);
    	ior->spec_length = *sasize;
    	ior->spec_retlen = salen;
    	ior->iorflags = IOR_M_COPY_LENGTH;
    	status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_SENSEMODE, &ior->iosb,
    	    	    	    io_completion, ior, 0, 0, 0, &slst, 0, 0);
    	if (!OK(status)) FREE_IOR(ior);
    } else {
    	struct NETLIBIOSBDEF myiosb;
    	unsigned short length;
    	ITMLST_INIT(slst, 0, *sasize, sa, &length);
    	status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_SENSEMODE,
    	    	    	    &myiosb, 0, 0, 0, 0, 0, &slst, 0, 0);
    	if (argc > 3 && salen != 0) *salen = length;
    	if (OK(status)) status = netlib___cvt_status(&myiosb);
    	if (argc > 4 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    }

    return status;

} /* netlib_getpeername */

/*
**++
**  ROUTINE:	netlib_connect
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
unsigned int netlib_connect (struct CTX **xctx, struct SINDEF *sa,
    	    	    	    	 unsigned int *salen,
    	    	    	    	 struct NETLIBIOSBDEF *iosb,
    	    	    	    	 void (*astadr)(), void *astprm) {
    struct CTX *ctx;
    unsigned int status;
    ITMLST2 sname;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (sa == 0 || salen == 0) return SS$_BADPARAM;
    if (*salen < sizeof(struct SINDEF)) return SS$_BADPARAM;

    ITMLST2_INIT(sname, 0, *salen, sa);

    if (argc > 4 && astadr != 0) {  
    	struct IOR *ior;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 5) ? astprm : 0);
    	status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_ACCESS, &ior->iosb,
    	    	    	    io_completion, ior, 0, 0, &sname, 0, 0, 0);
    	if (!OK(status)) FREE_IOR(ior);
    } else {
    	struct NETLIBIOSBDEF myiosb;
    	status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_ACCESS, &myiosb,
    	    	    	    0, 0, 0, 0, &sname, 0, 0, 0);
    	if (OK(status)) status = netlib___cvt_status(&myiosb);
    	if (argc > 3 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    }

    return status;

} /* netlib_connect */

/*
**++
**  ROUTINE:	netlib_write
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
unsigned int netlib_write (struct CTX **xctx, struct dsc$descriptor *dsc,
    	    	    	    struct SINDEF *sa, unsigned int *salen,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm) {
    struct CTX *ctx;
    void *bufptr;
    unsigned int status;
    unsigned short buflen;
    ITMLST2 sname, *snaddr;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    status = lib$analyze_sdesc(dsc, &buflen, &bufptr);
    if (!OK(status)) return status;

    if (argc > 3 && salen != 0) {
    	ITMLST2_INIT(sname, 0, *salen, sa);
    	snaddr = &sname;
    } else snaddr = 0;

    if (argc > 5 && astadr != 0) {
    	struct IOR *ior;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 6) ? astprm : 0);
    	if (buflen == 0) {
    	    ior->iosb.iosb_w_status = SS$_NORMAL;
    	    ior->iosb.iosb_w_count = 0;
    	    ior->iosb.iosb_l_unused = 0;
    	    status = sys$dclast(io_completion, ior, 0);
    	} else {
    	    status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_WRITEVBLK,
    	    	    	    &ior->iosb,
    	    	    	    io_completion, ior, bufptr, buflen, snaddr,
    	    	    	    0, 0, 0);
    	    if (!OK(status)) FREE_IOR(ior);
    	}
    } else {
    	struct NETLIBIOSBDEF myiosb;
    	if (buflen == 0) {
    	    status = myiosb.iosb_w_status = SS$_NORMAL;
    	    myiosb.iosb_w_count = 0;
    	    myiosb.iosb_l_unused = 0;
    	} else {
    	    status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_WRITEVBLK,
    	    	    	    &myiosb, 0, 0, bufptr, buflen, snaddr,
    	    	    	    0, 0, 0);
    	    if (OK(status)) status = netlib___cvt_status(&myiosb);
    	}
    	if (argc > 4 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    }

    return status;

} /* netlib_write */

/*
**++
**  ROUTINE:	netlib_read
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
unsigned int netlib_read (struct CTX **xctx, struct dsc$descriptor *dsc,
    	    	    	    struct SINDEF *sa, unsigned int *sasize,
    	    	    	    unsigned int *salen, TIME *timeout,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm) {
    struct CTX *ctx;
    struct IOR *ior;
    unsigned int status;
    ITMLST sname;
    int argc, do_from;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (dsc->dsc$b_dtype != DSC$K_DTYPE_T && dsc->dsc$b_dtype != 0)
    	return SS$_BADPARAM;
    if (dsc->dsc$b_class == DSC$K_CLASS_D) {
	if (dsc->dsc$w_length == 0) return SS$_BADPARAM;
    } else {
    	if (dsc->dsc$b_class != DSC$K_CLASS_S && dsc->dsc$b_class != 0)
    	return SS$_BADPARAM;
    }

    do_from = (argc > 3 && sa != 0 && sasize != 0 && *sasize != 0);
    if (argc > 7 && astadr != 0) {
    	struct IOR *ior;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 8) ? astprm : 0);
    	if (do_from) ITMLST_INIT(sname, 0, sizeof(struct SINDEF),
    	    	    &ior->specior.from, &ior->specior.fromlen);
    	if (do_from) {
    	    ior->spec_userfrom = sa;
    	    ior->spec_length = *sasize;
    	    ior->spec_retlen = salen;
    	    ior->iorflags = IOR_M_COPY_FROM;
    	} else ior->iorflags = 0;
    	if (timeout != 0) {
    	    ior->iorflags |= IOR_M_IO_TIMED;
    	    status = sys$setimr(netlib_asynch_efn, timeout, io_timeout, ior);
    	    if (!OK(status)) {
    	    	FREE_IOR(ior);
    	    	return status;
    	    }
    	}

    	status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_READVBLK, &ior->iosb,
    	    	    	    io_completion, ior, dsc->dsc$a_pointer,
    	    	    	    dsc->dsc$w_length, do_from ? &sname : 0, 0, 0, 0);
    	if (!OK(status)) {
    	    if (timeout != 0) sys$cantim(ior, 0);
    	    FREE_IOR(ior);
    	}
    } else {
    	struct NETLIBIOSBDEF myiosb;
    	struct SINDEF from;
    	unsigned short fromlen;
    	int timed_out;

    	if (do_from) ITMLST_INIT(sname, 0, sizeof(struct SINDEF),
    	    	    &from, &fromlen);

    	timed_out = 0;
    	if (argc > 5 && timeout != 0) {
    	    GET_IOR(ior, ctx, 0, 0, 0);
    	    ior->iorflags = IOR_M_IO_TIMED;
    	    status = sys$setimr(netlib_asynch_efn, timeout, io_timeout, ior);
    	    if (!OK(status)) {
    	    	FREE_IOR(ior);
    	    	return status;
    	    }
    	}
    	status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_READVBLK,
    	    	    	    &myiosb, 0, 0,
    	    	    	    dsc->dsc$a_pointer, dsc->dsc$w_length,
    	    	    	    do_from ? &sname : 0, 0, 0, 0);
    	if (argc > 5 && timeout != 0) {
    	    sys$cantim(ior, 0);
    	    timed_out = ior->iorflags & IOR_M_IO_TIMEOUT;
    	    FREE_IOR(ior);
    	}
    	if (OK(status)) {
    	    if (timed_out && myiosb.iosb_w_status == SS$_CANCEL)
    	    	myiosb.iosb_w_status = SS$_TIMEOUT;
    	    status = netlib___cvt_status(&myiosb);
    	}
    	if (argc > 6 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    	if (OK(status) && do_from) {
    	    unsigned int len;
    	    len = fromlen;
    	    if (len > *sasize) len = *sasize;
    	    memcpy(sa, &from, len);
    	    if (argc > 4 && salen != 0) *salen = len;
    	}
    }

    return status;

} /* netlib_read */

/*
**++
**  ROUTINE:	io_timeout
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
static unsigned int io_timeout (struct IOR *ior) {

    if (ior->iorflags & IOR_M_IO_COMPLETED) return SS$_NORMAL;
    ior->iorflags |= IOR_M_IO_TIMEOUT;
    return sys$cancel(ior->ctx->chan);

} /* io_timeout */

/*
**++
**  ROUTINE:	netlib_shutdown
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
unsigned int netlib_shutdown (struct CTX **xctx, unsigned int *shuttype,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    unsigned int status;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc > 3 && astadr != 0) {
    	struct IOR *ior;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 4) ? astprm : 0);
    	status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_DEACCESS|IO$M_SHUTDOWN,
    	    	    	    &ior->iosb, io_completion, ior, 0, 0, 0, 
    	    	    	    (shuttype == 0) ? 0 : *shuttype, 0, 0);
    	if (!OK(status)) FREE_IOR(ior);
    } else {
    	struct NETLIBIOSBDEF myiosb;
    	status = sys$qiow(netlib_synch_efn, ctx->chan,
    	    	    	    IO$_DEACCESS|IO$M_SHUTDOWN, &myiosb,
    	    	    	    0, 0, 0, 0, 0,
    	    	    	    (argc > 1 && shuttype != 0) ? *shuttype : 0, 0, 0);
    	if (OK(status)) status = netlib___cvt_status(&myiosb);
    	if (argc > 2 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    }

    return status;

} /* netlib_shutdown */

/*
**++
**  ROUTINE:	netlib_close
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
unsigned int netlib_close (struct CTX **xctx) {

    struct CTX *ctx;
    unsigned int status;

    VERIFY_CTX(xctx, ctx);

    status = sys$dassgn(ctx->chan);
    if (!OK(status)) return status;

    netlib___free_ctx(ctx);

    *xctx = 0;

    return SS$_NORMAL;

} /* netlib_close */

/*
**++
**  ROUTINE:	netlib_listen
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
unsigned int netlib_listen (struct CTX **xctx, unsigned int *backlog,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    unsigned int status;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc > 3 && astadr != 0) {
    	struct IOR *ior;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 5) ? astprm : 0);
    	status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_SETMODE, &ior->iosb,
    	    	    	    io_completion, ior, 0, 0, 0, 
    	    	    	    (backlog == 0) ? 4 : *backlog, 0, 0);
    	if (!OK(status)) FREE_IOR(ior);
    } else {
    	struct NETLIBIOSBDEF myiosb;
    	status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_SETMODE, &myiosb,
    	    	    	    0, 0, 0, 0, 0,
    	    	    	    (argc > 1 && backlog != 0) ? *backlog : 4, 0, 0);
    	if (OK(status)) status = netlib___cvt_status(&myiosb);
    	if (argc > 2 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    }

    return status;

} /* netlib_listen */

/*
**++
**  ROUTINE:	netlib_accept
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
unsigned int netlib_accept (struct CTX **xctx, struct CTX **xnewctx,
    	    	    	    struct SINDEF *sa, unsigned int *sasize,
    	    	    	    unsigned int *salen, struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm) {
    struct CTX *ctx, *newctx;
    ITMLST sname;
    unsigned int status;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 2) return SS$_INSFARG;
    if (xnewctx == 0) return SS$_BADPARAM;
    status = netlib___alloc_ctx(&newctx, SPECCTX_SIZE);
    if (!OK(status)) return status;

    status = sys$assign(&inetdevice_v5, &newctx->chan, 0, 0);
    if (!OK(status)) {
    	status = sys$assign(&inetdevice, &newctx->chan, 0, 0);
    }
    if (!OK(status)) {
    	netlib___free_ctx(newctx);
    	return status;
    }

    if (argc > 6 && astadr != 0) {
    	struct IOR *ior;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 7) ? astprm : 0);
    	ior->spec_userfrom = (sasize == 0) ? 0 : sa;
    	ior->spec_length = (sasize == 0) ? 0 : *sasize;
    	ior->spec_retlen = salen;
    	ior->spec_xnewctx = xnewctx;
    	ior->spec_newctx = newctx;
    	ITMLST_INIT(sname, 0, sizeof(struct SINDEF),
    	    	&ior->specior.from, &ior->specior.fromlen);
    	ior->iorflags = IOR_M_COPY_FROM|IOR_M_NEW_CONTEXT;
    	status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_ACCESS|IO$M_ACCEPT,
    	    	    	    &ior->iosb, io_completion, ior,
    	    	    	    0, 0, &sname, &newctx->chan, 0, 0);
    	if (!OK(status)) FREE_IOR(ior);
    } else {
    	struct SINDEF from;
    	struct NETLIBIOSBDEF myiosb;
    	unsigned short fromlen;

    	ITMLST_INIT(sname, 0, sizeof(struct SINDEF),
    	    	&from, &fromlen);
    	status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_ACCESS|IO$M_ACCEPT,
    	    	    	    &myiosb, 0, 0, 0, 0,
    	    	    	    &sname, &newctx->chan, 0, 0);
    	if (OK(status)) status = netlib___cvt_status(&myiosb);
    	if (argc > 5 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    	if (OK(status)) {
    	    *xnewctx = newctx;
    	    if (argc > 3 && sa != 0 && sasize != 0) {
    	    	unsigned int len;
    	    	len = fromlen;
    	    	if (len > *sasize) len = *sasize;
    	    	memcpy(sa, &from, len);
    	    	if (argc > 4 && salen != 0) *salen = len;
    	    }
    	}
    }

    return status;

} /* netlib_accept */

/*
**++
**  ROUTINE:	netlib_setsockopt
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
unsigned int netlib_setsockopt (struct CTX **xctx,
    	    	    	    unsigned int *level, unsigned int *option,
    	    	    	    void *value, unsigned int *vallen,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    unsigned int status, lev;
    ITMLST2 optdsc;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 5) return SS$_INSFARG;
    if (option == 0 || value == 0 || vallen == 0) return SS$_BADPARAM;
    if (level == 0) lev = NETLIB_K_LEVEL_SOCKET;
    else lev = *level;

    if (lev == NETLIB_K_LEVEL_SOCKET && *option == NETLIB_K_OPTION_REUSEADDR) {
    	ctx->flags |= CTX_M_USER_SET_REUSEADDR;
    }

    if (argc > 6 && astadr != 0) {
    	struct IOR *ior;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 7) ? astprm : 0);
    	ITMLST2_INIT(ior->specior.sockopt, *option, *vallen, value);
    	ITMLST2_INIT(optdsc,
    	    (lev == NETLIB_K_LEVEL_SOCKET) ? UCX$C_SOCKOPT : lev,
    	    sizeof(ior->specior.sockopt), &ior->specior.sockopt);
    	status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_SETMODE, &ior->iosb,
    	    	    	    io_completion, ior, 0, 0, 0, 0, &optdsc, 0);
    	if (!OK(status)) FREE_IOR(ior);
    } else {
    	struct NETLIBIOSBDEF myiosb;
    	ITMLST2 sockopt;
    	ITMLST2_INIT(sockopt, *option, *vallen, value);
    	ITMLST2_INIT(optdsc,
    	    (lev == NETLIB_K_LEVEL_SOCKET) ? UCX$C_SOCKOPT : lev,
    	    sizeof(sockopt), &sockopt);
    	status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_SETMODE,
    	    	    	    &myiosb, 0, 0, 0, 0, 0, 0, &optdsc, 0);
    	if (OK(status)) status = netlib___cvt_status(&myiosb);
    	if (argc > 5 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    }

    return status;

} /* netlib_setsockopt */

/*
**++
**  ROUTINE:	netlib_getsockopt
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
unsigned int netlib_getsockopt (struct CTX **xctx,
    	    	    	    unsigned int *level, unsigned int *option,
    	    	    	    void *value, unsigned int *valsize,
    	    	    	    unsigned int *vallen,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    unsigned int status, lev;
    ITMLST2 optdsc;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 5) return SS$_INSFARG;
    if (option == 0 || value == 0 || valsize == 0) return SS$_BADPARAM;
    if (level == 0) lev = NETLIB_K_LEVEL_SOCKET;
    else lev = *level;


    if (argc > 7 && astadr != 0) {
    	struct IOR *ior;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 8) ? astprm : 0);
    	ITMLST_INIT(ior->specior.sockopt_get, *option, *valsize,
    	    	    	value, &ior->spec_length);
    	ITMLST2_INIT(optdsc,
    	    (lev == NETLIB_K_LEVEL_SOCKET) ? UCX$C_SOCKOPT : lev,
    	    sizeof(ior->specior.sockopt_get), &ior->specior.sockopt_get);
    	ior->spec_retlen = vallen;
    	ior->iorflags = IOR_M_COPY_LENGTH;
    	status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_SENSEMODE, &ior->iosb,
    	    	    	    io_completion, ior, 0, 0, 0, 0, 0, &optdsc);
    	if (!OK(status)) FREE_IOR(ior);
    } else {
    	ITMLST sockopt_get;
    	struct NETLIBIOSBDEF myiosb;
    	unsigned short length;

    	ITMLST_INIT(sockopt_get, *option, *valsize, value, &length);
    	ITMLST2_INIT(optdsc,
    	    (lev == NETLIB_K_LEVEL_SOCKET) ? UCX$C_SOCKOPT : lev,
    	    sizeof(sockopt_get), &sockopt_get);
    	status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_SENSEMODE,
    	    	    	    &myiosb, 0, 0, 0, 0, 0, 0, 0, &optdsc);
    	if (OK(status)) status = netlib___cvt_status(&myiosb);
    	if (argc > 6 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    	if (OK(status) && argc > 5 && vallen != 0) *vallen = length;
    }

    return status;

} /* netlib_getsockopt */

/*
**++
**  ROUTINE:	netlib_name_to_address
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Uses the UCX IO$_ACPCONTROL $QIO function to translate a host
**  name into one or more IP addresses.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NETLIB_NAME_TO_ADDRESS  ctx, which, namdsc, addrlist, listsize [,count] [,iosb] [,astadr] [,astprm]
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
unsigned int netlib_name_to_address (struct CTX **xctx, unsigned int *whichp,
    	    	    	    struct dsc$descriptor *namdsc,
    	    	    	    struct INADDRDEF *addrlist,
    	    	    	    unsigned int *listsize, unsigned int *count,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    struct INADDRDEF addr;
    struct NETLIBIOSBDEF myiosb;
    struct HOSTENT hostent;
    unsigned int status, subfunction;
    ITMLST2 subfdsc;
    ITMLST2 entdsc;
    unsigned short helen;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 5) return SS$_INSFARG;

    if (namdsc == 0 || addrlist == 0 || listsize == 0) return SS$_BADPARAM;

    if (OK(netlib_strtoaddr(namdsc, &addr))) {
    	addrlist[0] = addr;
    	if (argc > 5 && count != 0) *count = 1;
    	if (argc > 6 && iosb != 0) {
    	    iosb->iosb_w_status = SS$_NORMAL;
    	    iosb->iosb_w_count = 1;
    	    iosb->iosb_l_unused = 0;
    	}
    	if (argc > 7 && astadr != 0) {
    	    return sys$dclast(astadr, (argc > 8) ? astprm : 0, 0);
    	} else {
    	    return SS$_NORMAL;
    	}
    }

    if (argc > 7 && astadr != 0) {
    	struct IOR *ior;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 8) ? astprm : 0);
    	status = lib$get_vm(&hostent_size, &ior->spec_hostent);
    	if (!OK(status)) {
    	    FREE_IOR(ior);
    	    return status;
    	}
    	ior->specior.subfunction =
    	    (INETACP$C_HOSTENT_OFFSET << 8) | INETACP_FUNC$C_GETHOSTBYNAME;

    	ITMLST2_INIT(subfdsc, 0, sizeof(ior->specior.subfunction),
    	    	    &ior->specior.subfunction);
    	ITMLST2_INIT(entdsc, 0, hostent_size, ior->spec_hostent);
    	
    	ior->spec_useralist = addrlist;
    	ior->spec_length = *listsize;
    	ior->spec_retlen = count;
    	ior->iorflags = IOR_M_COPY_ADDRS;
    	status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_ACPCONTROL, &ior->iosb,
    	    	    	    io_completion, ior, &subfdsc, namdsc,
    	    	    	    &ior->specior.fromlen, &entdsc, 0, 0);
    	if (!OK(status)) FREE_IOR(ior);
    	return status;
    }

    subfunction =
    	    (INETACP$C_HOSTENT_OFFSET << 8) | INETACP_FUNC$C_GETHOSTBYNAME;

    ITMLST2_INIT(subfdsc, 0, sizeof(subfunction), &subfunction);
    ITMLST2_INIT(entdsc, 0, hostent_size, &hostent);
    status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_ACPCONTROL, &myiosb,
    	    	    	0, 0, &subfdsc, namdsc, &helen, &entdsc, 0, 0);
    if (OK(status)) status = netlib___cvt_status(&myiosb);
    if (argc > 6 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    if (OK(status)) {
    	char *base;
    	unsigned int *offlst;
    	int i;
    	base = (char *) &hostent;
    	i = 0;
    	if (hostent.addrlist_offset != 0) {
    	    offlst = (unsigned int *) (base+hostent.addrlist_offset);
    	    while (i < *listsize && offlst[i] != 0) {
    	    	addrlist[i] = *(struct INADDRDEF *) (base + offlst[i]);
    	    	i++;
    	    }
    	}
    	if (argc > 5 && count != 0) *count = i;
    }

    return status;
    
} /* netlib_name_to_address */

/*
**++
**  ROUTINE:	netlib_address_to_name
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Uses the UCX IO$_ACPCONTROL $QIO function to translate an
**  IP address into a host name.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NETLIB_ADDRESS_TO_NAME  ctx, which, addr, addrsize, namdsc [,retlen] [,iosb] [,astadr] [,astprm]
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
unsigned int netlib_address_to_name (struct CTX **xctx, unsigned int *whichp,
    	    	    	    struct INADDRDEF *addr,
    	    	    	    unsigned int *addrsize,
    	    	    	    struct dsc$descriptor *namdsc,
    	    	    	    unsigned short *retlen,
    	    	    	    struct NETLIBIOSBDEF *iosb,
    	    	    	    void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    struct NETLIBIOSBDEF myiosb;
    ITMLST2 subfdsc, entdsc, adrdsc;
    unsigned int status, subfunction;
    char buf[1024];
#ifdef TCPWARE
    char tmp[64];
    struct dsc$descriptor tmpdsc;
#endif /* TCPWARE */
    unsigned short length;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 5) return SS$_INSFARG;

    if (addr == 0 || addrsize == 0 || namdsc == 0) return SS$_BADPARAM;
    if (*addrsize != sizeof(struct INADDRDEF)) return SS$_BADPARAM;

#ifdef TCPWARE
    INIT_SDESC (tmpdsc, sizeof(tmp), tmp);
    status = netlib_addrtostr(addr, &tmpdsc, &tmpdsc.dsc$w_length);
    if (!OK(status)) return status;
#else
    ITMLST2_INIT(adrdsc, 0, *addrsize, addr);
#endif /* TCPWARE */
    if (argc > 7 && astadr != 0) {
    	struct IOR *ior;
    	struct HOSTENT *h;
    	GET_IOR(ior, ctx, iosb, astadr, (argc > 8) ? astprm : 0);
    	status = lib$get_vm(&hostent_size, &h);
    	if (!OK(status)) {
    	    FREE_IOR(ior);
    	    return status;
    	}
    	ior->spec_usrdsc = namdsc;
    	ior->spec_retlen = retlen;
    	ior->specior.subfunction =
#ifndef TCPWARE
    	    (INETACP$C_TRANS << 8) |
#endif /* not TCPWARE */
    	    INETACP_FUNC$C_GETHOSTBYADDR;

    	ITMLST2_INIT(subfdsc, 0, sizeof(ior->specior.subfunction),
    	    	    	    &ior->specior.subfunction);
    	ITMLST2_INIT(entdsc, 0, sizeof(h->buffer), h->buffer);
    	ior->spec_hostent = h;
    	ior->iorflags = IOR_M_COPY_HOSTNAME;
    	status = sys$qio(netlib_asynch_efn, ctx->chan, IO$_ACPCONTROL, &ior->iosb,
    	    	    	    io_completion, ior, &subfdsc,
#ifdef TCPWARE
    	    	    	    &tmpdsc,
#else
    	    	    	    &adrdsc,
#endif /* TCPWARE */
    	    	    	    &ior->specior.fromlen, &entdsc, 0, 0);
    	if (!OK(status)) FREE_IOR(ior);
    	return status;
    }

    subfunction =
#ifndef TCPWARE
    	    	  (INETACP$C_TRANS << 8) |
#endif
    	    	  INETACP_FUNC$C_GETHOSTBYADDR;

    ITMLST2_INIT(subfdsc, 0, sizeof(subfunction), &subfunction);
    ITMLST2_INIT(entdsc, 0, sizeof(buf), buf);
    status = sys$qiow(netlib_synch_efn, ctx->chan, IO$_ACPCONTROL, &myiosb,
    	    	    	0, 0, &subfdsc,
#ifdef TCPWARE
    	    	    	&tmpdsc,
#else
    	    	    	&adrdsc,
#endif /* TCPWARE */
    	    	    	&length, &entdsc, 0, 0);
    if (OK(status)) status = netlib___cvt_status(&myiosb);
    if (argc > 6 && iosb != 0) netlib___cvt_iosb(iosb, &myiosb);
    if (OK(status)) {
    	str$copy_r(namdsc, &length, buf);
    	if (argc > 5 && retlen != 0) *retlen = length;
    }

    return status;

} /* netlib_address_to_name */

/*
**++
**  ROUTINE:	NETLIB_GET_HOSTNAME
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Obtains the local host name configured for this TCP/IP package.
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
unsigned int netlib_get_hostname (void *bufdsc, unsigned short *retlenp) {

    char buf[256];
    unsigned int status;
    unsigned short retlen;
    int argc;

    SETARGCOUNT(argc);

    if (gethostname(buf, sizeof(buf)) < 0) return SS$_ABORT;
    retlen = strlen(buf);

    status = str$copy_r(bufdsc, &retlen, buf);
    if (!OK(status)) return status;
    if (argc > 1 && retlenp != 0) *retlenp = retlen;

    return SS$_NORMAL;

} /* netlib_get_hostname */

/*
**++
**  ROUTINE:	netlib___get_nameservers
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Obtains the list of DNS servers configured for this TCP/IP package.
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	NETLIB___GET_NAMESERVERS nsq
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
int netlib___get_nameservers (QUEUE *nsq) {

    struct NAMESERVER *ns;
    struct INADDRDEF a;
    struct CTX *tmpctx;
    struct dsc$descriptor dsc, lognamdsc;
    ITMLST lnmlst[2];
    char name[256], lognam[256], *cp, *anchor;
    unsigned int status, size, maxidx;
    unsigned short namlen;
    int index, i, remain;
    int v5, count;
    static unsigned int one = 1;

    tmpctx = 0;
    count = 0;

    ITMLST_INIT(lnmlst[0], LNM$_STRING, sizeof(name), name, &namlen);
    ITMLST_INIT(lnmlst[1], 0, 0, 0, 0);

    INIT_SDESC(lognamdsc, 0, lognam);

    for (v5 = 1; (count == 0) && (v5 >= 0); v5--) {
    	for (index = 0; index <= 16; index++) {
    	    lognamdsc.dsc$w_length = sprintf(lognam, (v5 == 0 ? "UCX$BIND_SERVER%03d" : "TCPIP$BIND_SERVER%03d"), index);
    	    status = sys$trnlnm(0, &nameserver_tabnam, &lognamdsc, 0, lnmlst);
    	    if (!OK(status) || namlen == 0) continue;
    	    for (anchor = name, remain = namlen; remain > 0;
    	    	    	    	    	remain -= i+1, anchor = cp+1) {
    	    	cp = memchr(anchor, ',', remain);
    	    	if (cp == 0) i = remain;
    	    	else i = cp - anchor;
    	    	INIT_SDESC(dsc, i, anchor);
    	    	if (!OK(netlib_strtoaddr(&dsc, &a))) {
    	    	    if (tmpctx == 0) {
    	    	    	if (!OK(netlib_socket(&tmpctx, 0, 0))) continue;
    	    	    }
    	    	    if (!OK(netlib_name_to_address(&tmpctx, 0, &dsc, &a, &one, 0,
    	    	    	    	0, 0, 0))) continue;
    	    	}
    	    	size = sizeof(struct NAMESERVER);
    	    	status = lib$get_vm(&size, &ns);
    	    	if (!OK(status)) break;
    	    	ns->addr = a;
    	    	queue_insert(ns, nsq->tail);
    	    	count += 1;
    	    }
    	}
    }

    if (tmpctx != 0) netlib_close(&tmpctx);
    return count;

} /* netlib___get_nameservers */

/*
**++
**  ROUTINE:	netlib___get_domain
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Returns the DNS domain name for this host, either derived from the
**  logical name provided by UCX, or by picking apart the local host name.
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	NETLIB___GET_DOMAIN  buf, bufsize, retlen
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
int netlib___get_domain (char *buf, unsigned short bufsize, unsigned short *retlen) {

    struct dsc$descriptor dsc;
    ITMLST lnmlst[2];
    char name[256], *cp;
    unsigned short namlen;

    static $DESCRIPTOR(lognamdsc, "UCX$BIND_DOMAIN");
    static $DESCRIPTOR(lognamdsc_v5, "TCPIP$BIND_DOMAIN");
    static $DESCRIPTOR(tabnam, "LNM$FILE_DEV");

    ITMLST_INIT(lnmlst[0], LNM$_STRING, sizeof(name), name, &namlen);
    ITMLST_INIT(lnmlst[1], 0, 0, 0, 0);

    if (OK(sys$trnlnm(0, &tabnam, &lognamdsc_v5, 0, lnmlst)))
    	cp = name;
    else if (OK(sys$trnlnm(0, &tabnam, &lognamdsc, 0, lnmlst)))
    	cp = name;
    else {
    	INIT_SDESC(dsc, sizeof(name), name);
    	if (!OK(netlib_get_hostname(&dsc, &namlen))) return 0;
    	cp = memchr(name, '.', namlen);
    	if (cp == 0) return 0;
    	cp += 1;
    	namlen -= (cp - name);
    }

    if (memchr(cp, '.', namlen) == 0) return 0;
    if (namlen > bufsize-1) namlen = bufsize-1;
    memcpy(buf, cp, namlen);
    *retlen = namlen;

    return 1;

} /* netlib___get_domain */

/*
**++
**  ROUTINE:	netlib_dns_mx_lookup
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Public entry point for MX RR lookup routine.  Simply passes
**  the call through to the NETLIB generic routine that performs its
**  own DNS queries.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NETLIB_DNS_MX_LOOKUP  ctx, namdsc, mxrr, mxrrsize [,mxrrcount] [,iosb] [,astadr] [,astprm]
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
unsigned int netlib_dns_mx_lookup(struct CTX **xctx,
    	    	    struct dsc$descriptor *namdsc, struct MXRRDEF *mxrr,
    	    	    unsigned int *mxrrsize, unsigned int *mxrrcount,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm) {

    struct CTX *ctx;
    int argc;

    VERIFY_CTX(xctx, ctx);
    SETARGCOUNT(argc);
    if (argc < 4) return SS$_INSFARG;

    return netlib___dns_mx_lookup(xctx, namdsc, mxrr, mxrrsize,
    	    (argc > 4) ? mxrrcount : 0, (argc > 5) ? iosb : 0,
    	    (argc > 6) ? astadr : 0, (argc > 7) ? astprm : 0);

} /* netlib_dns_mx_lookup */
