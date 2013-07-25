/*[[ CMS REPLACEMENT HISTORY, Element COMPATIBILITY.C]]*/
/*[[ *2     7-NOV-2004 15:37:55 MADISON ""]]*/
/*[[ *1    12-MAR-1998 16:40:55 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element COMPATIBILITY.C]]*/
/*
**++
**  FACILITY:	NETLIB
**
**  ABSTRACT:	Pre-V2 compatibility routines.
**
**  MODULE DESCRIPTION:
**
**  	This module contains wrapper routines for NETLIB V2 that implement
**  the V1 programming interface, for compatibility with V1-based applications.
**
**  N.B.: This compatibility may be removed in a future release of NETLIB.
**  	  Existing applications should migrate to the V2 API, and all new
**  	  applications should use only the V2 API.
**
**  AUTHOR: 	    M. Madison
**  	    	    COPYRIGHT © 1994, 1995, 2004  MADGOAT SOFTWARE.  ALL RIGHTS RESERVED.
**
**  CREATION DATE:  25-OCT-1994
**
**  MODIFICATION HISTORY:
**
**  	25-OCT-1994 V1.0    Madison 	Initial coding.
**  	13-NOV-1994 V1.1    Madison 	Another V1 compatibility hack -- for
**  	    	    	    	    	udp_receive.
**  	17-NOV-1994 V1.1-1  Madison 	A shutdown call on disconnect.
**  	11-JAN-1995 V1.1-2  Madison 	Fix shutdown call on disconnect;
**  	    	    	    	    	   add default timeout.
**  	11-JAN-1995 V1.1-3  Madison 	Fixed invocation of default timeout.
**  	17-FEB-1995 V1.1-4  Madison 	Allow SS$_LINKDISCON as success on
**  	    	    	    	    	    tcp_disconnect.
**      07-NOV-2004 V1.1-5  Madison     IA64 support.
**      25-JUL-2013 V1.1-6  Sneddon     Swap varargs.h for stdarg.h.
**--
*/
#include "netlibdef.h"
#include <ssdef.h>
#include <descrip.h>
#include <stsdef.h>
#include <stdarg.h>
#include <string.h>
#include <lib$routines.h>
#include <starlet.h>

/*
**  A few handy macros and structure definitions
*/
#define OK(x) $VMS_STATUS_SUCCESS(x)
#define VERIFY_CONTEXT(p,c) {if (p == 0) return SS$_BADPARAM; c = *p;}
#define SETARGCOUNT(x) va_count(x)

#if defined(__ALPHA) || defined(__ia64__)
#pragma member_alignment save
#pragma nomember_alignment
#endif
    typedef struct {unsigned int low, high;} TIME;
#if defined(__ALPHA) || defined(__ia64__)
#pragma member_alignment restore
#endif

/*
**  These constants are from the V1 programming interface.
*/
#define NET_K_TCP   1
#define NET_K_UDP   2
#define NET_M_PUSH  1
#define NET_M_NOTRM 2

/*
**  Context structure for tracking a V1-style "connection"
*/
    struct COMPATCTX {
    	struct NETLIBIOSBDEF iosb;
    	void *ctx;
    	unsigned int proto;
    	unsigned int rcvsinlen;
    	void (*astadr)();
    	void *astprm;
    	unsigned short *retlenp;
    	struct INADDRDEF *retaddr;
    	unsigned short *retport;
    	struct NETLIBIOSBDEF *user_iosb;
    	struct SINDEF rcvsin;
    };
/*
**  Forward declarations
*/
    unsigned int net_assign(struct COMPATCTX **xctx);
    unsigned int net_bind(struct COMPATCTX **xctx, unsigned int proto,
    	    	    unsigned short port, unsigned int threads,
    	    	    unsigned int notpass);
    unsigned int net_get_address(struct COMPATCTX **xctx,
    	    	    struct dsc$descriptor *host, unsigned int alsize,
    	    	    struct INADDRDEF *alist, unsigned int *alcount);
    unsigned int net_addr_to_name(struct COMPATCTX **xctx,
    	    	    struct INADDRDEF addr, struct dsc$descriptor *name);
    unsigned int net_deassign(struct COMPATCTX **xctx);
    unsigned int net_get_info(struct COMPATCTX **xctx,
    	    	    struct INADDRDEF *remadr, unsigned int *remport,
    	    	    struct INADDRDEF *lcladr, unsigned int *lclport);
    unsigned int net_get_hostname(struct dsc$descriptor *name, unsigned int *len);
    unsigned int tcp_connect(struct COMPATCTX **xctx,
    	    	    struct dsc$descriptor *node, unsigned int port);
    unsigned int tcp_connect_addr(struct COMPATCTX **xctx,
    	    	    struct INADDRDEF *addr, unsigned int port);
    unsigned int tcp_accept(struct COMPATCTX **xctx,
    	    	    struct COMPATCTX **newctx, struct NETLIBIOSBDEF *iosb,
    	    	    void (*astadr)(), void *astprm);
    unsigned int tcp_disconnect(struct COMPATCTX **xctx);
    unsigned int tcp_send(struct COMPATCTX **xctx, struct dsc$descriptor *str,
    	    	    unsigned int flags, struct NETLIBIOSBDEF *iosb,
    	    	    void (*astadr)(), void *astprm);
    unsigned int tcp_receive(struct COMPATCTX **xctx, struct dsc$descriptor *str,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm,
    	    	    TIME *timeout);
    unsigned int tcp_get_line(struct COMPATCTX **xctx, struct dsc$descriptor *str,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm,
    	    	    TIME *timeout);
    unsigned int udp_send(struct COMPATCTX **xctx, struct INADDRDEF addr,
    	    	    unsigned short port, void *buf, unsigned short buflen);
    unsigned int udp_receive(struct COMPATCTX **xctx, void *bufptr,
    	    	    unsigned short bufsize, unsigned short *buflen,
    	    	    struct INADDRDEF *srcaddr, unsigned int *srcport,
    	    	    TIME *timeout, struct NETLIBIOSBDEF *iosb,
    	    	    void (*astadr)(), void *astprm);
    static void udp_read_completion(struct COMPATCTX *ctx);
    unsigned int net_set_trace_routine(unsigned int (*routine)());
    unsigned int dns_mxlook(struct dsc$descriptor *name, unsigned int *size,
    	    	    	    	unsigned int *count, struct MXRRDEF *mxrr);
/*
**  OWN storage
*/
    static unsigned int ctxsize = sizeof(struct COMPATCTX);
    static unsigned int sinsize = sizeof(struct SINDEF);
    static unsigned int inasize = sizeof(struct INADDRDEF);
    static unsigned int which[2] = {NETLIB_K_LOOKUP_DNS, NETLIB_K_LOOKUP_HOST_TABLE};
    static unsigned int type[2] = {NETLIB_K_TYPE_STREAM, NETLIB_K_TYPE_DGRAM};

/*
**++
**  ROUTINE:	net_assign
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Creates a V1 API network context.  Under V1, this routine
**  would also assign a channel to the network device.  While this
**  no longer happens, applications should not be affected.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NET_ASSIGN  ctxptr
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	SS$_NORMAL, or any from LIB$GET_VM.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int net_assign (struct COMPATCTX **xctx) {

    unsigned int status;
    struct COMPATCTX *ctx;

    status = lib$get_vm(&ctxsize, &ctx);
    if (!OK(status)) return status;
    memset(ctx, 0, ctxsize);
    *xctx = ctx;

    return SS$_NORMAL;

} /* net_assign */

/*
**++
**  ROUTINE:	net_bind
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Binds a network context to a particular port.  For passive
**  connections, also establishes the context as a listener.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NETLIB_BIND  ctxptr, protocol [,port] [,backlog] [,notpassive]
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
unsigned int net_bind (struct COMPATCTX **xctx, unsigned int proto,
    	    	    unsigned short port, unsigned int threads,
    	    	    unsigned int notpass) {

    struct COMPATCTX *ctx;
    struct SINDEF sin;
    struct NETLIBIOSBDEF iosb;
    unsigned int status;
    int argc, passive;

/*
**  Make sure we have a valid context
*/
    VERIFY_CONTEXT(xctx, ctx);
/*
**  Make sure we have enough arguments
*/
    SETARGCOUNT(argc);
    if (argc < 2) return SS$_INSFARG;

/*
**  Use TCP if they specified an invalid protocol
*/
    if (proto < 1 || proto > 2) proto = 1;

/*
**  Create a V2 socket
*/
    status = netlib_socket(&ctx->ctx, &type[proto-1]);
    if (!OK(status)) return status;
/*
**  Passive mode?
*/
    passive = (argc > 4) ? !notpass : ((argc > 2 && port != 0) ? 1 : 0);

/*
**  Bind the V2 socket
*/
    memset(&sin, 0, sizeof(sin));
    sin.sin_w_family = NETLIB_K_AF_INET;
    sin.sin_w_port = (argc > 2) ? netlib_word_swap(port) : 0;

    status = netlib_bind(&ctx->ctx, &sin, &sinsize, &iosb);
    if (OK(status)) status = iosb.iosb_w_status;
    if (!OK(status)) {
    	netlib_close(&ctx->ctx);
    	ctx->ctx = 0;
    	return status;
    }

/*
**  For a passive open, establish the listener now
*/
    if (passive) {
    	status = netlib_listen(&ctx->ctx, (argc > 3) ? &threads : 0, &iosb);
    	if (OK(status)) status = iosb.iosb_w_status;
    	if (!OK(status)) {
    	    netlib_close(&ctx->ctx);
    	    ctx->ctx = 0;
    	}
    }

    return status;
 
} /* net_bind */

/*
**++
**  ROUTINE:	net_get_address
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Looks up a host name, returning an IP address.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NET_GET_ADDRESS  ctxptr, hostname, listsize, alist [,acount]
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
unsigned int net_get_address (struct COMPATCTX **xctx,
    	    	    struct dsc$descriptor *host, unsigned int alsize,
    	    	    struct INADDRDEF *alist, unsigned int *alcount) {

    struct COMPATCTX *ctx;
    unsigned int status;
    int argc, i, temp;

    VERIFY_CONTEXT(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 4) return SS$_INSFARG;
    if (host == 0 || alist == 0) return SS$_BADPARAM;

    if (ctx->ctx == 0) {
    	status = netlib_socket(&ctx->ctx);
    	if (!OK(status)) return status;
    	temp = 1;
    } else temp = 0;

    for (i = 0; i < 2; i++) {
    	status = netlib_name_to_address(&ctx->ctx, &which[i],
    	    	    	host, alist, &alsize, (argc > 4) ? alcount : 0);
    	if (OK(status)) break;
    }

    if (temp) {
    	netlib_close(&ctx->ctx);
    	ctx->ctx = 0;
    }

    return status;

} /* net_get_address */

/*
**++
**  ROUTINE:	net_addr_to_name
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Looks up an IP address, returning a host name.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NET_ADDR_TO_NAME  ctxptr, ip-address, hostname
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
unsigned int net_addr_to_name (struct COMPATCTX **xctx,
    	    	    struct INADDRDEF addr, struct dsc$descriptor *name) {
    struct COMPATCTX *ctx;
    unsigned int status;
    int argc, i, temp;

    VERIFY_CONTEXT(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 3) return SS$_INSFARG;
    if (name == 0) return SS$_BADPARAM;

    if (ctx->ctx == 0) {
    	status = netlib_socket(&ctx->ctx);
    	if (!OK(status)) return status;
    	temp = 1;
    } else temp = 0;

    for (i = 0; i < 2; i++) {
    	status = netlib_address_to_name(&ctx->ctx, &which[i], &addr, &inasize,
    	    	    	    	    	    	 name);
    	if (OK(status)) break;
    }

    if (temp) {
    	netlib_close(&ctx->ctx);
    	ctx->ctx = 0;
    }

    return status;

} /* net_addr_to_name */

/*
**++
**  ROUTINE:	net_deassign
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Closes down a V1 network context.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NET_DEASSIGN  ctxptr
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	SS$_NORMAL always returned.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int net_deassign (struct COMPATCTX **xctx) {

    struct COMPATCTX *ctx;

    VERIFY_CONTEXT(xctx, ctx);

    if (ctx->ctx != 0) {
    	netlib_close(&ctx->ctx);
    	ctx->ctx = 0;
    }

    lib$free_vm(&ctxsize, &ctx);

    *xctx = 0;

    return SS$_NORMAL;

} /* net_deassign */

/*
**++
**  ROUTINE:	net_get_info
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Gets the local and/or remote socket information (address and port)
**  for a connection.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NET_GET_INFO  ctxptr, [remadr] [,remport] [,lcladr] [,lclport]
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
unsigned int net_get_info (struct COMPATCTX **xctx,
    	    	    struct INADDRDEF *remadr, unsigned int *remport,
    	    	    struct INADDRDEF *lcladr, unsigned int *lclport) {

    struct COMPATCTX *ctx;
    struct SINDEF sin;
    unsigned int status;
    unsigned int retlen;
    int argc;

    VERIFY_CONTEXT(xctx, ctx);
    SETARGCOUNT(argc);
    if (argc < 2) return SS$_INSFARG;
    if (ctx->ctx == 0) return SS$_NOLINKS;

/*
**  Get the remote information, if they asked for it
*/
    if (remadr != 0 || (argc > 2 && remport != 0)) {
    	status = netlib_getpeername(&ctx->ctx, &sin, &sinsize, &retlen);
    	if (!OK(status)) return status;
    	if (argc > 2 && remport != 0) *remport = sin.sin_w_port;
    	if (remadr != 0) *remadr = sin.sin_x_addr;
    }

/*
**  Now get the local information, if they asked for it
*/
    if (argc > 3) {
    	status = netlib_getsockname(&ctx->ctx, &sin, &sinsize, &retlen);
    	if (OK(status)) {
    	    if (lcladr != 0) *lcladr = sin.sin_x_addr;
    	    if (argc > 4 && lclport != 0) *lclport = sin.sin_w_port;
    	}
    }

    return status;

} /* net_get_info */

/*
**++
**  ROUTINE:	net_get_hostname
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Returns the local IP host name.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NET_GET_HOSTNAME  hostname [,retlen]
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
unsigned int net_get_hostname (struct dsc$descriptor *name, unsigned int *len) {

    unsigned int status;
    unsigned short retlen;
    int argc;

    SETARGCOUNT(argc);
    status = netlib_get_hostname(name, &retlen);
    if (OK(status) && argc > 1 && len != 0) *len = retlen;

    return status;

} /* net_get_hostname */

/*
**++
**  ROUTINE:	tcp_connect
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Performs an active open to establish a TCP connection to
**  a remote system (by name).
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	TCP_CONNECT  ctxptr, hostname, port
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
unsigned int tcp_connect (struct COMPATCTX **xctx,
    	    	    struct dsc$descriptor *node, unsigned int port) {

    struct COMPATCTX *ctx;
    unsigned short p;
    unsigned int status;

    VERIFY_CONTEXT(xctx, ctx);

    if (ctx->ctx == 0) {
    	status = netlib_socket(&ctx->ctx, &type[ctx->proto-1]);
    	if (!OK(status)) return status;
    }
    p = port;
    return netlib_connect_by_name(&ctx->ctx, node, &p);

} /* tcp_connect */

/*
**++
**  ROUTINE:	tcp_connect_addr
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Performs an active open to establish a connection to
**  a remote system by address.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	TCP_CONNECT_ADDR  ctxptr, addrptr, port
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
unsigned int tcp_connect_addr (struct COMPATCTX **xctx,
    	    	    struct INADDRDEF *addr, unsigned int port) {

    struct COMPATCTX *ctx;
    struct SINDEF sin;
    unsigned int status;

    VERIFY_CONTEXT(xctx, ctx);

    if (ctx->ctx == 0) {
    	status = netlib_socket(&ctx->ctx, &type[ctx->proto-1]);
    	if (!OK(status)) return status;
    }

    memset(&sin, 0, sizeof(sin));
    sin.sin_w_family = NETLIB_K_AF_INET;
    sin.sin_w_port = netlib_word_swap(port);
    sin.sin_x_addr = *addr;
    return netlib_connect(&ctx->ctx, &sin, &sinsize);

} /* tcp_connect */

/*
**++
**  ROUTINE:	tcp_accept
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Accepts an incoming TCP connection (passive open completion).
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	TCP_ACCEPT  ctxptr, newctxptr [,iosb] [,astadr] [,astprm]
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
unsigned int tcp_accept (struct COMPATCTX **xctx,
    	    	    struct COMPATCTX **xnew, struct NETLIBIOSBDEF *iosb,
    	    	    void (*astadr)(), void *astprm) {

    struct COMPATCTX *ctx, *new;
    int argc;

    SETARGCOUNT(argc);
    if (argc < 2) return SS$_INSFARG;

    VERIFY_CONTEXT(xctx, ctx);
    VERIFY_CONTEXT(xnew, new);

    return netlib_accept(&ctx->ctx, &new->ctx, 0, 0, 0,
    	    	(argc > 2) ? iosb : 0, (argc > 3) ? astadr : 0,
    	    	(argc > 4) ? astprm : 0);

} /* tcp_accept */

/*
**++
**  ROUTINE:	tcp_disconnect
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Closes down a TCP connection.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	TCP_DISCONNECT  ctxptr
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
unsigned int tcp_disconnect (struct COMPATCTX **xctx) {

    struct COMPATCTX *ctx;
    unsigned int status;
    static unsigned int shuttype = NETLIB_K_SHUTDOWN_SENDER;

    VERIFY_CONTEXT(xctx, ctx);
    if (ctx->ctx == 0) return SS$_NORMAL;

    status = netlib_shutdown(&ctx->ctx, &shuttype);
    if (!OK(status) && status != SS$_LINKDISCON) return status;

    status = netlib_close(&ctx->ctx);
    if (OK(status)) ctx->ctx = 0;

    return status;

} /* tcp_disconnect */

/*
**++
**  ROUTINE:	tcp_send
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Sends data on a TCP connection, possibly followed by
**  a CR/LF sequence.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	TCP_SEND  ctxptr, strdsc [,flags] [,iosb] [,astadr] [,astprm]
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
unsigned int tcp_send (struct COMPATCTX **xctx, struct dsc$descriptor *str,
    	    	    	unsigned int flags, struct NETLIBIOSBDEF *iosb,
    	    	    	void (*astadr)(), void *astprm) {

    struct COMPATCTX *ctx;
    unsigned int f;
    int argc;

    VERIFY_CONTEXT(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 2) return SS$_INSFARG;
    if (str == 0) return SS$_BADPARAM;
    if (ctx->ctx == 0) return SS$_NOLINKS;

    f = (argc > 2) ? flags : 0;

    if (f & NET_M_NOTRM) {
    	return netlib_write(&ctx->ctx, str, 0, 0, (argc > 3) ? iosb : 0,
    	    	    (argc > 4) ? astadr : 0, (argc > 5) ? astprm : 0);
    } else {
    	return netlib_writeline(&ctx->ctx, str, (argc > 3) ? iosb : 0,
    	    	    (argc > 4) ? astadr : 0, (argc > 5) ? astprm : 0);
    }

} /* tcp_send */

/*
**++
**  ROUTINE:	tcp_receive
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Reads data off a TCP connection.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	TCP_RECEIVE  ctxptr, bufdsc [,iosb] [,astadr] [,astprm] [,timeout]
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
unsigned int tcp_receive (struct COMPATCTX **xctx, struct dsc$descriptor *str,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm,
    	    	    TIME *timeout) {

    struct COMPATCTX *ctx;
    struct dsc$descriptor dsc;
    unsigned int status;
    int argc;

    VERIFY_CONTEXT(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 2) return SS$_INSFARG;
    if (str == 0) return SS$_BADPARAM;
    if (ctx->ctx == 0) return SS$_NOLINKS;

/*
**  HACK to support broken MGFTP code, which passes us a static descriptor
**  which is set up as a dynamic descriptor!
*/
    status = lib$analyze_sdesc(str, &dsc.dsc$w_length, &dsc.dsc$a_pointer);
    dsc.dsc$b_dtype = DSC$K_DTYPE_T;
    dsc.dsc$b_class = DSC$K_CLASS_S;

    return netlib_read(&ctx->ctx, &dsc, 0, 0, 0, (argc > 5) ? timeout : 0,
    	    	    	(argc > 2) ? iosb : 0, (argc > 3) ? astadr : 0,
    	    	    	(argc > 4) ? astprm : 0);

} /* tcp_receive */

/*
**++
**  ROUTINE:	tcp_get_line
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Reads a "line" of data (a string of characters which is terminated
**  with a CR/LF sequence) from the TCP connection.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	TCP_GET_LINE  ctxptr, bufdsc [,iosb] [,astadr] [,astprm], [,timeout]
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
unsigned int tcp_get_line (struct COMPATCTX **xctx, struct dsc$descriptor *str,
    	    	    struct NETLIBIOSBDEF *iosb, void (*astadr)(), void *astprm,
    	    	    TIME *timeout) {
    struct COMPATCTX *ctx;
    static unsigned int flags = NETLIB_M_ALLOW_LF;
    static TIME default_timeout = {1,0};
    static int did_timeout = 0;
    static $DESCRIPTOR(default_timeoutdsc, "0 00:10:00.00");
    int argc;

    VERIFY_CONTEXT(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 2) return SS$_INSFARG;
    if (str == 0) return SS$_BADPARAM;
    if (ctx->ctx == 0) return SS$_NOLINKS;

    if (!did_timeout) did_timeout = OK(sys$bintim(&default_timeoutdsc, &default_timeout));

    return netlib_readline(&ctx->ctx, str, 0, &flags,
    	    	    	(argc > 5 && timeout != 0) ? timeout : &default_timeout,
    	    	    	(argc > 2) ? iosb : 0, (argc > 3) ? astadr : 0,
    	    	    	(argc > 4) ? astprm : 0);

} /* tcp_get_line */

/*
**++
**  ROUTINE:	udp_send
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Sends a UDP datagram.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	UDP_SEND  ctxptr, address, port, bufptr, buflen
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
unsigned int udp_send (struct COMPATCTX **xctx, struct INADDRDEF addr,
    	    	unsigned short port, void *buf, unsigned short buflen) {

    struct COMPATCTX *ctx;
    struct SINDEF sin;
    struct dsc$descriptor dsc;

    VERIFY_CONTEXT(xctx, ctx);
    if (ctx->ctx == 0) return SS$_NOLINKS;

    dsc.dsc$b_dtype = DSC$K_DTYPE_T;
    dsc.dsc$b_class = DSC$K_CLASS_S;
    dsc.dsc$w_length = buflen;
    dsc.dsc$a_pointer = buf;
    memset(&sin, 0, sizeof(sin));
    sin.sin_w_family = NETLIB_K_AF_INET;
    sin.sin_x_addr = addr;
    sin.sin_w_port = netlib_word_swap(port);

    return netlib_write(&ctx->ctx, &dsc, &sin, &sinsize);

} /* udp_send */

/*
**++
**  ROUTINE:	udp_receive
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Receives a UDP datagram.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	UDP_RECEIVE  ctxptr, bufptr, bufsize [,retlen] [,srcaddr]
**  	    	     [,srcport] [,timeout] [,iosb] [,astadr] [,astprm]
**
**  N.B.:  for synchronous calls, srcport should be the address of a
**  	   longword.  For asynchronous calls, it should be the address
**  	   of a word.
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
unsigned int udp_receive (struct COMPATCTX **xctx, void *bufptr,
    	    	    unsigned short bufsize, unsigned short *buflen,
    	    	    struct INADDRDEF *srcaddr, unsigned int *srcport,
    	    	    TIME *timeout, struct NETLIBIOSBDEF *iosb,
    	    	    void (*astadr)(), void *astprm) {

    struct COMPATCTX *ctx;
    struct SINDEF sin;
    struct dsc$descriptor dsc;
    unsigned int status;
    int argc;

    VERIFY_CONTEXT(xctx, ctx);
    SETARGCOUNT(argc);

    if (argc < 3) return SS$_INSFARG;
    if (bufptr == 0) return SS$_BADPARAM;
    if (ctx->ctx == 0) return SS$_NOLINKS;

    dsc.dsc$b_dtype = DSC$K_DTYPE_T;
    dsc.dsc$b_class = DSC$K_CLASS_S;
    dsc.dsc$a_pointer = bufptr;
    dsc.dsc$w_length = bufsize;

    if (argc > 8 && astadr != 0) {
    	ctx->retaddr = srcaddr;      
    	ctx->retport = (unsigned short *) srcport;
    	ctx->retlenp = buflen;
    	ctx->user_iosb = iosb;
    	ctx->astadr = astadr;
    	ctx->astprm = (argc > 9) ? astprm : 0;

    	return netlib_read(&ctx->ctx, &dsc, &ctx->rcvsin, &sinsize,
    	    	    	    &ctx->rcvsinlen, timeout, &ctx->iosb,
    	    	    	    udp_read_completion, ctx);
    }

    status = netlib_read(&ctx->ctx, &dsc, &ctx->rcvsin, &sinsize,
    	    	&ctx->rcvsinlen, (argc > 6) ? timeout : 0, &ctx->iosb);

    if (OK(status)) {
    	if (argc > 3 && buflen != 0) *buflen = ctx->iosb.iosb_w_count;
    	if (argc > 4 && srcaddr != 0) *srcaddr = ctx->rcvsin.sin_x_addr;
    	if (argc > 5 && srcport != 0)
    	    *srcport = netlib_word_swap(ctx->rcvsin.sin_w_port);
    }
    if (argc > 7 && iosb != 0) memcpy(iosb, &ctx->iosb, sizeof(ctx->iosb));

    return status;

} /* udp_receive */

/*
**++
**  ROUTINE:	udp_read_completion
**
**  FUNCTIONAL DESCRIPTION:
**
**  	AST completion routine for asynchronous UDP_READ calls.  Does some
**  pre-processing before invoking the caller's AST routine.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	UDP_READ_COMPLETION  ctx
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
static void udp_read_completion (struct COMPATCTX *ctx) {

/*
**  Stash the originating address and port, if the caller wanted them
*/
    if (OK(ctx->iosb.iosb_w_status)) {
    	if (ctx->retaddr != 0) *(ctx->retaddr) = ctx->rcvsin.sin_x_addr;
    	if (ctx->retport != 0)
    	    *(ctx->retport) = netlib_word_swap(ctx->rcvsin.sin_w_port);
    	if (ctx->retlenp != 0) *(ctx->retlenp) = ctx->iosb.iosb_w_count;
    }

/*
**  Stash the IOSB, if it was wanted
*/
    if (ctx->user_iosb != 0) memcpy(ctx->user_iosb, &ctx->iosb, sizeof(ctx->iosb));

/*
**  Invoke the caller's AST routine
*/
    (*ctx->astadr)(ctx->astprm);

} /* udp_read_completion */

/*
**++
**  ROUTINE:	net_set_trace_routine
**
**  FUNCTIONAL DESCRIPTION:
**
**  	This routine was used in the V1 interface for tracing activity
**  of the MX lookup routine.  It no longer has any effect.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NET_SET_TRACE_ROUTINE  rtnptr
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	SS$_NORMAL always returned.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int net_set_trace_routine (unsigned int (*routine)()) {

    return SS$_NORMAL;

} /* net_set_trace_routine */

/*
**++
**  ROUTINE:	dns_mxlook
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Looks up MX RR's in the DNS for a domain name.  Returns them
**  in an array of special structures (the same structure is used for
**  the V2 equivalent of this routine).
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	DNS_MXLOOK  name, mxrrsize [,mxrrcount], mxrrptr
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
unsigned int dns_mxlook(struct dsc$descriptor *name, unsigned int *size,
    	    	    	    unsigned int *count, struct MXRRDEF *mxrr) {

    void *ctx;
    unsigned int status;

    status = netlib_socket(&ctx);
    if (!OK(status)) return status;
    status = netlib_dns_mx_lookup(&ctx, name, mxrr, size, count);
    netlib_close(&ctx);

    return status;

} /* dns_mxlook */
