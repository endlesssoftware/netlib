/*[[ CMS REPLACEMENT HISTORY, Element NETLIBDEF.H]]*/
/*[[ *2     7-NOV-2004 15:37:58 MADISON "NETLIBDEF.R32"]]*/
/*[[ *1    12-MAR-1998 16:41:06 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element NETLIBDEF.H]]*/
/*
**  NETLIBDEF.H
**
**  Definitions for use with NETLIB routines.
**
**  COPYRIGHT © 1993, 1997, 2004  MADGOAT SOFTWARE.  ALL RIGHTS RESERVED.
**
**  MODIFICATION HISTORY:
**
**  29-Sep-1993	    Madison 	Initial coding.
**  11-Mar-1997	    Madison 	DNS updates, courtesy Claudio Allocchio.
**  27-Nov-1997	    Madison 	Add NETLIB_M_FLUSH.
**  07-Nov-2004     Madison     Add ALLOW_CR.
*/
#ifndef __NETLIBDEF_H_LOADED__
#define __NETLIBDEF_H_LOADED__

#define NETLIB_K_TYPE_STREAM	    	1
#define NETLIB_K_TYPE_DGRAM 	    	2

#define NETLIB_K_OPTION_REUSEADDR   0x04
#define NETLIB_K_OPTION_KEEPALIVE   0x08
#define NETLIB_K_OPTION_BROADCAST   0x20

#define NETLIB_K_OPTION_SNDBUF	    0x1001
#define NETLIB_K_OPTION_RCVBUF	    0x1002
#define NETLIB_K_OPTION_SNDLOWAT    0x1003
#define NETLIB_K_OPTION_RCVLOWAT    0x1004

#define NETLIB_K_LEVEL_SOCKET	    0xffff

#define NETLIB_K_AF_INET    	    	2

#define NETLIB_K_LOOKUP_DNS 	    	1
#define NETLIB_K_LOOKUP_HOST_TABLE  	2

#define NETLIB_K_SHUTDOWN_RECEIVER  	0
#define NETLIB_K_SHUTDOWN_SENDER    	1
#define NETLIB_K_SHUTDOWN_BOTH	    	2

#define NETLIB_M_ALLOW_LF   	    (1<<0)
#define NETLIB_M_FLUSH	    	    (1<<1)
#define NETLIB_M_ALLOW_CR           (1<<2)

#define NETLIB_M_DOMAIN_SEARCH	    (1<<0)
#define NETLIB_M_NO_RECURSION	    (1<<1)

#define NETLIB_K_DNS_TYPE_A 	  0x01
#define NETLIB_K_DNS_TYPE_NS	  0x02
#define NETLIB_K_DNS_TYPE_MD	  0x03
#define NETLIB_K_DNS_TYPE_MF	  0x04
#define NETLIB_K_DNS_TYPE_CNAME	  0x05
#define NETLIB_K_DNS_TYPE_SOA	  0x06
#define NETLIB_K_DNS_TYPE_MB	  0x07
#define NETLIB_K_DNS_TYPE_MG	  0x08
#define NETLIB_K_DNS_TYPE_MR	  0x09
#define NETLIB_K_DNS_TYPE_NULL	  0x0A
#define NETLIB_K_DNS_TYPE_WKS	  0x0B
#define NETLIB_K_DNS_TYPE_PTR	  0x0C
#define NETLIB_K_DNS_TYPE_HINFO	  0x0D
#define NETLIB_K_DNS_TYPE_MINFO	  0x0E
#define NETLIB_K_DNS_TYPE_MX	  0x0F
#define NETLIB_K_DNS_TYPE_TXT	  0x10
#define NETLIB_K_DNS_TYPE_RP      0x11
#define NETLIB_K_DNS_TYPE_AFSDB   0x12
#define NETLIB_K_DNS_TYPE_X25     0x13
#define NETLIB_K_DNS_TYPE_ISDN    0x14
#define NETLIB_K_DNS_TYPE_RT      0x15
#define NETLIB_K_DNS_TYPE_NSAP    0x16
#define NETLIB_K_DNS_TYPE_NSAP_PTR 0x17
#define NETLIB_K_DNS_TYPE_SIG     0x18
#define NETLIB_K_DNS_TYPE_KEY     0x19
#define NETLIB_K_DNS_TYPE_PX      0x1A
#define NETLIB_K_DNS_TYPE_GPOS    0x1B
#define NETLIB_K_DNS_TYPE_AAAA    0x1C
#define NETLIB_K_DNS_TYPE_LOC     0x1D

#define NETLIB_K_DNS_TYPE_UINFO   0x64
#define NETLIB_K_DNS_TYPE_UID     0x65
#define NETLIB_K_DNS_TYPE_GID     0x66
#define NETLIB_K_DNS_TYPE_UNSPEC  0x67

#define NETLIB_K_DNS_TYPE_AXFR    0xFC
#define NETLIB_K_DNS_TYPE_MAILB   0xFD
#define NETLIB_K_DNS_TYPE_MAILA   0xFE

#define NETLIB_K_DNS_QTYPE_ALL	      0xFF

#define NETLIB_K_DNS_CLASS_IN	  0x01
#define NETLIB_K_DNS_CLASS_CS	  0x02
#define NETLIB_K_DNS_CLASS_CH	  0x03
#define NETLIB_K_DNS_CLASS_HS	  0x04
#define NETLIB_K_DNS_QCLASS_ALL	  0xFF

#define NETLIB_K_DNS_OP_STDQ	  0
#define NETLIB_K_DNS_OP_INVQ	  1
#define NETLIB_K_DNS_OP_STATUS	  2

#define NETLIB_K_DNS_RC_SUCCESS	  0
#define NETLIB_K_DNS_RC_FMTERR	  1
#define NETLIB_K_DNS_RC_SRVFAIL	  2
#define NETLIB_K_DNS_RC_NAMERR	  3
#define NETLIB_K_DNS_RC_NOTIMP	  4
#define NETLIB_K_DNS_RC_REFUSE	  5

#if defined(__ALPHA) || defined(__ia64__)
#pragma member_alignment save
#pragma nomember_alignment
#endif

#pragma nostandard
    struct NETLIB_DNS_HEADER {
    	unsigned short dns_w_queryid;
    	variant_union {
    	    unsigned short dns_w_flags;
    	    variant_struct {
    	    	unsigned int dns_v_recursion_desired    : 1;
    	    	unsigned int dns_v_truncated    	: 1;
    	    	unsigned int dns_v_authoritative	: 1;
    	    	unsigned int dns_v_opcode	    	: 4;
    	    	unsigned int dns_v_reply	    	: 1;
    	    	unsigned int dns_v_reply_code   	: 4;
    	    	unsigned int dns_v_xx_unsused_xx	: 3;
    	    	unsigned int dns_v_recursion_available  : 1;
    	    } dns_x_flags;
    	} dns_r_flags_overlay;
    	unsigned short dns_w_qdcount;
    	unsigned short dns_w_ancount;
    	unsigned short dns_w_nscount;
    	unsigned short dns_w_arcount;
    };
#pragma standard


    struct SOCKADDRDEF {
    	unsigned short sockaddr_w_family;
    	unsigned char  sockaddr_x_data[14];
    };

    struct INADDRDEF {
    	unsigned long inaddr_l_addr;
    };

    struct SINDEF {
    	unsigned short sin_w_family;
    	unsigned short sin_w_port;
    	struct INADDRDEF sin_x_addr;
    	unsigned char sin_x_mbz[8];
    };

    struct NETLIBIOSBDEF {
    	unsigned short iosb_w_status;
    	unsigned short iosb_w_count;
    	unsigned long  iosb_l_unused;
    };

#define NETLIB_S_MXRR_NAME  128
    struct MXRRDEF {
    	unsigned int mxrr_l_preference;
    	unsigned int mxrr_l_length;
    	char	     mxrr_t_name[NETLIB_S_MXRR_NAME];
    };

#if defined(__ALPHA) || defined(__ia64__)
#pragma member_alignment restore
#endif

#define netlib_word_swap(x) ((((x)>>8)&0xff)|(((x)&0xff)<<8 ))
#define netlib_long_swap(x) ((((x)>>24)&0xff)|(((x)>>8)&0xff00)|(((x)&0xff00)<<8)|((x)<<24))

#ifndef __NETLIB_BUILD__

#ifdef __cplusplus
extern "C" {
#endif
    unsigned int netlib_socket(void **socket, ...);
    unsigned int netlib_server_setup(void **socket, void *sa, unsigned int *salen);
    unsigned int netlib_bind(void **socket, void *sa, unsigned int *salen, ...);
    unsigned int netlib_getsockname(void **socket, void *sa,
    	    	    unsigned int *sasize, unsigned int *salen, ...);
    unsigned int netlib_getpeername(void **socket, void *sa,
    	    	    unsigned int *sasize, unsigned int *salen, ...);
    unsigned int netlib_connect(void **socket, void *sa, unsigned int *salen, ...);
    unsigned int netlib_write(void **socket, void *bufdsc, ...);
    unsigned int netlib_writeline(void **socket, void *bufdsc, ...);
    unsigned int netlib_read(void **socket, void *bufdsc, ...);
    unsigned int netlib_readline(void **socket, void *bufdsc, ...);
    unsigned int netlib_shutdown(void **socket, ...);
    unsigned int netlib_close(void **socket, ...);
    unsigned int netlib_listen(void **socket, ...);
    unsigned int netlib_accept(void **socket, void **newsock, ...);
    unsigned int netlib_get_hostname(void *bufdsc, ...);
    unsigned int netlib_setsockopt(void **socket, unsigned int *level,
    	    	    unsigned int *option, void *value, unsigned int *vallen, ...);
    unsigned int netlib_getsockopt(void **socket, unsigned int *level,
    	    	    unsigned int *option, void *buffer, unsigned int *bufsize, ...);
    unsigned int netlib_name_to_address(void **socket, unsigned int *which, void *namdsc,
    	    	    void *addrlist, unsigned int *addrlistsize, ...);
    unsigned int netlib_address_to_name(void *socket, unsigned int *which, void *addr,
    	    	    unsigned int *addrsize, void *namdsc, ...);
    int netlib_dns_skipname(unsigned char *bufp, unsigned short *buflen);
    unsigned int netlib_dns_expandname(unsigned char *buf, unsigned short *buflen,
    	    	    unsigned char *bufp, void *namdsc, ...);
    unsigned int netlib_dns_query(void **socket, void *namdsc,
    	    	    unsigned int *class, unsigned int *type,
    	    	    unsigned char *buf, unsigned short *bufsize, ...);
    unsigned int netlib_strtoaddr(void *dsc, struct INADDRDEF *a);
    unsigned int netlib_addrtostr(struct INADDRDEF *a, void *dsc, ...);
    unsigned int netlib_connect_by_name(void **socket,
    	    	    void *dsc, unsigned short *port, ...);
    unsigned int netlib_dns_mx_lookup(void **socket, void *dsc,
    	    	    void *mxrr, unsigned int *mxrrsize, ...);
    unsigned int netlib_hton_long(unsigned int *value);
    unsigned int netlib_ntoh_long(unsigned int *value);
    unsigned short netlib_hton_word(unsigned short *value);
    unsigned short netlib_ntoh_word(unsigned short *value);
    unsigned int   netlib_version(void *dsc, ...);

#ifdef __cplusplus
}
#endif

#endif /* not __NETLIB_BUILD__ */
#endif /* __NETLIBDEF_H_LOADED__ */
