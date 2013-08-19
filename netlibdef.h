/********************************************************************************************************************************/
/* Created: 19-Aug-2013 12:49:20 by OpenVMS SDL EV2-3      */
/* Source:  19-AUG-2013 12:48:51 MG_SRC:[NETLIB]NETLIBDEF.SDL;69 */
/********************************************************************************************************************************/
/*** MODULE NETLIBDEF ***/
#ifndef __NETLIBDEF_LOADED
#define __NETLIBDEF_LOADED 1
 
#pragma __nostandard			 /* This file uses non-ANSI-Standard features */
#ifdef __INITIAL_POINTER_SIZE			 /* Defined whenever ptr size pragmas supported */
#pragma __required_pointer_size __save		 /* Save the previously-defined required ptr size */
#pragma __required_pointer_size __short		 /* And set ptr size default to 32-bit pointers */
#endif
 
#ifdef __cplusplus
    extern "C" {
#define __unknown_params ...
#define __optional_params ...
#else
#define __unknown_params
#define __optional_params ...
#endif
 
#ifndef __struct
#if !defined(__VAXC)
#define __struct struct
#else
#define __struct variant_struct
#endif
#endif
 
#ifndef __union
#if !defined(__VAXC)
#define __union union
#else
#define __union variant_union
#endif
#endif
 
#ifndef __NETLIBDEF_H_LOADED__
#define __NETLIBDEF_H_LOADED__
#define NETLIB_K_TYPE_STREAM 1
#define NETLIB_K_TYPE_DGRAM 2
#define NETLIB_K_OPTION_REUSEADDR 4
#define NETLIB_K_OPTION_KEEPALIVE 8
#define NETLIB_K_OPTION_BROADCAST 32
#define NETLIB_K_OPTION_SNDBUF 4097
#define NETLIB_K_OPTION_RCVBUF 4098
#define NETLIB_K_OPTION_SNDLOWAT 4099
#define NETLIB_K_OPTION_RCVLOWAT 4100
#define NETLIB_K_LEVEL_SOCKET 65535
#define NETLIB_K_AF_INET 2
#define NETLIB_K_LOOKUP_DNS 1
#define NETLIB_K_LOOKUP_HOST_TABLE 2
#define NETLIB_K_SHUTDOWN_RECEIVER 0
#define NETLIB_K_SHUTDOWN_SENDER 1
#define NETLIB_K_SHUTDOWN_BOTH 2
#define NETLIB_M_ALLOW_LF 1
#define NETLIB_M_FLUSH 2
#define NETLIB_M_ALLOW_CR 4
#define NETLIB_M_DOMAIN_SEARCH 1
#define NETLIB_M_NO_RECURSION 2
#define NETLIB_K_DNS_TYPE_A 1
#define NETLIB_K_DNS_TYPE_NS 2
#define NETLIB_K_DNS_TYPE_MD 3
#define NETLIB_K_DNS_TYPE_MF 4
#define NETLIB_K_DNS_TYPE_CNAME 5
#define NETLIB_K_DNS_TYPE_SOA 6
#define NETLIB_K_DNS_TYPE_MB 7
#define NETLIB_K_DNS_TYPE_MG 8
#define NETLIB_K_DNS_TYPE_MR 9
#define NETLIB_K_DNS_TYPE_NULL 10
#define NETLIB_K_DNS_TYPE_WKS 11
#define NETLIB_K_DNS_TYPE_PTR 12
#define NETLIB_K_DNS_TYPE_HINFO 13
#define NETLIB_K_DNS_TYPE_MINFO 14
#define NETLIB_K_DNS_TYPE_MX 15
#define NETLIB_K_DNS_TYPE_TXT 16
#define NETLIB_K_DNS_TYPE_RP 17
#define NETLIB_K_DNS_TYPE_AFSDB 18
#define NETLIB_K_DNS_TYPE_X25 19
#define NETLIB_K_DNS_TYPE_ISDN 20
#define NETLIB_K_DNS_TYPE_RT 21
#define NETLIB_K_DNS_TYPE_NSAP 22
#define NETLIB_K_DNS_TYPE_NSAP_PTR 23
#define NETLIB_K_DNS_TYPE_SIG 24
#define NETLIB_K_DNS_TYPE_KEY 25
#define NETLIB_K_DNS_TYPE_PX 26
#define NETLIB_K_DNS_TYPE_GPOS 27
#define NETLIB_K_DNS_TYPE_AAAA 28
#define NETLIB_K_DNS_TYPE_LOC 29
#define NETLIB_K_DNS_TYPE_UINFO 100
#define NETLIB_K_DNS_TYPE_UID 101
#define NETLIB_K_DNS_TYPE_GID 102
#define NETLIB_K_DNS_TYPE_UNSPEC 103
#define NETLIB_K_DNS_TYPE_AXFR 252
#define NETLIB_K_DNS_TYPE_MAILB 253
#define NETLIB_K_DNS_TYPE_MAILA 254
#define NETLIB_K_DNS_QTYPE_ALL 255
#define NETLIB_K_DNS_CLASS_IN 1
#define NETLIB_K_DNS_CLASS_CS 2
#define NETLIB_K_DNS_CLASS_CH 3
#define NETLIB_K_DNS_CLASS_HS 4
#define NETLIB_K_DNS_QCLASS_ALL 255
#define NETLIB_K_DNS_OP_STDQ 0
#define NETLIB_K_DNS_OP_INVQ 1
#define NETLIB_K_DNS_OP_STATUS 2
#define NETLIB_K_DNS_RC_SUCCESS 0
#define NETLIB_K_DNS_RC_FMTERR 1
#define NETLIB_K_DNS_RC_SRVFAIL 2
#define NETLIB_K_DNS_RC_NAMERR 3
#define NETLIB_K_DNS_RC_NOTIMP 4
#define NETLIB_K_DNS_RC_REFUSE 5
	
#define INADDRDEF inaddrdef
#define MXRRDEF mxrrdef
#define NETLIBIOSBDEF netlibiosbdef
#define NETLIB_DNS_HEADER netlib_dns_header
#define SINDEF sindef
#if defined(__ALPHA) || defined(__ia64__)
#pragma member_alignment save
#pragma nomember_alignment
#endif
#pragma nostandard
#define DNS_M_RECURSION_DESIRED 0x1
#define DNS_M_TRUNCATED 0x1
#define DNS_M_AUTHORITATIVE 0x1
#define DNS_M_OPCODE 0xF
#define DNS_M_REPLY 0x1
#define DNS_M_REPLY_CODE 0xF
#define DNS_M_XX_UNUSED_XX 0x7
#define DNS_M_RECURSION_AVAILABLE 0x1
	
struct netlib_dns_header {
    unsigned short int dns_w_queryid;
    __union  {
        unsigned short int dns_w_flags;
        __union  {
            unsigned dns_v_recursion_desired : 1;
            unsigned dns_v_truncated : 1;
            unsigned dns_v_authoritative : 1;
            unsigned dns_v_opcode : 4;
            unsigned dns_v_reply : 1;
            unsigned dns_v_reply_code : 4;
            unsigned dns_v_xx_unused_xx : 3;
            unsigned dns_v_recursion_available : 1;
            } dns_x_flags;
        } dns_r_flags_overlay;
    unsigned short int dns_w_qdcount;
    unsigned short int dns_w_ancount;
    unsigned short int dns_w_nscount;
    unsigned short int dns_w_arcount;
    } ;
 
#if !defined(__VAXC)
#define dns_w_flags dns_r_flags_overlay.dns_w_flags
#define dns_x_flags	dns_r_flags_overlay.dns_x_flags
#define dns_v_recursion_desired dns_x_flags.dns_v_recursion_desired
#define dns_v_truncated dns_x_flags.dns_v_truncated
#define dns_v_authoritative dns_x_flags.dns_v_authoritative
#define dns_v_opcode dns_x_flags.dns_v_opcode
#define dns_v_reply dns_x_flags.dns_v_reply
#define dns_v_reply_code dns_x_flags.dns_v_reply_code
#define dns_v_xx_unused_xx dns_x_flags.dns_v_xx_unused_xx
#define dns_v_recursion_available dns_x_flags.dns_v_recursion_available
#endif		/* #if !defined(__VAXC) */
 
	
#pragma standard
#define SOCKADDR_S_DATA 14
	
struct sockaddrdef {
    unsigned short int sockaddr_w_family;
    unsigned char sockaddr_x_data [14];
    } ;
	
struct inaddrdef {
    unsigned int inaddr_l_addr;
    } ;
#define SIN_S_MBZ 8
	
struct sindef {
    unsigned short int sin_w_family;
    unsigned short int sin_w_port;
    struct INADDRDEF sin_x_addr;
    unsigned char sin_x_mbz [8];
    } ;
	
struct netlibiosbdef {
    unsigned short int iosb_w_status;
    unsigned short int iosb_w_count;
    unsigned int iosb_l_unused;
    } ;
#define NETLIB_S_MXRR_NAME 128
	
struct mxrrdef {
    unsigned int mxrr_l_preference;
    unsigned int mxrr_l_length;
    char mxrr_t_name [128];
    } ;
	
#define NETLIB_K_METHOD_ANY 0
#define NETLIB_K_METHOD_SSL2 1
#define NETLIB_K_METHOD_SSL3 2
#define NETLIB_K_METHOD_TLS1 3
#define NETLIB_K_FILETYPE_PEM 1
#define NETLIB_K_FILETYPE_ASN1 2
	
#define netlib_word_swap(x) ((((x)>>8)&0xff)|(((x)&0xff)<<8 ))
#define netlib_long_swap(x) ((((x)>>24)&0xff)|(((x)>>8)&0xff00)|(((x)&0xff00)<<8)|((x)<<24))
#ifndef __NETLIB_BUILD__
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
    unsigned int netlib_ssl_socket(void **sslsock, void **socket, void **ssl);
    unsigned int netlib_ssl_accept(void **sslsock, ...);
    unsigned int netlib_ssl_connect(void **sslsock, ...);
    unsigned int netlib_ssl_shutdown(void **sslsock, ...);
    unsigned int netlib_ssl_read(void **sslsock, void *bufdsc, ...);
    unsigned int netlib_ssl_write(void **sslsock, void *bufdsc, ...);
    unsigned int netlib_ssl_version(void *dsc, ...);
#endif /* not __NETLIB_BUILD__ */
	
	
	
#endif /* __NETLIBDEF_H_LOADED__ */
 
#ifdef __INITIAL_POINTER_SIZE			 /* Defined whenever ptr size pragmas supported */
#pragma __required_pointer_size __restore		 /* Restore the previously-defined required ptr size */
#endif
#ifdef __cplusplus
    }
#endif
#pragma __standard
 
#endif /* __NETLIBDEF_LOADED */
 
