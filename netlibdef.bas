!+
!  NETLIBDEF.BAS
!
!  Definitions for use with NETLIB routines.
!
!  COPYRIGHT © 1993, 1997  MADGOAT SOFTWARE.  ALL RIGHTS RESERVED.
!
!  MODIFICATION HISTORY:
!
!  29-Sep-1993	    Madison 	Initial coding.
!  11-Mar-1997	    Madison 	DNS updates, courtesy Claudio Allocchio.
!  27-Nov-1997	    Madison 	Add NETLIB_M_FLUSH.
!  21-May-1998	    DBS		Created a BASIC version.
!-

declare long constant	NETLIB_K_TYPE_STREAM	= 1
declare long constant	NETLIB_K_TYPE_DGRAM	= 2

declare long constant	NETLIB_K_OPTION_REUSEADDR = 4
declare long constant	NETLIB_K_OPTION_KEEPALIVE = 8
declare long constant	NETLIB_K_OPTION_BROADCAST = 16

declare long constant	NETLIB_K_OPTION_SNDBUF	= X"1001"
declare long constant	NETLIB_K_OPTION_RCVBUF	= X"1002"
declare long constant	NETLIB_K_OPTION_SNDLOWAT = X"1003"
declare long constant	NETLIB_K_OPTION_RCVLOWAT = X"1004"

declare long constant	NETLIB_K_LEVEL_SOCKET	= X"FFFF"

declare long constant	NETLIB_K_AF_INET	= 2

declare long constant	NETLIB_K_LOOKUP_DNS	= 1
declare long constant	NETLIB_K_LOOKUP_HOST_TABLE = 2

declare long constant	NETLIB_K_SHUTDOWN_RECEIVER = 0
declare long constant	NETLIB_K_SHUTDOWN_SENDER = 1
declare long constant	NETLIB_K_SHUTDOWN_BOTH	= 2

declare long constant	NETLIB_M_ALLOW_LF	= 1
declare long constant	NETLIB_M_FLUSH		= 2

declare long constant	NETLIB_M_DOMAIN_SEARCH	= 1
declare long constant	NETLIB_M_NO_RECURSION	= 2

declare long constant	NETLIB_K_DNS_TYPE_A 	= X"01"
declare long constant	NETLIB_K_DNS_TYPE_NS	= X"02"
declare long constant	NETLIB_K_DNS_TYPE_MD	= X"03"
declare long constant	NETLIB_K_DNS_TYPE_MF	= X"04"
declare long constant	NETLIB_K_DNS_TYPE_CNAME	= X"05"
declare long constant	NETLIB_K_DNS_TYPE_SOA	= X"06"
declare long constant	NETLIB_K_DNS_TYPE_MB	= X"07"
declare long constant	NETLIB_K_DNS_TYPE_MG	= X"08"
declare long constant	NETLIB_K_DNS_TYPE_MR	= X"09"
declare long constant	NETLIB_K_DNS_TYPE_NULL	= X"0A"
declare long constant	NETLIB_K_DNS_TYPE_WKS	= X"0B"
declare long constant	NETLIB_K_DNS_TYPE_PTR	= X"0C"
declare long constant	NETLIB_K_DNS_TYPE_HINFO	= X"0D"
declare long constant	NETLIB_K_DNS_TYPE_MINFO	= X"0E"
declare long constant	NETLIB_K_DNS_TYPE_MX	= X"0F"
declare long constant	NETLIB_K_DNS_TYPE_TXT	= X"10"
declare long constant	NETLIB_K_DNS_TYPE_RP	= X"11"
declare long constant	NETLIB_K_DNS_TYPE_AFSDB	= X"12"
declare long constant	NETLIB_K_DNS_TYPE_X25	= X"13"
declare long constant	NETLIB_K_DNS_TYPE_ISDN	= X"14"
declare long constant	NETLIB_K_DNS_TYPE_RT	= X"15"
declare long constant	NETLIB_K_DNS_TYPE_NSAP	= X"16"
declare long constant	NETLIB_K_DNS_TYPE_NSAP_PTR = X"17"
declare long constant	NETLIB_K_DNS_TYPE_SIG	= X"18"
declare long constant	NETLIB_K_DNS_TYPE_KEY	= X"19"
declare long constant	NETLIB_K_DNS_TYPE_PX	= X"1A"
declare long constant	NETLIB_K_DNS_TYPE_GPOS	= X"1B"
declare long constant	NETLIB_K_DNS_TYPE_AAAA	= X"1C"
declare long constant	NETLIB_K_DNS_TYPE_LOC	= X"1D"

declare long constant	NETLIB_K_DNS_TYPE_UINFO	= X"64"
declare long constant	NETLIB_K_DNS_TYPE_UID	= X"65"
declare long constant	NETLIB_K_DNS_TYPE_GID	= X"66"
declare long constant	NETLIB_K_DNS_TYPE_UNSPEC = X"67"

declare long constant	NETLIB_K_DNS_TYPE_AXFR	= X"FC"
declare long constant	NETLIB_K_DNS_TYPE_MAILB	= X"FD"
declare long constant	NETLIB_K_DNS_TYPE_MAILA	= X"FE"

declare long constant	NETLIB_K_DNS_QTYPE_ALL	= X"FF"

declare long constant	NETLIB_K_DNS_CLASS_IN	= X"01"
declare long constant	NETLIB_K_DNS_CLASS_CS	= X"02"
declare long constant	NETLIB_K_DNS_CLASS_CH	= X"03"
declare long constant	NETLIB_K_DNS_CLASS_HS	= X"04"
declare long constant	NETLIB_K_DNS_QCLASS_ALL	= X"FF"

declare long constant	NETLIB_K_DNS_OP_STDQ	= 0
declare long constant	NETLIB_K_DNS_OP_INVQ	= 1
declare long constant	NETLIB_K_DNS_OP_STATUS	= 2

declare long constant	NETLIB_K_DNS_RC_SUCCESS	= 0
declare long constant	NETLIB_K_DNS_RC_FMTERR	= 1
declare long constant	NETLIB_K_DNS_RC_SRVFAIL	= 2
declare long constant	NETLIB_K_DNS_RC_NAMERR	= 3
declare long constant	NETLIB_K_DNS_RC_NOTIMP	= 4
declare long constant	NETLIB_K_DNS_RC_REFUSE	= 5

declare long constant	dns_s_header		= 12
declare long constant	dns_v_recursion_desired	= 1
declare long constant	dns_v_truncated		= 1
declare long constant	dns_v_authoritative	= 1
declare long constant	dns_v_opcode		= 4
declare long constant	dns_v_reply		= 1
declare long constant	dns_v_reply_code	= 4
declare long constant	dns_v_xx_unsused_xx	= 3
declare long constant	dns_v_recursion_available = 1

record NETLIB_DNS_HEADER
    variant
      case
	string	dns_t_header = dns_s_header
      case
	word	dns_w_queryid
	word	dns_w_flags
	word	dns_w_qdcount
	word	dns_w_ancount
	word	dns_w_nscount
	word	dns_w_arcount
    end variant
end record NETLIB_DNS_HEADER

declare long constant	sockaddr_s_sockaddrdef	= 16
declare long constant	sockaddr_s_data		= 14

record SOCKADDRDEF
    variant
      case
        string	sockaddr_t_addr = sockaddr_s_sockaddrdef
      case
	word	sockaddr_w_family
	byte	sockaddr_x_data (1 to sockaddr_s_data)
    end variant
end record SOCKADDRDEF

record INADDRDEF
	long	inaddr_l_addr
end record INADDRDEF

declare long constant	sin_s_sindef		= 16
declare long constant	sin_s_mbz		= 8

record SINDEF
    variant
      case
	string	sin_t_sin = sin_s_sindef
      case
	word	sin_w_family
	word	sin_w_port
	INADDRDEF sin_x_addr
	byte	sin_x_mbz (1 to sin_s_mbz)
    end variant
end record SINDEF

record NETLIBIOSBDEF
	word	iosb_w_status
	word	iosb_w_count
	long	iosb_l_unused
end record NETLIBIOSBDEF

declare long constant	NETLIB_S_MXRR_NAME	= 128

record MXRRDEF
	long	mxrr_l_preference
	long	mxrr_l_length
    	string	mxrr_t_name = NETLIB_S_MXRR_NAME
end record MXRRDEF

!#define netlib_word_swap(x) ((((x)>>8)&0xff)|(((x)&0xff)<<8 ))
!#define netlib_long_swap(x) ((((x)>>24)&0xff)|(((x)>>8)&0xff00)|(((x)&0xff00)<<8)|((x)<<24))

external long function	netlib_accept
external long function	netlib_address_to_name
external long function	netlib_addrtostr
external long function	netlib_bind
external long function	netlib_close
external long function	netlib_connect
external long function	netlib_connect_by_name
external long function	netlib_dns_expandname
external long function	netlib_dns_mx_lookup
external long function	netlib_dns_query
external long function	netlib_dns_skipname
external long function	netlib_get_hostname
external long function	netlib_getpeername
external long function	netlib_getsockname
external long function	netlib_getsockopt
external long function	netlib_hton_long
external long function	netlib_listen
external long function	netlib_name_to_address
external long function	netlib_ntoh_long
external long function	netlib_read
external long function	netlib_readline
external long function	netlib_server_setup
external long function	netlib_setsockopt
external long function	netlib_shutdown
external long function	netlib_socket
external long function	netlib_strtoaddr
external long function	netlib_version
external long function	netlib_write
external long function	netlib_writeline
external word function	netlib_hton_word
external word function	netlib_ntoh_word
