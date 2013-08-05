(********************************************************************************************************************************)
(* Created:  5-Aug-2013 22:08:24 by OpenVMS SDL EV2-3      *)
(* Source:  05-AUG-2013 22:08:05 MG_SRC:[NETLIB]NETLIBDEF.SDL;26 *)
(********************************************************************************************************************************)
 
MODULE NETLIBDEF ;
 
[HIDDEN] TYPE	(**** Pre-declared data types ****)
	$BYTE = [BYTE] -128..127;
	$WORD = [WORD] -32768..32767;
	$QUAD = [QUAD,UNSAFE] RECORD
		L0:UNSIGNED; L1:INTEGER; END;
	$OCTA = [OCTA,UNSAFE] RECORD
		L0,L1,L2:UNSIGNED; L3:INTEGER; END;
	$UBYTE = [BYTE] 0..255;
	$UWORD = [WORD] 0..65535;
	$UQUAD = [QUAD,UNSAFE] RECORD
		L0,L1:UNSIGNED; END;
	$UOCTA = [OCTA,UNSAFE] RECORD
		L0,L1,L2,L3:UNSIGNED; END;
	$UOCTAQUAD = [OCTA(2),UNSAFE] RECORD
		L0,L1,L2,L3,L4,L5,L6,L7:UNSIGNED; END;
	$PACKED_DEC = [BIT(4),UNSAFE] 0..15;
	$DEFTYP = [UNSAFE] INTEGER;
	$DEFPTR = [UNSAFE] ^$DEFTYP;
	$BOOL = [BIT(1),UNSAFE] BOOLEAN;
	$BIT2 = [BIT(2),UNSAFE] 0..3;
	$BIT3 = [BIT(3),UNSAFE] 0..7;
	$BIT4 = [BIT(4),UNSAFE] 0..15;
	$BIT5 = [BIT(5),UNSAFE] 0..31;
	$BIT6 = [BIT(6),UNSAFE] 0..63;
	$BIT7 = [BIT(7),UNSAFE] 0..127;
	$BIT8 = [BIT(8),UNSAFE] 0..255;
	$BIT9 = [BIT(9),UNSAFE] 0..511;
	$BIT10 = [BIT(10),UNSAFE] 0..1023;
	$BIT11 = [BIT(11),UNSAFE] 0..2047;
	$BIT12 = [BIT(12),UNSAFE] 0..4095;
	$BIT13 = [BIT(13),UNSAFE] 0..8191;
	$BIT14 = [BIT(14),UNSAFE] 0..16383;
	$BIT15 = [BIT(15),UNSAFE] 0..32767;
	$BIT16 = [BIT(16),UNSAFE] 0..65535;
	$BIT17 = [BIT(17),UNSAFE] 0..131071;
	$BIT18 = [BIT(18),UNSAFE] 0..262143;
	$BIT19 = [BIT(19),UNSAFE] 0..524287;
	$BIT20 = [BIT(20),UNSAFE] 0..1048575;
	$BIT21 = [BIT(21),UNSAFE] 0..2097151;
	$BIT22 = [BIT(22),UNSAFE] 0..4194303;
	$BIT23 = [BIT(23),UNSAFE] 0..8388607;
	$BIT24 = [BIT(24),UNSAFE] 0..16777215;
	$BIT25 = [BIT(25),UNSAFE] 0..33554431;
	$BIT26 = [BIT(26),UNSAFE] 0..67108863;
	$BIT27 = [BIT(27),UNSAFE] 0..134217727;
	$BIT28 = [BIT(28),UNSAFE] 0..268435455;
	$BIT29 = [BIT(29),UNSAFE] 0..536870911;
	$BIT30 = [BIT(30),UNSAFE] 0..1073741823;
	$BIT31 = [BIT(31),UNSAFE] 0..2147483647;
	$BIT32 = [BIT(32),UNSAFE] UNSIGNED;
 
(*** MODULE NETLIBDEF ***)
 
[HIDDEN] TYPE	(**** SDL-Generated type names ****)
	NETLIBDEF$$TYP1 = ^NETLIBIOSB$TYPE;
	NETLIBDEF$$TYP2 = ^NETLIBIOSB$TYPE;
	NETLIBDEF$$TYP3 = ^NETLIBIOSB$TYPE;
	NETLIBDEF$$TYP4 = ^NETLIBIOSB$TYPE;
	NETLIBDEF$$TYP5 = ^NETLIBIOSB$TYPE;
 
 
CONST	NETLIB_K_TYPE_STREAM = 1;
	NETLIB_K_TYPE_DGRAM = 2;
	NETLIB_K_OPTION_REUSEADDR = 4;
	NETLIB_K_OPTION_KEEPALIVE = 8;
	NETLIB_K_OPTION_BROADCAST = 32;
	NETLIB_K_OPTION_SNDBUF = 4097;
	NETLIB_K_OPTION_RCVBUF = 4098;
	NETLIB_K_OPTION_SNDLOWAT = 4099;
	NETLIB_K_OPTION_RCVLOWAT = 4100;
	NETLIB_K_LEVEL_SOCKET = 65535;
	NETLIB_K_AF_INET = 2;
	NETLIB_K_LOOKUP_DNS = 1;
	NETLIB_K_LOOKUP_HOST_TABLE = 2;
	NETLIB_K_SHUTDOWN_RECEIVER = 0;
	NETLIB_K_SHUTDOWN_SENDER = 1;
	NETLIB_K_SHUTDOWN_BOTH = 2;
	NETLIB_M_ALLOW_LF = 1;
	NETLIB_M_FLUSH = 2;
	NETLIB_M_ALLOW_CR = 4;
	NETLIB_M_DOMAIN_SEARCH = 1;
	NETLIB_M_NO_RECURSION = 2;
	NETLIB_K_DNS_TYPE_A = 1;
	NETLIB_K_DNS_TYPE_NS = 2;
	NETLIB_K_DNS_TYPE_MD = 3;
	NETLIB_K_DNS_TYPE_MF = 4;
	NETLIB_K_DNS_TYPE_CNAME = 5;
	NETLIB_K_DNS_TYPE_SOA = 6;
	NETLIB_K_DNS_TYPE_MB = 7;
	NETLIB_K_DNS_TYPE_MG = 8;
	NETLIB_K_DNS_TYPE_MR = 9;
	NETLIB_K_DNS_TYPE_NULL = 10;
	NETLIB_K_DNS_TYPE_WKS = 11;
	NETLIB_K_DNS_TYPE_PTR = 12;
	NETLIB_K_DNS_TYPE_HINFO = 13;
	NETLIB_K_DNS_TYPE_MINFO = 14;
	NETLIB_K_DNS_TYPE_MX = 15;
	NETLIB_K_DNS_TYPE_TXT = 16;
	NETLIB_K_DNS_TYPE_RP = 17;
	NETLIB_K_DNS_TYPE_AFSDB = 18;
	NETLIB_K_DNS_TYPE_X25 = 19;
	NETLIB_K_DNS_TYPE_ISDN = 20;
	NETLIB_K_DNS_TYPE_RT = 21;
	NETLIB_K_DNS_TYPE_NSAP = 22;
	NETLIB_K_DNS_TYPE_NSAP_PTR = 23;
	NETLIB_K_DNS_TYPE_SIG = 24;
	NETLIB_K_DNS_TYPE_KEY = 25;
	NETLIB_K_DNS_TYPE_PX = 26;
	NETLIB_K_DNS_TYPE_GPOS = 27;
	NETLIB_K_DNS_TYPE_AAAA = 28;
	NETLIB_K_DNS_TYPE_LOC = 29;
	NETLIB_K_DNS_TYPE_UINFO = 100;
	NETLIB_K_DNS_TYPE_UID = 101;
	NETLIB_K_DNS_TYPE_GID = 102;
	NETLIB_K_DNS_TYPE_UNSPEC = 103;
	NETLIB_K_DNS_TYPE_AXFR = 252;
	NETLIB_K_DNS_TYPE_MAILB = 253;
	NETLIB_K_DNS_TYPE_MAILA = 254;
	NETLIB_K_DNS_QTYPE_ALL = 255;
	NETLIB_K_DNS_CLASS_IN = 1;
	NETLIB_K_DNS_CLASS_CS = 2;
	NETLIB_K_DNS_CLASS_CH = 3;
	NETLIB_K_DNS_CLASS_HS = 4;
	NETLIB_K_DNS_QCLASS_ALL = 255;
	NETLIB_K_DNS_OP_STDQ = 0;
	NETLIB_K_DNS_OP_INVQ = 1;
	NETLIB_K_DNS_OP_STATUS = 2;
	NETLIB_K_DNS_RC_SUCCESS = 0;
	NETLIB_K_DNS_RC_FMTERR = 1;
	NETLIB_K_DNS_RC_SRVFAIL = 2;
	NETLIB_K_DNS_RC_NAMERR = 3;
	NETLIB_K_DNS_RC_NOTIMP = 4;
	NETLIB_K_DNS_RC_REFUSE = 5;
	dns_m_recursion_desired = 1;
	dns_m_truncated = 1;
	dns_m_authoritative = 1;
	dns_m_opcode = 15;
	dns_m_reply = 1;
	dns_m_reply_code = 15;
	dns_m_xx_unused_xx = 7;
	dns_m_recursion_available = 1;
 
TYPE	NETLIB_DNS_HEADER$TYPE = RECORD CASE INTEGER OF
	1: (dns_w_queryid : $UWORD;
	    dns_r_flags_overlay : [BYTE(2)] RECORD END;
	    dns_w_qdcount : $UWORD;
	    dns_w_ancount : $UWORD;
	    dns_w_nscount : $UWORD;
	    dns_w_arcount : $UWORD;
	    );
	2: (dns_w_flags : [POS(16)] $UWORD;
	    );
	3: (dns_x_flags : [POS(16), BYTE(1)] RECORD END;
	    );
	4: (dns_v_recursion_desired : [POS(16)] $BOOL;
	    );
	5: (dns_v_truncated : [POS(16)] $BOOL;
	    );
	6: (dns_v_authoritative : [POS(16)] $BOOL;
	    );
	7: (dns_v_opcode : [POS(16)] $BIT4;
	    );
	8: (dns_v_reply : [POS(16)] $BOOL;
	    );
	9: (dns_v_reply_code : [POS(16)] $BIT4;
	    );
	10: (dns_v_xx_unused_xx : [POS(16)] $BIT3;
	    );
	11: (dns_v_recursion_available : [POS(16)] $BOOL;
	    )
	END;
 
CONST	SOCKADDR_S_DATA = 14;
 
TYPE	SOCKADDR$TYPE = RECORD
	    sockaddr_w_family : $UWORD;
	    sockaddr_x_data : ARRAY [1..14] OF $UBYTE;
	END;
	INADDR$TYPE = RECORD
	    inaddr_l_addr : UNSIGNED;
	END;
 
CONST	SIN_S_MBZ = 8;
 
TYPE	SIN$TYPE = RECORD
	    sin_w_family : $UWORD;
	    sin_w_port : $UWORD;
	    sin_x_addr : INADDR$TYPE;
	    sin_x_mbz : ARRAY [1..8] OF $UBYTE;
	END;
	NETLIBIOSB$TYPE = RECORD
	    iosb_w_status : $UWORD;
	    iosb_w_count : $UWORD;
	    iosb_l_unused : UNSIGNED;
	END;
 
CONST	NETLIB_S_MXRR_NAME = 128;
 
TYPE	MXRR$TYPE = RECORD
	    mxrr_l_preference : UNSIGNED;
	    mxrr_l_length : UNSIGNED;
	    mxrr_t_name : PACKED ARRAY [1..128] OF CHAR;
	END;
 
CONST	NETLIB_K_METHOD_ANY = 0;
	NETLIB_K_METHOD_SSL2 = 1;
	NETLIB_K_METHOD_SSL3 = 2;
	NETLIB_K_METHOD_TLS1 = 3;
	NETLIB_K_FILETYPE_PEM = 1;
	NETLIB_K_FILETYPE_ASN1 = 2;
 
[ASYNCHRONOUS] FUNCTION netlib_socket : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_server_setup : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_bind : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_getsockname : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_getpeername : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_connect : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_write : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_writeline : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_read : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_readline : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_shutdown : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_close : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_listen : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_accept : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_get_hostname : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_setsockopt : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_getsockopt : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_name_to_address : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_address_to_name : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_dns_skipname : INTEGER; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_dns_expandname : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_dns_query : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_strtoaddr : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_addrtostr : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_connect_by_name : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_dns_mx_lookup : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_hton_long : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_ntoh_long : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_hton_word : $UWORD; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_ntoh_word : $UWORD; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_version : UNSIGNED; EXTERNAL;
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_context : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_SOCKET                                                        *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_socket (
	VAR context : [VOLATILE] UNSIGNED;
	socket : UNSIGNED;
	ssl_ctx : UNSIGNED) : INTEGER; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_ACCEPT                                                        *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_accept (
	VAR context : [VOLATILE] UNSIGNED;
	timeout : $UQUAD := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBDEF$$TYP1 := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : INTEGER := %IMMED 0) : INTEGER; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_CONNECT                                                       *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_connect (
	VAR context : [VOLATILE] UNSIGNED;
	timeout : $UQUAD := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBDEF$$TYP2 := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : INTEGER := %IMMED 0) : INTEGER; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_SHUTDOWN                                                      *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_shutdown (
	VAR context : [VOLATILE] UNSIGNED;
	VAR iosb : [VOLATILE] NETLIBDEF$$TYP3 := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : INTEGER := %IMMED 0) : INTEGER; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_READ                                                          *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_read (
	VAR context : [VOLATILE] UNSIGNED;
	buffer : [CLASS_S] PACKED ARRAY [$l2..$u2:INTEGER] OF CHAR;
	timeout : $UQUAD := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBDEF$$TYP4 := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : INTEGER := %IMMED 0) : INTEGER; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_WRITE                                                         *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_write (
	VAR context : [VOLATILE] UNSIGNED;
	buffer : [CLASS_S] PACKED ARRAY [$l2..$u2:INTEGER] OF CHAR;
	timeout : $UQUAD := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBDEF$$TYP5 := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : INTEGER := %IMMED 0) : INTEGER; EXTERNAL;
 
END.
