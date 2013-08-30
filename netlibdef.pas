(********************************************************************************************************************************)
(* Created: 30-Aug-2013 11:41:29 by OpenVMS SDL EV2-3      *)
(* Source:  30-AUG-2013 11:40:47 MG_SRC:[NETLIB]NETLIBDEF.SDL;73 *)
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
 
CONST	NETLIB_K_TYPE_STREAM = 1;
	NETLIB_K_TYPE_DGRAM = 2;
 
(*                                                                          *)
(* IP Socket options                                                        *)
(*                                                                          *)
 
CONST	NETLIB_K_OPTION_REUSEADDR = 4;
	NETLIB_K_OPTION_KEEPALIVE = 8;
	NETLIB_K_OPTION_BROADCAST = 32;
	NETLIB_K_OPTION_SNDBUF = 4097;
	NETLIB_K_OPTION_RCVBUF = 4098;
	NETLIB_K_OPTION_SNDLOWAT = 4099;
	NETLIB_K_OPTION_RCVLOWAT = 4100;
 
(*                                                                          *)
(* SSL Socket options                                                       *)
(*                                                                          *)
 
CONST	NETLIB_K_OPTION_SOCKET = 16385;
	NETLIB_K_OPTION_SSL = 16386;
	NETLIB_K_OPTION_CIPHER = 16388;
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
 
TYPE	NETLIB_DNS_HEADER = RECORD CASE INTEGER OF
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
 
(*                                                                          *)
(* Socket Routines...                                                       *)
(*                                                                          *)
(*                                                                          *)
(* NETLIB_SOCKET                                                            *)
(*                                                                          *)
(*	Create socket                                                       *)
(*                                                                          *)
(*	socket	= new socket                                                *)
(*	socktyp = socket type                                               *)
(*  family	= socket family                                             *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_socket (
	VAR socket : [VOLATILE] UNSIGNED;
	socktyp : UNSIGNED := %IMMED 0;
	family : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SERVER_SETUP                                                      *)
(*                                                                          *)
(*	Socket setup for inetd server                                       *)
(*	                                                                    *)
(*	socket	= socket to bind                                            *)
(*	sa	= socket address (IP address, port, etc.)                   *)
(*	salen	= length of sa                                              *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_server_setup (
	VAR socket : [VOLATILE] UNSIGNED;
	sa : SIN$TYPE;
	salen : UNSIGNED) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_BIND                                                              *)
(*                                                                          *)
(*	Set address and/or port for socket.                                 *)
(*                                                                          *)
(*	socket	= socket to bind                                            *)
(*	sa	= socket address (IP address, port, etc.)                   *)
(*	salen	= length of sa                                              *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_bind (
	VAR socket : [VOLATILE] UNSIGNED;
	sa : SIN$TYPE;
	salen : UNSIGNED;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_GETSOCKNAME                                                       *)
(*                                                                          *)
(*	Return local information for socket                                 *)
(*                                                                          *)
(*	socket	= socket to query                                           *)
(*	sa	= SINDEF structure                                          *)
(*	sasize	= size of sa                                                *)
(*  salen	= returned length of sa                                     *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_getsockname (
	socket : UNSIGNED;
	VAR sa : [VOLATILE] SIN$TYPE;
	sasize : UNSIGNED;
	VAR salen : [VOLATILE] UNSIGNED;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_GETPEERNAME                                                       *)
(*                                                                          *)
(*	Return remote information for socket                                *)
(*                                                                          *)
(*	socket	= socket to query                                           *)
(*	sa	= SINDEF structure                                          *)
(*	sasize	= size of sa                                                *)
(*  salen	= returned length of sa                                     *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_getpeername (
	socket : UNSIGNED;
	VAR sa : [VOLATILE] SIN$TYPE;
	sasize : UNSIGNED;
	VAR salen : [VOLATILE] UNSIGNED;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_CONNECT                                                           *)
(*                                                                          *)
(*	Establish a connection to a remote system.                          *)
(*                                                                          *)
(*	socket	= socket to connect                                         *)
(*	sa	= socket address describing where to connect                *)
(*	salen	= length of sa                                              *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_connect (
	socket : UNSIGNED;
	sa : SIN$TYPE;
	salen : UNSIGNED;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SHUTDOWN                                                          *)
(*                                                                          *)
(*	Shutdown connection (don't delete socket)                           *)
(*                                                                          *)
(*	socket	= socket to shutdown                                        *)
(*  shuttype= type of shutdown                                              *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_shutdown (
	VAR socket : [VOLATILE] UNSIGNED;
	shuttype : UNSIGNED;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_CLOSE                                                             *)
(*                                                                          *)
(*	Close a socket                                                      *)
(*                                                                          *)
(*	socket	= socket to close                                           *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_close (
	socket : UNSIGNED) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_ACCEPT                                                            *)
(*                                                                          *)
(*	Wait for incoming connections                                       *)
(*                                                                          *)
(*	socket	= socket to connect                                         *)
(*	newsocket = new incoming socket                                     *)
(*	ra	= socket address describing remote end                      *)
(*	rasize	= length of ra                                              *)
(*	ralen	= returned length of ra                                     *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_accept (
	socket : UNSIGNED;
	VAR newsocket : [VOLATILE] UNSIGNED;
	VAR ra : [VOLATILE] SIN$TYPE;
	rasize : UNSIGNED;
	VAR ralen : [VOLATILE] UNSIGNED;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_GETSOCKOPT                                                        *)
(*                                                                          *)
(*	Get socket option                                                   *)
(*                                                                          *)
(*	socket	= socket to query                                           *)
(*  level	= level of option                                           *)
(*  option	= option                                                    *)
(*  value	= address of result storage                                 *)
(*  valsize	= size of value                                             *)
(*  vallen	= returned length of value                                  *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_getsockopt (
	socket : UNSIGNED;
	level : UNSIGNED;
	option : UNSIGNED;
	%IMMED value : UNSIGNED;
	valsize : UNSIGNED;
	VAR vallen : [VOLATILE] UNSIGNED;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_ADDRESS_TO_NAME                                                   *)
(*                                                                          *)
(*	Get the hostname of an IP address                                   *)
(*                                                                          *)
(*  socket	= socket to get info about                                  *)
(*  which	= optional, type of DNS lookup                              *)
(*  address	= INADDRDEF to be looked up                                 *)
(*  addrsize= length of address                                             *)
(*  hostname= descriptor to receive hostname                                *)
(*  retlen	= hostname length                                           *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_address_to_name (
	socket : UNSIGNED;
	which : UNSIGNED := %IMMED 0;
	ADDRESS : INADDR$TYPE;
	addrsize : UNSIGNED;
	VAR hostname : [CLASS_S,VOLATILE] PACKED ARRAY [$l5..$u5:INTEGER] OF CHAR;
	VAR retlen : [VOLATILE] $UWORD;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_DNS_MX_LOOKUP                                                     *)
(*                                                                          *)
(*	Look up MX records for a domain name                                *)
(*                                                                          *)
(*	socket	= a socket                                                  *)
(*	hostname= hostname to lookup                                        *)
(*  mxrrlist= array of MXRRDEF structures                                   *)
(*  mxrrsize= number elements in mxrrlist                                   *)
(*  mxrrcnt = number of elements actually written                           *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_dns_mx_lookup (
	socket : UNSIGNED;
	name : [CLASS_S] PACKED ARRAY [$l2..$u2:INTEGER] OF CHAR;
	%REF mxrrlist : [VOLATILE] ARRAY [$l3..$u3:INTEGER] OF MXRR$TYPE;
	mxrrsize : UNSIGNED;
	VAR mxrrcnt : [VOLATILE] UNSIGNED := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_DNS_SKIPNAME                                                      *)
(*                                                                          *)
(*	Skip a name in a DNS response                                       *)
(*                                                                          *)
(*	bufptr	= pointer to DNS response area                              *)
(*	buflen	= count of bytes in buffer from bufptr                      *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_dns_skipname (
	%IMMED bufptr : $DEFPTR;
	buflen : $UWORD) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_DNS_EXPANDNAME                                                    *)
(*                                                                          *)
(*	Expand name is DNS response                                         *)
(*                                                                          *)
(*	buffer	= start of DNS response buffer                              *)
(*	buflen	= buffer size                                               *)
(*	bufptr	= area containing domain name                               *)
(*	name	= descriptor to receive expanded name                       *)
(*	retlen	= optional, length of name                                  *)
(*  skipcount=number of bytes in buffer used                                *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_dns_expandname (
	%IMMED bufstart : $DEFPTR;
	buflen : $UWORD;
	%IMMED bufptr : $DEFPTR;
	VAR name : [CLASS_S,VOLATILE] PACKED ARRAY [$l4..$u4:INTEGER] OF CHAR;
	VAR retlen : [VOLATILE] $UWORD := %IMMED 0;
	VAR skipcount : [VOLATILE] $UWORD := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_DNS_QUERY                                                         *)
(*                                                                          *)
(*	Perform a DNS query                                                 *)
(*                                                                          *)
(*	socket	= a socket                                                  *)
(*	name	= domain name to look up                                    *)
(*	class	= class of query                                            *)
(*	qtype	= type of query                                             *)
(*	buffer	= buffer to receive dns response                            *)
(*	bufsize	= size of buffer in bytes                                   *)
(*	flags	= query options                                             *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_dns_query (
	socket : UNSIGNED;
	name : [CLASS_S] PACKED ARRAY [$l2..$u2:INTEGER] OF CHAR;
	class : UNSIGNED := %IMMED 0;
	qtype : UNSIGNED;
	%IMMED buffer : $DEFPTR;
	bufsize : $UWORD;
	flags : UNSIGNED := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_STRTOADDR                                                         *)
(*                                                                          *)
(*	Convert a dotted-address to binary form                             *)
(*                                                                          *)
(*	string	= input IP address string                                   *)
(*  address	= output binary address                                     *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_strtoaddr (
	string : [CLASS_S] PACKED ARRAY [$l1..$u1:INTEGER] OF CHAR;
	VAR address : [VOLATILE] INADDR$TYPE) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_NAME_TO_ADDRESS                                                   *)
(*                                                                          *)
(*	Get IP address(es) for a host name                                  *)
(*                                                                          *)
(*  socket	= socket to get info about                                  *)
(*  which   = type of lookup                                                *)
(*  hostname= host name to look up                                          *)
(*  addrlist= array of INADDRDEF structures                                 *)
(*  addrsize= number elements in addrlist                                   *)
(*  addrcnt = number of elements actually written                           *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_name_to_address (
	socket : UNSIGNED;
	which : UNSIGNED;
	hostname : [CLASS_S] PACKED ARRAY [$l3..$u3:INTEGER] OF CHAR;
	%REF addrlist : [VOLATILE] ARRAY [$l4..$u4:INTEGER] OF INADDR$TYPE;
	addrsize : UNSIGNED;
	VAR addrcnt : [VOLATILE] UNSIGNED := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_GET_HOSTNAME                                                      *)
(*                                                                          *)
(*	Return internet hostname of local host                              *)
(*	                                                                    *)
(*	namdsc	= string to receive hostname                                *)
(*	retlen	= optional, return length of hostname                       *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_get_hostname (
	VAR namdsc : [CLASS_S,VOLATILE] PACKED ARRAY [$l1..$u1:INTEGER] OF CHAR := %IMMED 0;
	VAR retlen : [VOLATILE] $UWORD := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SETSOCKOPT                                                        *)
(*                                                                          *)
(*	Set socket option                                                   *)
(*                                                                          *)
(*	socket	= socket to query                                           *)
(*  level	= level of option                                           *)
(*  option	= option                                                    *)
(*  value	= address of result storage                                 *)
(*  vallen	= size of value                                             *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_setsockopt (
	socket : UNSIGNED;
	level : UNSIGNED;
	option : UNSIGNED;
	%IMMED value : UNSIGNED;
	vallen : UNSIGNED;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_LISTEN                                                            *)
(*                                                                          *)
(*	Configure socket to receive connections                             *)
(*                                                                          *)
(*	socket	= socket to query                                           *)
(*  level	= backlog connections                                       *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_listen (
	socket : UNSIGNED;
	backlog : UNSIGNED := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_WRITE                                                             *)
(*                                                                          *)
(*	Write data to socket                                                *)
(*                                                                          *)
(*	socket	= socket to read from                                       *)
(*  buffer	= receive buffer                                            *)
(*	sa	= optional, SINDEF structure                                *)
(*	salen	= optional, size of sa                                      *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_write (
	socket : UNSIGNED;
	buffer : [CLASS_S] PACKED ARRAY [$l2..$u2:INTEGER] OF CHAR;
	sa : SIN$TYPE := %IMMED 0;
	salen : UNSIGNED := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_WRITELINE                                                         *)
(*                                                                          *)
(*	Write data to socket adding terminating CR/LF pair.                 *)
(*                                                                          *)
(*	socket	= socket to read from                                       *)
(*  buffer	= receive buffer                                            *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_writeline (
	socket : UNSIGNED;
	buffer : [CLASS_S] PACKED ARRAY [$l2..$u2:INTEGER] OF CHAR;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_READ                                                              *)
(*                                                                          *)
(*	Read data from socket                                               *)
(*                                                                          *)
(*	socket	= socket to read from                                       *)
(*  buffer	= receive buffer                                            *)
(*	sa	= optional, SINDEF structure                                *)
(*	sasize	= optional, size of sa                                      *)
(*  salen	= optional, returned length of sa                           *)
(*  timeout	= optional, read timeout                                    *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_read (
	socket : UNSIGNED;
	VAR buffer : [CLASS_S,VOLATILE] PACKED ARRAY [$l2..$u2:INTEGER] OF CHAR;
	VAR sa : [VOLATILE] SIN$TYPE := %IMMED 0;
	sasize : UNSIGNED := %IMMED 0;
	VAR salen : [VOLATILE] UNSIGNED := %IMMED 0;
	timeout : $UQUAD := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_READLINE                                                          *)
(*                                                                          *)
(*	Read line from socket                                               *)
(*                                                                          *)
(*	socket	= socket to read from                                       *)
(*  buffer	= buffer to receive line                                    *)
(*  retlen	= optional, return length of buffer                         *)
(*  flags	= optional, control flags                                   *)
(*  timeout	= optional, read timeout                                    *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_readline (
	socket : UNSIGNED;
	VAR buffer : [CLASS_S,VOLATILE] PACKED ARRAY [$l2..$u2:INTEGER] OF CHAR;
	VAR retlen : [VOLATILE] $UWORD := %IMMED 0;
	flags : UNSIGNED := %IMMED 0;
	timeout : $UQUAD := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*	                                                                    *)
(*	NETLIB_ADDRTOSTR                                                    *)
(*	                                                                    *)
(*	Convert binary IP to string                                         *)
(*	                                                                    *)
(*	address	= INADDRDEF structure                                       *)
(*	string	= string to receive address                                 *)
(*	retlen	= optional, return length of string                         *)
(*	                                                                    *)
 
[ASYNCHRONOUS] FUNCTION netlib_addrtostr (
	ADDRESS : INADDR$TYPE;
	VAR string : [CLASS_S,VOLATILE] PACKED ARRAY [$l2..$u2:INTEGER] OF CHAR;
	VAR retlen : [VOLATILE] $UWORD := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_CONNECT_BY_NAME                                                   *)
(*                                                                          *)
(*	Connect to remote host by name.                                     *)
(*                                                                          *)
(*	socket	= stream socket allocated by NETLIB_SOCKET                  *)
(*	hostname= string containing the hostname                            *)
(*	port	= port number in host order                                 *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_connect_by_name (
	socket : UNSIGNED;
	hostname : [CLASS_S] PACKED ARRAY [$l2..$u2:INTEGER] OF CHAR;
	port : $UWORD;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_HTON_LONG                                                         *)
(*                                                                          *)
(*	Convert host-order longword to network-order                        *)
(*                                                                          *)
(*	value	= longword to convert                                       *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_hton_long (
	value : UNSIGNED) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_NTOH_LONG                                                         *)
(*                                                                          *)
(*	Convert network-order longword to host-order                        *)
(*                                                                          *)
(*	value	= longword to convert                                       *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ntoh_long (
	value : UNSIGNED) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_HTON_WORD                                                         *)
(*                                                                          *)
(*	Convert host-order word to network-order                            *)
(*                                                                          *)
(*	value	= word to convert                                           *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_hton_word (
	value : $UWORD) : $UWORD; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_NTOH_WORD                                                         *)
(*                                                                          *)
(*	Convert network-order word to host-order                            *)
(*                                                                          *)
(*	value	= word to convert                                           *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ntoh_word (
	value : $UWORD) : $UWORD; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_VERSION                                                           *)
(*                                                                          *)
(*	Return NETLIB version                                               *)
(*	                                                                    *)
(*	strver	= string to receive version string                          *)
(*	retlen	= optional, return length of string                         *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_version (
	VAR strver : [CLASS_S,VOLATILE] PACKED ARRAY [$l1..$u1:INTEGER] OF CHAR := %IMMED 0;
	VAR retlen : [VOLATILE] $UWORD := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* SSL Routines                                                             *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_context : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_SOCKET                                                        *)
(*                                                                          *)
(*	Allocate an SSL socket                                              *)
(*                                                                          *)
(*  context = SSL socket                                                    *)
(*	socket	= NETLIB socket                                             *)
(*	ssl_ctx	= SSL_CTX structure                                         *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_socket (
	VAR context : [VOLATILE] UNSIGNED;
	socket : UNSIGNED;
	ssl_ctx : UNSIGNED) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_ACCEPT                                                        *)
(*                                                                          *)
(*	Accept incoming SSL connection                                      *)
(*                                                                          *)
(*  context = SSL socket                                                    *)
(*  timeout	= optional, read timeout                                    *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_accept (
	VAR context : [VOLATILE] UNSIGNED;
	timeout : $UQUAD := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_CONNECT                                                       *)
(*                                                                          *)
(*	Establish an SSL connection to a remote system.                     *)
(*                                                                          *)
(*  context = SSL socket                                                    *)
(*  timeout	= optional, read timeout                                    *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_connect (
	VAR context : [VOLATILE] UNSIGNED;
	timeout : $UQUAD := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_SHUTDOWN                                                      *)
(*                                                                          *)
(*	Shutdown SSL socket (don't delete socket)                           *)
(*                                                                          *)
(*  context = SSL socket                                                    *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_shutdown (
	VAR context : [VOLATILE] UNSIGNED;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_CLOSE                                                         *)
(*                                                                          *)
(*	Close an SSL socket                                                 *)
(*                                                                          *)
(*	socket	= socket to close                                           *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_close (
	socket : UNSIGNED) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_READ                                                          *)
(*                                                                          *)
(*	Read data from SSL socket                                           *)
(*                                                                          *)
(*  context = SSL socket                                                    *)
(*  buffer	= receive buffer                                            *)
(*  timeout	= optional, read timeout                                    *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_read (
	VAR context : [VOLATILE] UNSIGNED;
	buffer : [CLASS_S] PACKED ARRAY [$l2..$u2:INTEGER] OF CHAR;
	timeout : $UQUAD := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_WRITE                                                         *)
(*                                                                          *)
(*	Write data to SSL socket                                            *)
(*                                                                          *)
(*  context = SSL socket                                                    *)
(*  buffer	= receive buffer                                            *)
(*  timeout	= optional, read timeout                                    *)
(*	iosb	= optional, I/O status block                                *)
(*	astadr	= optional, I/O completion AST                              *)
(*	astprm	= optional, AST parameter                                   *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_write (
	VAR context : [VOLATILE] UNSIGNED;
	buffer : [CLASS_S] PACKED ARRAY [$l2..$u2:INTEGER] OF CHAR;
	timeout : $UQUAD := %IMMED 0;
	VAR iosb : [VOLATILE] NETLIBIOSB$TYPE := %IMMED 0;
	%IMMED [UNBOUND, ASYNCHRONOUS] PROCEDURE astadr := %IMMED 0;
	%IMMED astprm : UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_VERSION                                                       *)
(*                                                                          *)
(*	Return OpenSSL library version                                      *)
(*	                                                                    *)
(*	strver	= optional, string to receive version string                *)
(*	retlen	= optional, return length of string                         *)
(*	numver  = optional, longword to receive version as number           *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_ssl_version (
	VAR strver : [CLASS_S,VOLATILE] PACKED ARRAY [$l1..$u1:INTEGER] OF CHAR := %IMMED 0;
	VAR retlen : [VOLATILE] $UWORD := %IMMED 0;
	VAR numver : [VOLATILE] UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
(*                                                                          *)
(* NETLIB_SSL_GETSOCKOPT                                                    *)
(*                                                                          *)
(*	Get SSL socket option                                               *)
(*                                                                          *)
(*	socket	= socket to query                                           *)
(*  option	= option                                                    *)
(*  value	= address of result storage                                 *)
(*  valsize	= size of value                                             *)
(*  vallen	= returned length of value                                  *)
(*                                                                          *)
 
[ASYNCHRONOUS] FUNCTION netlib_getsockopt (
	socket : UNSIGNED;
	option : UNSIGNED;
	%IMMED value : UNSIGNED;
	valsize : UNSIGNED;
	VAR vallen : [VOLATILE] UNSIGNED := %IMMED 0) : UNSIGNED; EXTERNAL;
 
END.
