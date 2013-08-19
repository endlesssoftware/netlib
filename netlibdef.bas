 !********************************************************************************************************************************
 ! Created: 19-Aug-2013 12:49:00 by OpenVMS SDL EV2-3      
 ! Source:  19-AUG-2013 12:48:51 MG_SRC:[NETLIB]NETLIBDEF.SDL;69 
 !********************************************************************************************************************************
      ! *** PREDECLARED TYPES
     
      %IF %DECLARED(%BASIC$QUADWORD_DECLARED) = 0 %THEN
         RECORD BASIC$QUADWORD
            LONG FILL(2)
         END RECORD
         %LET %BASIC$QUADWORD_DECLARED = 1
      %END %IF
     
      %IF %DECLARED(%BASIC$OCTAWORD_DECLARED) = 0 %THEN
         RECORD BASIC$OCTAWORD
            LONG FILL(4)
         END RECORD
         %LET %BASIC$OCTAWORD_DECLARED = 1
      %END %IF
     
      %IF %DECLARED(%BASIC$F_FLOATING_COMPLEX_DECL) = 0 %THEN
         RECORD BASIC$F_FLOATING_COMPLEX
            SINGLE REAL_PART
            SINGLE IMAGINARY_PART
         END RECORD
         %LET %BASIC$F_FLOATING_COMPLEX_DECL = 1
      %END %IF
     
      %IF %DECLARED(%BASIC$D_FLOATING_COMPLEX_DECL) = 0 %THEN
         RECORD BASIC$D_FLOATING_COMPLEX
            DOUBLE REAL_PART
            DOUBLE IMAGINARY_PART
         END RECORD
         %LET %BASIC$D_FLOATING_COMPLEX_DECL = 1
      %END %IF
     
      %IF %DECLARED(%BASIC$G_FLOATING_COMPLEX_DECL) = 0 %THEN
         RECORD BASIC$G_FLOATING_COMPLEX
            GFLOAT REAL_PART
            GFLOAT IMAGINARY_PART
         END RECORD
         %LET %BASIC$G_FLOATING_COMPLEX_DECL = 1
      %END %IF
     
      %IF %DECLARED(%BASIC$H_FLOATING_COMPLEX_DECL) = 0 %THEN
         RECORD BASIC$H_FLOATING_COMPLEX
            HFLOAT REAL_PART
            HFLOAT IMAGINARY_PART
         END RECORD
         %LET %BASIC$H_FLOATING_COMPLEX_DECL = 1
      %END %IF
     
    
    !*** MODULE NETLIBDEF ***
    
    
      DECLARE LONG CONSTANT NETLIB_K_TYPE_STREAM = 1
      DECLARE LONG CONSTANT NETLIB_K_TYPE_DGRAM = 2
      DECLARE LONG CONSTANT NETLIB_K_OPTION_REUSEADDR = 4
      DECLARE LONG CONSTANT NETLIB_K_OPTION_KEEPALIVE = 8
      DECLARE LONG CONSTANT NETLIB_K_OPTION_BROADCAST = 32
      DECLARE LONG CONSTANT NETLIB_K_OPTION_SNDBUF = 4097
      DECLARE LONG CONSTANT NETLIB_K_OPTION_RCVBUF = 4098
      DECLARE LONG CONSTANT NETLIB_K_OPTION_SNDLOWAT = 4099
      DECLARE LONG CONSTANT NETLIB_K_OPTION_RCVLOWAT = 4100
      DECLARE LONG CONSTANT NETLIB_K_LEVEL_SOCKET = 65535
      DECLARE LONG CONSTANT NETLIB_K_AF_INET = 2
      DECLARE LONG CONSTANT NETLIB_K_LOOKUP_DNS = 1
      DECLARE LONG CONSTANT NETLIB_K_LOOKUP_HOST_TABLE = 2
      DECLARE LONG CONSTANT NETLIB_K_SHUTDOWN_RECEIVER = 0
      DECLARE LONG CONSTANT NETLIB_K_SHUTDOWN_SENDER = 1
      DECLARE LONG CONSTANT NETLIB_K_SHUTDOWN_BOTH = 2
      DECLARE LONG CONSTANT NETLIB_M_ALLOW_LF = 1
      DECLARE LONG CONSTANT NETLIB_M_FLUSH = 2
      DECLARE LONG CONSTANT NETLIB_M_ALLOW_CR = 4
      DECLARE LONG CONSTANT NETLIB_M_DOMAIN_SEARCH = 1
      DECLARE LONG CONSTANT NETLIB_M_NO_RECURSION = 2
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_A = 1
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_NS = 2
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_MD = 3
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_MF = 4
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_CNAME = 5
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_SOA = 6
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_MB = 7
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_MG = 8
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_MR = 9
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_NULL = 10
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_WKS = 11
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_PTR = 12
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_HINFO = 13
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_MINFO = 14
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_MX = 15
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_TXT = 16
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_RP = 17
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_AFSDB = 18
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_X25 = 19
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_ISDN = 20
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_RT = 21
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_NSAP = 22
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_NSAP_PTR = 23
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_SIG = 24
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_KEY = 25
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_PX = 26
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_GPOS = 27
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_AAAA = 28
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_LOC = 29
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_UINFO = 100
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_UID = 101
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_GID = 102
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_UNSPEC = 103
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_AXFR = 252
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_MAILB = 253
      DECLARE LONG CONSTANT NETLIB_K_DNS_TYPE_MAILA = 254
      DECLARE LONG CONSTANT NETLIB_K_DNS_QTYPE_ALL = 255
      DECLARE LONG CONSTANT NETLIB_K_DNS_CLASS_IN = 1
      DECLARE LONG CONSTANT NETLIB_K_DNS_CLASS_CS = 2
      DECLARE LONG CONSTANT NETLIB_K_DNS_CLASS_CH = 3
      DECLARE LONG CONSTANT NETLIB_K_DNS_CLASS_HS = 4
      DECLARE LONG CONSTANT NETLIB_K_DNS_QCLASS_ALL = 255
      DECLARE LONG CONSTANT NETLIB_K_DNS_OP_STDQ = 0
      DECLARE LONG CONSTANT NETLIB_K_DNS_OP_INVQ = 1
      DECLARE LONG CONSTANT NETLIB_K_DNS_OP_STATUS = 2
      DECLARE LONG CONSTANT NETLIB_K_DNS_RC_SUCCESS = 0
      DECLARE LONG CONSTANT NETLIB_K_DNS_RC_FMTERR = 1
      DECLARE LONG CONSTANT NETLIB_K_DNS_RC_SRVFAIL = 2
      DECLARE LONG CONSTANT NETLIB_K_DNS_RC_NAMERR = 3
      DECLARE LONG CONSTANT NETLIB_K_DNS_RC_NOTIMP = 4
      DECLARE LONG CONSTANT NETLIB_K_DNS_RC_REFUSE = 5
      DECLARE LONG CONSTANT dns_m_recursion_desired = x'00000001'
      DECLARE LONG CONSTANT dns_m_truncated = x'00000001'
      DECLARE LONG CONSTANT dns_m_authoritative = x'00000001'
      DECLARE LONG CONSTANT dns_m_opcode = x'0000000F'
      DECLARE LONG CONSTANT dns_m_reply = x'00000001'
      DECLARE LONG CONSTANT dns_m_reply_code = x'0000000F'
      DECLARE LONG CONSTANT dns_m_xx_unused_xx = x'00000007'
      DECLARE LONG CONSTANT dns_m_recursion_available = x'00000001'
      DECLARE LONG CONSTANT dns_S_NETLIB_DNS_HEADER = 12
      record NETLIB_DNS_HEADER
         WORD dns_w_queryid
         group dns_r_flags_overlay
            variant
               case
                  WORD dns_w_flags
               case
                  group dns_x_flags
                     variant
                     case
                        WORD recursion_desired_bits   ! COMMENT ADDED BY SDL - recursion_desired_bits contains bits  &
 ! recursion_desired through recursion_available
                     end variant
                  end group dns_x_flags
            end variant
         end group dns_r_flags_overlay
         WORD dns_w_qdcount
         WORD dns_w_ancount
         WORD dns_w_nscount
         WORD dns_w_arcount
      end record NETLIB_DNS_HEADER
    
      DECLARE LONG CONSTANT SOCKADDR_S_DATA = 14
      DECLARE LONG CONSTANT sockaddr_S_SOCKADDRDEF = 16
      record SOCKADDRDEF
         WORD sockaddr_w_family
         BYTE sockaddr_x_data(1 to 14)
      end record SOCKADDRDEF
    
      DECLARE LONG CONSTANT inaddr_S_INADDRDEF = 4
      record INADDRDEF
         LONG inaddr_l_addr
      end record INADDRDEF
    
      DECLARE LONG CONSTANT SIN_S_MBZ = 8
      DECLARE LONG CONSTANT sin_S_SINDEF = 16
      record SINDEF
         WORD sin_w_family
         WORD sin_w_port
         INADDRDEF sin_x_addr
         BYTE sin_x_mbz(1 to 8)
      end record SINDEF
    
      DECLARE LONG CONSTANT iosb_S_NETLIBIOSBDEF = 8
      record NETLIBIOSBDEF
         WORD iosb_w_status
         WORD iosb_w_count
         LONG iosb_l_unused
      end record NETLIBIOSBDEF
    
      DECLARE LONG CONSTANT NETLIB_S_MXRR_NAME = 128
      DECLARE LONG CONSTANT mxrr_S_MXRRDEF = 136
      record MXRRDEF
         LONG mxrr_l_preference
         LONG mxrr_l_length
         STRING mxrr_t_name = 128
      end record MXRRDEF
    
      DECLARE LONG CONSTANT NETLIB_K_METHOD_ANY = 0
      DECLARE LONG CONSTANT NETLIB_K_METHOD_SSL2 = 1
      DECLARE LONG CONSTANT NETLIB_K_METHOD_SSL3 = 2
      DECLARE LONG CONSTANT NETLIB_K_METHOD_TLS1 = 3
      DECLARE LONG CONSTANT NETLIB_K_FILETYPE_PEM = 1
      DECLARE LONG CONSTANT NETLIB_K_FILETYPE_ASN1 = 2
    ! 
    !  Socket Routines...
    ! 
    ! 
    !  NETLIB_SOCKET
    ! 
    ! 	Create socket
    ! 
    ! 	socket	= new socket
    ! 	socktyp = socket type
    !   family	= socket family
    ! 
      EXTERNAL LONG FUNCTION  netlib_socket &
               ( &
                   LONG  BY REF, &
                   LONG  BY REF, &
                   LONG  BY REF &
               )
    ! 
    !  NETLIB_SERVER_SETUP
    ! 
    ! 	Socket setup for inetd server
    ! 	
    ! 	socket	= socket to bind
    ! 	sa	= socket address (IP address, port, etc.)
    ! 	salen	= length of sa
    ! 
      EXTERNAL LONG FUNCTION  netlib_server_setup &
               ( &
                   LONG  BY REF, &
                   SINDEF  BY REF, &
                   LONG  BY REF &
               )
    ! 
    !  NETLIB_BIND
    ! 
    ! 	Set address and/or port for socket.
    ! 
    ! 	socket	= socket to bind
    ! 	sa	= socket address (IP address, port, etc.)
    ! 	salen	= length of sa
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_bind &
               ( &
                   LONG  BY REF, &
                   SINDEF  BY REF, &
                   LONG  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_GETSOCKNAME
    ! 
    ! 	Return local information for socket
    ! 
    ! 	socket	= socket to query
    ! 	sa	= SINDEF structure
    ! 	sasize	= size of sa
    !   salen	= returned length of sa
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_getsockname &
               ( &
                   LONG  BY REF, &
                   SINDEF  BY REF, &
                   LONG  BY REF, &
                   LONG  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_GETPEERNAME
    ! 
    ! 	Return remote information for socket
    ! 
    ! 	socket	= socket to query
    ! 	sa	= SINDEF structure
    ! 	sasize	= size of sa
    !   salen	= returned length of sa
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_getpeername &
               ( &
                   LONG  BY REF, &
                   SINDEF  BY REF, &
                   LONG  BY REF, &
                   LONG  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_CONNECT
    ! 
    ! 	Establish a connection to a remote system.
    ! 
    ! 	socket	= socket to connect
    ! 	sa	= socket address describing where to connect
    ! 	salen	= length of sa
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_connect &
               ( &
                   LONG  BY REF, &
                   SINDEF  BY REF, &
                   LONG  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_SHUTDOWN
    ! 
    ! 	Shutdown connection (don't delete socket)
    ! 
    ! 	socket	= socket to shutdown
    !   shuttype= type of shutdown
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_shutdown &
               ( &
                   LONG  BY REF, &
                   LONG  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_CLOSE
    ! 
    ! 	Close a socket
    ! 
    ! 	socket	= socket to close
    ! 
      EXTERNAL LONG FUNCTION  netlib_close &
               ( &
                   LONG  BY REF &
               )
    ! 
    !  NETLIB_ACCEPT
    ! 
    ! 	Wait for incoming connections
    ! 
    ! 	socket	= socket to connect
    ! 	newsocket = new incoming socket
    ! 	ra	= socket address describing remote end
    ! 	rasize	= length of ra
    ! 	ralen	= returned length of ra
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_accept &
               ( &
                   LONG  BY REF, &
                   LONG  BY REF, &
                   SINDEF  BY REF, &
                   LONG  BY REF, &
                   LONG  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_GETSOCKOPT
    ! 
    ! 	Get socket option
    ! 
    ! 	socket	= socket to query
    !   level	= level of option
    !   option	= option
    !   value	= address of result storage
    !   valsize	= size of value
    !   vallen	= returned length of value
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_getsockopt &
               ( &
                   LONG  BY REF, &
                   LONG  BY REF, &
                   LONG  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY REF, &
                   LONG  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_ADDRESS_TO_NAME
    ! 
    ! 	Get the hostname of an IP address
    ! 
    !   socket	= socket to get info about
    !   which	= optional, type of DNS lookup
    !   address	= INADDRDEF to be looked up
    !   addrsize= length of address
    !   hostname= descriptor to receive hostname
    !   retlen	= hostname length
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_address_to_name &
               ( &
                   LONG  BY REF, &
                   LONG  BY REF, &
                   INADDRDEF  BY REF, &
                   LONG  BY REF, &
                   STRING  BY DESC, &
                   WORD  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_DNS_MX_LOOKUP
    ! 
    ! 	Look up MX records for a domain name
    ! 
    ! 	socket	= a socket
    ! 	hostname= hostname to lookup
    !   mxrrlist= array of MXRRDEF structures
    !   mxrrsize= number elements in mxrrlist
    !   mxrrcnt = number of elements actually written
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_dns_mx_lookup &
               ( &
                   LONG  BY REF, &
                   STRING  BY DESC, &
                   MXRRDEF DIM() BY REF, &
                   LONG  BY REF, &
                  OPTIONAL LONG  BY REF, &
                   NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_DNS_SKIPNAME
    ! 
    ! 	Skip a name in a DNS response
    ! 
    ! 	bufptr	= pointer to DNS response area
    ! 	buflen	= count of bytes in buffer from bufptr
    ! 
      EXTERNAL LONG FUNCTION  netlib_dns_skipname &
               ( &
                   LONG  BY VALUE, &
                   WORD  BY REF &
               )
    ! 
    !  NETLIB_DNS_EXPANDNAME
    ! 
    ! 	Expand name is DNS response
    ! 
    ! 	buffer	= start of DNS response buffer
    ! 	buflen	= buffer size
    ! 	bufptr	= area containing domain name
    ! 	name	= descriptor to receive expanded name
    ! 	retlen	= optional, length of name
    !   skipcount=number of bytes in buffer used
    ! 
      EXTERNAL LONG FUNCTION  netlib_dns_expandname &
               ( &
                   LONG  BY VALUE, &
                   WORD  BY REF, &
                   LONG  BY VALUE, &
                   STRING  BY DESC, &
                  OPTIONAL WORD  BY REF, &
                   WORD  BY REF &
               )
    ! 
    !  NETLIB_DNS_QUERY
    ! 
    ! 	Perform a DNS query
    ! 
    ! 	socket	= a socket
    ! 	name	= domain name to look up
    ! 	class	= class of query
    ! 	qtype	= type of query
    ! 	buffer	= buffer to receive dns response
    ! 	bufsize	= size of buffer in bytes
    ! 	flags	= query options
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_dns_query &
               ( &
                   LONG  BY REF, &
                   STRING  BY DESC, &
                   LONG  BY REF, &
                   LONG  BY REF, &
                   LONG  BY VALUE, &
                   WORD  BY REF, &
                  OPTIONAL LONG  BY REF, &
                   NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_STRTOADDR
    ! 
    ! 	Convert a dotted-address to binary form
    ! 
    ! 	string	= input IP address string
    !   address	= output binary address
    ! 
      EXTERNAL LONG FUNCTION  netlib_strtoaddr &
               ( &
                   STRING  BY DESC, &
                   INADDRDEF  BY REF &
               )
    ! 
    !  NETLIB_NAME_TO_ADDRESS
    ! 
    ! 	Get IP address(es) for a host name
    ! 
    !   socket	= socket to get info about
    !   which   = type of lookup
    !   hostname= host name to look up
    !   addrlist= array of INADDRDEF structures
    !   addrsize= number elements in addrlist
    !   addrcnt = number of elements actually written
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_name_to_address &
               ( &
                   LONG  BY REF, &
                   LONG  BY REF, &
                   STRING  BY DESC, &
                   INADDRDEF DIM() BY REF, &
                   LONG  BY REF, &
                  OPTIONAL LONG  BY REF, &
                   NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_GET_HOSTNAME
    ! 
    ! 	Return internet hostname of local host
    ! 	
    ! 	namdsc	= string to receive hostname
    ! 	retlen	= optional, return length of hostname
    ! 
      EXTERNAL LONG FUNCTION  netlib_get_hostname &
               ( &
                  OPTIONAL STRING  BY DESC, &
                   WORD  BY REF &
               )
    ! 
    !  NETLIB_SETSOCKOPT
    ! 
    ! 	Set socket option
    ! 
    ! 	socket	= socket to query
    !   level	= level of option
    !   option	= option
    !   value	= address of result storage
    !   vallen	= size of value
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_setsockopt &
               ( &
                   LONG  BY REF, &
                   LONG  BY REF, &
                   LONG  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_LISTEN
    ! 
    ! 	Configure socket to receive connections
    ! 
    ! 	socket	= socket to query
    !   level	= backlog connections
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_listen &
               ( &
                   LONG  BY REF, &
                  OPTIONAL LONG  BY REF, &
                   NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_WRITE
    ! 
    ! 	Write data to socket
    ! 
    ! 	socket	= socket to read from
    !   buffer	= receive buffer
    ! 	sa	= optional, SINDEF structure
    ! 	salen	= optional, size of sa
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_write &
               ( &
                   LONG  BY REF, &
                   STRING  BY DESC, &
                  OPTIONAL SINDEF  BY REF, &
                   LONG  BY REF, &
                   NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_WRITELINE
    ! 
    ! 	Write data to socket adding terminating CR/LF pair.
    ! 
    ! 	socket	= socket to read from
    !   buffer	= receive buffer
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_writeline &
               ( &
                   LONG  BY REF, &
                   STRING  BY DESC, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_READ
    ! 
    ! 	Read data from socket
    ! 
    ! 	socket	= socket to read from
    !   buffer	= receive buffer
    ! 	sa	= optional, SINDEF structure
    ! 	sasize	= optional, size of sa
    !   salen	= optional, returned length of sa
    !   timeout	= optional, read timeout
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_read &
               ( &
                   LONG  BY REF, &
                   STRING  BY DESC, &
                  OPTIONAL SINDEF  BY REF, &
                   LONG  BY REF, &
                   LONG  BY REF, &
                   BASIC$QUADWORD  BY REF, &
                   NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_READLINE
    ! 
    ! 	Read line from socket
    ! 
    ! 	socket	= socket to read from
    !   buffer	= buffer to receive line
    !   retlen	= optional, return length of buffer
    !   flags	= optional, control flags
    !   timeout	= optional, read timeout
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_readline &
               ( &
                   LONG  BY REF, &
                   STRING  BY DESC, &
                  OPTIONAL WORD  BY REF, &
                   LONG  BY REF, &
                   BASIC$QUADWORD  BY REF, &
                   NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 	
    ! 	NETLIB_ADDRTOSTR
    ! 	
    ! 	Convert binary IP to string
    ! 	
    ! 	address	= INADDRDEF structure
    ! 	string	= string to receive address
    ! 	retlen	= optional, return length of string
    ! 	
      EXTERNAL LONG FUNCTION  netlib_addrtostr &
               ( &
                   INADDRDEF  BY REF, &
                   STRING  BY DESC, &
                  OPTIONAL WORD  BY REF &
               )
    ! 
    !  NETLIB_CONNECT_BY_NAME
    ! 
    ! 	Connect to remote host by name.
    ! 
    ! 	socket	= stream socket allocated by NETLIB_SOCKET
    ! 	hostname= string containing the hostname
    ! 	port	= port number in host order
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_connect_by_name &
               ( &
                   LONG  BY REF, &
                   STRING  BY DESC, &
                   WORD  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_HTON_LONG
    ! 
    ! 	Convert host-order longword to network-order
    ! 
    ! 	value	= longword to convert
    ! 
      EXTERNAL LONG FUNCTION  netlib_hton_long &
               ( &
                   LONG  BY REF &
               )
    ! 
    !  NETLIB_NTOH_LONG
    ! 
    ! 	Convert network-order longword to host-order
    ! 
    ! 	value	= longword to convert
    ! 
      EXTERNAL LONG FUNCTION  netlib_ntoh_long &
               ( &
                   LONG  BY REF &
               )
    ! 
    !  NETLIB_HTON_WORD
    ! 
    ! 	Convert host-order word to network-order
    ! 
    ! 	value	= word to convert
    ! 
      EXTERNAL WORD FUNCTION  netlib_hton_word &
               ( &
                   WORD  BY REF &
               )
    ! 
    !  NETLIB_NTOH_WORD
    ! 
    ! 	Convert network-order word to host-order
    ! 
    ! 	value	= word to convert
    ! 
      EXTERNAL WORD FUNCTION  netlib_ntoh_word &
               ( &
                   WORD  BY REF &
               )
    ! 
    !  NETLIB_VERSION
    ! 
    ! 	Return NETLIB version
    ! 	
    ! 	strver	= string to receive version string
    ! 	retlen	= optional, return length of string
    ! 
      EXTERNAL LONG FUNCTION  netlib_version &
               ( &
                  OPTIONAL STRING  BY DESC, &
                   WORD  BY REF &
               )
    ! 
    !  SSL Routines
    ! 
      EXTERNAL LONG FUNCTION  netlib_ssl_context
    ! 
    !  NETLIB_SSL_SOCKET
    ! 
    ! 	Allocate an SSL socket
    ! 
    !   context = SSL socket
    ! 	socket	= NETLIB socket
    ! 	ssl_ctx	= SSL_CTX structure
    ! 
      EXTERNAL LONG FUNCTION  netlib_ssl_socket &
               ( &
                   LONG  BY REF, &
                   LONG  BY REF, &
                   LONG  BY REF &
               )
    ! 
    !  NETLIB_SSL_ACCEPT
    ! 
    ! 	Accept incoming SSL connection
    ! 
    !   context = SSL socket
    !   timeout	= optional, read timeout
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_ssl_accept &
               ( &
                   LONG  BY REF, &
                   BASIC$QUADWORD  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_SSL_CONNECT
    ! 
    ! 	Establish an SSL connection to a remote system.
    ! 
    !   context = SSL socket
    !   timeout	= optional, read timeout
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_ssl_connect &
               ( &
                   LONG  BY REF, &
                   BASIC$QUADWORD  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_SSL_SHUTDOWN
    ! 
    ! 	Shutdown SSL socket (don't delete socket)
    ! 
    !   context = SSL socket
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_ssl_shutdown &
               ( &
                   LONG  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_SSL_CLOSE
    ! 
    ! 	Close an SSL socket
    ! 
    ! 	socket	= socket to close
    ! 
      EXTERNAL LONG FUNCTION  netlib_ssl_close &
               ( &
                   LONG  BY REF &
               )
    ! 
    !  NETLIB_SSL_READ
    ! 
    ! 	Read data from SSL socket
    ! 
    !   context = SSL socket
    !   buffer	= receive buffer
    !   timeout	= optional, read timeout
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_ssl_read &
               ( &
                   LONG  BY REF, &
                   STRING  BY DESC, &
                   BASIC$QUADWORD  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_SSL_WRITE
    ! 
    ! 	Write data to SSL socket
    ! 
    !   context = SSL socket
    !   buffer	= receive buffer
    !   timeout	= optional, read timeout
    ! 	iosb	= optional, I/O status block
    ! 	astadr	= optional, I/O completion AST
    ! 	astprm	= optional, AST parameter
    ! 
      EXTERNAL LONG FUNCTION  netlib_ssl_write &
               ( &
                   LONG  BY REF, &
                   STRING  BY DESC, &
                   BASIC$QUADWORD  BY REF, &
                  OPTIONAL NETLIBIOSBDEF  BY REF, &
                   LONG  BY VALUE, &
                   LONG  BY VALUE &
               )
    ! 
    !  NETLIB_SSL_VERSION
    ! 
    ! 	Return OpenSSL library version
    ! 	
    ! 	strver	= optional, string to receive version string
    ! 	retlen	= optional, return length of string
    ! 	numver  = optional, longword to receive version as number
    ! 
      EXTERNAL LONG FUNCTION  netlib_ssl_version &
               ( &
                  OPTIONAL STRING  BY DESC, &
                   WORD  BY REF, &
                   LONG  BY REF &
               )
