/*[[ CMS REPLACEMENT HISTORY, Element ECHOSERVER.C]]*/
/*[[ *1    12-MAR-1998 16:40:58 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element ECHOSERVER.C]]*/
/*
**++
**  FACILITY:	NETLIB test "forked" server
**
**  ABSTRACT:	Echo server.
**
**  MODULE DESCRIPTION:
**
**  	This is a test server that simply returns any characters sent
**  	to it over a TCP connection.  It is primarily intended for testing
**  	NETLIB's "forked" server compatibility.
**
**  AUTHOR: 	    M. Madison
**  	    	    COPYRIGHT © 1994, MADGOAT SOFTWARE.  ALL RIGHTS RESERVED.
**
**  CREATION DATE:  17-NOV-1994
**
**  MODIFICATION HISTORY:
**
**  	17-NOV-1994 V1.0    Madison 	Initial coding.
**--
*/
#include "netlib_dir:netlibdef.h"
#include <descrip.h>
#include <stsdef.h>
#include <ssdef.h>
#include <string.h>
/*
**  The following definition should match the port number
**  you have configured for this service in your master server configuration.
*/
    const unsigned short ECHO_PORT = 2000;
/*
**  Forward declarations
*/
    unsigned int main(void);

/*
**++
**  ROUTINE:	main
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Echo server main routine.  Establishes the service connection,
**  	then just echoes any received data back to the sender.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	main()
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
unsigned int main (void) {

    struct SINDEF server_sin;
    struct dsc$descriptor dsc;
    char chr;
    unsigned int status, context;
    const unsigned int sinsize = sizeof(struct SINDEF);
/*
**  The server setup MUST precede any CRTL I/O calls!
**
**  The socket address argument is ignored by all TCP/IPs except for
**  CMU TCP/IP, which requires it, due to the way forked servers are
**  implemented in that package.
*/
    memset(&server_sin, 0, sinsize);
    server_sin.sin_w_port = netlib_hton_word(&ECHO_PORT);

    status = netlib_server_setup(&context, &server_sin, &sinsize);
    if (!$VMS_STATUS_SUCCESS(status)) return status;

    dsc.dsc$b_dtype = DSC$K_DTYPE_T;
    dsc.dsc$b_class = DSC$K_CLASS_S;
    dsc.dsc$a_pointer = &chr;
    dsc.dsc$w_length = 1;

/*
**  The logic here is very simple because we're doing the I/O 1 byte
**  at a time.  As soon as the 1-byte buffer is filled, the read request
**  will complete; we then immediately echo the data back again and
**  start up another read.
*/
    while (1) {

    	status = netlib_read(&context, &dsc);
    	if (!$VMS_STATUS_SUCCESS(status)) break;
    	status = netlib_write(&context, &dsc);
    	if (!$VMS_STATUS_SUCCESS(status)) break;

    }

/*
**  LINKDISCON is the normal ending status for us -- it just means
**  the remote side closed down the connection.
*/
    if (status == SS$_LINKDISCON) status = SS$_NORMAL;

    netlib_close(&context);

    return status;

} /* main */
