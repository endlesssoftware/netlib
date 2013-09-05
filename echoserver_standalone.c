/*[[ CMS REPLACEMENT HISTORY, Element ECHOSERVER_STANDALONE.C]]*/
/*[[ *1    12-MAR-1998 16:40:59 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element ECHOSERVER_STANDALONE.C]]*/
/*
**++
**  FACILITY:	NETLIB test server (standalone)
**
**  ABSTRACT:	Standalone version of the echo server.
**
**  MODULE DESCRIPTION:
**
**  	This is a test server for NETLIB.  It implements an "echo" server
**  	via TCP, which is supposed to echo back any characters sent to it.
**
**  	We do character-at-a-time I/O, which isn't very efficient, but
**  	it's a good test of NETLIB's asynchronous I/O capabilities.
**
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
#include <stdlib.h>
#include <string.h>
#include <descrip.h>
#include <stsdef.h>
#include <ssdef.h>
#include <lib$routines.h>
#include <starlet.h>
#ifdef __DECC
#include <builtins.h>
#else
#pragma builtins
#endif

#define INIT_SDESC(dsc, len, ptr) {(dsc).dsc$b_dtype = DSC$K_DTYPE_T;\
    (dsc).dsc$b_class = DSC$K_CLASS_S; (dsc).dsc$w_length = (len);\
    (dsc).dsc$a_pointer = (ptr);}

/*
**  The following definition should match the port number you use
**  in the client.
*/
    const unsigned short ECHO_PORT = 2000;

    static unsigned int sinsize = sizeof(struct SINDEF);
/*
**  I/O request
*/
    typedef struct IORequest {
    	struct IORequest *flink, *blink;
    	void *network_context;
    	struct NETLIBIOSBDEF iosb;
    	struct SINDEF remsin;
    	struct dsc$descriptor bufdsc;
    	struct dsc$descriptor adrdsc;
    	unsigned int sinlen, count;
    	char buf[1];
    	char remaddr[256];
    } IORequest;
/*
**  Queue
*/
    typedef struct Queue {void *head, *tail;} Queue;
/*
**  Forward declarations
*/
    unsigned int main(int argc, char *argv[]);
    void Accept_AST(IORequest *ior);
    void Read_AST(IORequest *ior);
    void Write_AST(IORequest *ior);
    IORequest *Allocate_IOR(void);
    IORequest *Get_IOR(void);
/*
**  OWN storage
*/
    static Queue Free_Queue = {&Free_Queue, &Free_Queue};
    static void *network_listener;
    static int Done = 0;
    static unsigned int completion_status;

/*
**++
**  ROUTINE:	main
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Echo server main routine.  Waits for incoming service connections,
**  	then starts a chain of ASTs that echo the data received back to the
**  	sender on each connection.
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
unsigned int main (int argc, char *argv[]) {

    IORequest *ior;
    struct SINDEF sin;
    unsigned int status;
    struct dsc$descriptor dsc;
    unsigned short port, buflen;
    int i;
    char buf[512];
    static $DESCRIPTOR(faodsc, "Listener started on port !UW, use CTRL/Y to stop.");

    status = netlib_socket(&network_listener);
    if (!$VMS_STATUS_SUCCESS(status)) return status;

    memset(&sin, 0, sinsize);
    port = (argc > 1) ? atoi(argv[1]) : ECHO_PORT;
    sin.sin_w_port = netlib_hton_word(&port);
    status = netlib_bind(&network_listener, &sin, &sinsize);
    if (!$VMS_STATUS_SUCCESS(status)) return status;
    status = netlib_listen(&network_listener);
    if (!$VMS_STATUS_SUCCESS(status)) return status;

    INIT_SDESC(dsc, sizeof(buf), buf);
    sys$fao(&faodsc, &buflen, &dsc, port);
    dsc.dsc$w_length = buflen;
    lib$put_output(&dsc);

    for (i = 0; i < 16; i++) {
    	ior = Allocate_IOR();
    	if (ior == 0) break;
    	_INSQUE(ior, Free_Queue.tail);
    }

    ior = Get_IOR();
    if (ior == 0) return SS$_INSFMEM;
    status = netlib_accept(&network_listener, &ior->network_context,
    	    	    	    &ior->remsin, &sinsize, &ior->sinlen,
    	    	    	    &ior->iosb, Accept_AST, ior);
    if (!$VMS_STATUS_SUCCESS(status)) return status;

    while (!Done) {
    	sys$hiber();
    }

    netlib_close(&network_listener);

    return SS$_NORMAL;

} /* main */

/*
**++
**  ROUTINE:	Accept_AST
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Completion AST for accepts.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	Accept_AST(ior)
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
void Accept_AST (IORequest *ior) {

    unsigned int status;
    struct dsc$descriptor dsc;
    char buf[512];
    unsigned short buflen;
    static $DESCRIPTOR(faodsc, "Connection accepted from !AS, port !UW");

    if ($VMS_STATUS_SUCCESS(ior->iosb.iosb_w_status)) {
    	netlib_addrtostr(&ior->remsin.sin_x_addr, &ior->adrdsc, &buflen);
    	ior->adrdsc.dsc$w_length = buflen;
    	INIT_SDESC(dsc, sizeof(buf), buf);
    	sys$fao(&faodsc, &buflen, &dsc, &ior->adrdsc,
    	    	    netlib_ntoh_word(&ior->remsin.sin_w_port));
    	dsc.dsc$w_length = buflen;
    	lib$put_output(&dsc);

    	status = netlib_read(&ior->network_context, &ior->bufdsc,
    	    	    	    	0, 0, 0, 0, &ior->iosb, Read_AST, ior);

    	ior = Get_IOR();
    	if (ior == 0) {
    	    completion_status = SS$_INSFMEM;
    	    Done = 1;
    	    sys$wake(0,0);
    	    return;
    	}
    }

    status = netlib_accept(&network_listener, &ior->network_context,
    	    	    	    &ior->remsin, &sinsize, &ior->sinlen,
    	    	    	    &ior->iosb, Accept_AST, ior);
    if (!$VMS_STATUS_SUCCESS(status)) {
    	completion_status = status;
    	Done = 1;
    	sys$wake(0,0);
    }

} /* Accept_AST */

/*
**++
**  ROUTINE:	Read_AST
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Completion AST for network read.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	Read_AST(ior)
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
void Read_AST (IORequest *ior) {

    struct dsc$descriptor dsc, msgdsc;
    unsigned int status;
    int len;
    unsigned short buflen;
    char buf[512], msgbuf[256];

    static $DESCRIPTOR(faodsc,
    	"Connection from !AS port !UW closed, !UL bytes echoed:!/    !AS");

    status = ior->iosb.iosb_w_status;
    if ($VMS_STATUS_SUCCESS(status)) {
    	status = netlib_write(&ior->network_context, &ior->bufdsc, 0, 0,
    	    	    	    	&ior->iosb, Write_AST, ior);
    	if ($VMS_STATUS_SUCCESS(status)) return;
    }

    netlib_close(&ior->network_context);

    INIT_SDESC(msgdsc, sizeof(msgbuf), msgbuf);
    buflen = 0;
    sys$getmsg(status, &buflen, &msgdsc, 15, 0);
    msgdsc.dsc$w_length = buflen;
    INIT_SDESC(dsc, sizeof(buf), buf);
    sys$fao(&faodsc, &buflen, &dsc, &ior->adrdsc,
    	    	netlib_ntoh_word(&ior->remsin.sin_w_port), ior->count, &msgdsc);
    dsc.dsc$w_length = buflen;
    lib$put_output(&dsc);
    
    _INSQUE(ior, Free_Queue.head);

} /* Read_AST */

/*
**++
**  ROUTINE:	Write_AST
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Completion AST for terminal and network writes.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	Write_AST(ior)
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
void Write_AST (IORequest *ior) {

    struct dsc$descriptor dsc;
    unsigned int status;
    int len;
    unsigned short buflen;
    char buf[512];

    static $DESCRIPTOR(faodsc,
    	"Connection from !AS port !UW closed, !UL bytes echoed. (write error)");

    status = ior->iosb.iosb_w_status;
    if ($VMS_STATUS_SUCCESS(status)) {
    	ior->count++;
    	status = netlib_read(&ior->network_context, &ior->bufdsc,
    	    	    	    	0, 0, 0, 0, &ior->iosb, Read_AST, ior);
    }

    if (!$VMS_STATUS_SUCCESS(status)) {

    	netlib_close(&ior->network_context);

    	INIT_SDESC(dsc, sizeof(buf), buf);
    	sys$fao(&faodsc, &buflen, &dsc, &ior->adrdsc,
    	    	    netlib_ntoh_word(&ior->remsin.sin_w_port), ior->count);
    	dsc.dsc$w_length = buflen;
    	lib$put_output(&dsc);

    	_INSQUE(ior, Free_Queue.head);
    	return;
    }

} /* Write_AST */

/*
**++
**  ROUTINE:	Allocate_IOR
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Allocates an IORequest.
**
**  RETURNS:    IORequest *
**
**  PROTOTYPE:
**
**  	Allocate_IOR()
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	0 = allocation failure
**
**  SIDE EFFECTS:   	None.
**
**--
*/
IORequest *Allocate_IOR (void) {

    IORequest *ior;
    unsigned int status;
    const unsigned int iorsize = sizeof(IORequest);

    status = lib$get_vm(&iorsize, &ior);
    if (!$VMS_STATUS_SUCCESS(status)) return 0;

    memset(ior, 0, iorsize);
    INIT_SDESC(ior->bufdsc, 1, ior->buf);
    INIT_SDESC(ior->adrdsc, sizeof(ior->remaddr), ior->remaddr);

    return ior;

} /* Allocate_IOR */

/*
**++
**  ROUTINE:	Get_IOR
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Gets a free IORequest, either off the free queue or
**  	by allocating one.
**
**  RETURNS:	IORequest *
**
**  PROTOTYPE:
**
**  	Get_IOR()
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	0 = couldn't get one
**
**  SIDE EFFECTS:   	None.
**
**--
*/
IORequest *Get_IOR (void) {

    IORequest *ior;

    if (_REMQUE(Free_Queue.head, (void **) &ior) & 1) return Allocate_IOR();
    ior->count = 0;
    INIT_SDESC(ior->adrdsc, sizeof(ior->remaddr), ior->remaddr);

    return ior;

} /* Get_IOR */
