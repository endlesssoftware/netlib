/*[[ CMS REPLACEMENT HISTORY, Element ECHOCLIENT.C]]*/
/*[[ *1    12-MAR-1998 16:40:58 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element ECHOCLIENT.C]]*/
/*
**++
**  FACILITY:	NETLIB test client
**
**  ABSTRACT:	Echo client.
**
**  MODULE DESCRIPTION:
**
**  	This is a test client for NETLIB.  It connects to an "echo" server
**  	via TCP, which is supposed to echo back any characters sent to it.
**
**  	We do character-at-a-time I/O, which isn't very efficient, but
**  	it's a good test of NETLIB's asynchronous I/O capabilities.
**
**  	N.B.: This program expects that SYS$INPUT and SYS$OUTPUT point
**  	      to terminal devices.
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <descrip.h>
#include <stsdef.h>
#include <ssdef.h>
#include <iodef.h>
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
**  This is the default port we use (matches the accompanying server program).
*/
    const unsigned short ECHO_PORT = 2000;

/*
**  Other statics we need because we have to pass these values by
**  reference
*/
    static unsigned int sinsize = sizeof(struct SINDEF);
    static unsigned int adrsize = sizeof(struct INADDRDEF);
    static unsigned int which_dns = NETLIB_K_LOOKUP_DNS;
    static unsigned int which_ht  = NETLIB_K_LOOKUP_HOST_TABLE;
/*
**  I/O request
*/
    typedef struct IORequest {
    	struct IORequest *flink, *blink;
    	struct NETLIBIOSBDEF iosb;
    	struct dsc$descriptor bufdsc;
    	char buf[2];
    } IORequest;
/*
**  Queue
*/
    typedef struct Queue {void *head, *tail;} Queue;
/*
**  Forward declarations
*/
    unsigned int main(int argc, char *argv[]);
    void Terminal_Read_AST(IORequest *ior);
    void Network_Read_AST(IORequest *ior);
    void Write_AST(IORequest *ior);
    IORequest *Allocate_IOR(void);
    IORequest *Get_IOR(void);
/*
**  OWN storage
*/
    static Queue Free_Queue = {&Free_Queue, &Free_Queue};
    static $DESCRIPTOR(input_device, "SYS$INPUT");
    static $DESCRIPTOR(output_device, "SYS$OUTPUT");
    static $DESCRIPTOR(help, "Usage: ECHOCLIENT hostname [port-number]");
    static void *network_context;
    static unsigned short input_channel;
    static unsigned short output_channel;
    static int Done = 0;
    static unsigned int completion_status;

/*
**++
**  ROUTINE:	main
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Echo client main routine.  Establishes the service connection,
**  	then reads data from the terminal and sends it off to the echo
**  	server.  Any data read from the echo server is displayed on the
**  	terminal.
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
    struct SINDEF remsin;
    struct dsc$descriptor dsc;
    unsigned int status, remlen;
    unsigned short port, namelen;
    char name[1024], buf[1024];
    int i;

    if (argc < 2) {
    	lib$put_output(&help);
    	return SS$_NORMAL;
    }

/*
**  Get channels to the input and output devices
*/
    status = sys$assign(&input_device, &input_channel, 0, 0);
    if (!$VMS_STATUS_SUCCESS(status)) return status;
    status = sys$assign(&output_device, &output_channel, 0, 0);
    if (!$VMS_STATUS_SUCCESS(status)) return status;

/*
**  Allocate a network socket and connect to the server
*/
    status = netlib_socket(&network_context);
    if (!$VMS_STATUS_SUCCESS(status)) return status;

    INIT_SDESC(dsc, strlen(argv[1]), argv[1]);
    port = (argc > 2) ? atoi(argv[2]) : ECHO_PORT;
    status = netlib_connect_by_name(&network_context, &dsc, &port);
    if (!$VMS_STATUS_SUCCESS(status)) return status;

/*
**  Let the user know who we're connected to
*/
    status = netlib_getpeername(&network_context, &remsin, &sinsize, &remlen);
    if ($VMS_STATUS_SUCCESS(status)) {
    	INIT_SDESC(dsc, sizeof(name), name);
    	status = netlib_address_to_name(&network_context, &which_dns,
    	    	    &remsin.sin_x_addr, &adrsize, &dsc, &namelen);
    	if (!$VMS_STATUS_SUCCESS(status))
    	    status = netlib_address_to_name(&network_context, &which_ht,
    	    	    &remsin.sin_x_addr, &adrsize, &dsc, &namelen);
    	if (!$VMS_STATUS_SUCCESS(status))
    	    status = netlib_addrtostr(&remsin.sin_x_addr, &dsc, &namelen);
    	if ($VMS_STATUS_SUCCESS(status)) {
    	    i = sprintf(buf, "Connected to host %.*s, port %d.  CTRL/Z exits.",
    	    	namelen, name, netlib_ntoh_word(&remsin.sin_w_port));
    	    INIT_SDESC(dsc, i, buf);
    	    lib$put_output(&dsc);
    	    dsc.dsc$w_length = 0;
    	    lib$put_output(&dsc);
    	}
    }

/*
**  Pre-load the free queue
*/
    for (i = 0; i < 16; i++) {
    	ior = Allocate_IOR();
    	if (ior == 0) break;
    	_INSQUE(ior, Free_Queue.tail);
    }

/*
**  Start a terminal read
*/
    ior = Get_IOR();
    if (ior == 0) return SS$_INSFMEM;
    status = sys$qio(0, input_channel,
    	    	IO$_READVBLK|IO$M_NOECHO|IO$M_NOFILTR,
    	    	&ior->iosb, Terminal_Read_AST, ior,
    	    	ior->buf, 1, 0, 0, 0, 0);
    if (!$VMS_STATUS_SUCCESS(status)) return status;
    ior = Get_IOR();
    if (ior == 0) {
    	sys$cancel(input_channel);
    	return SS$_INSFMEM;
    }

/*
**  Start a network read
*/
    status = netlib_read(&network_context, &ior->bufdsc,
    	    	    	  0, 0, 0, 0, &ior->iosb,
    	    	    	  Network_Read_AST, ior);
    if (!$VMS_STATUS_SUCCESS(status)) {
    	sys$cancel(input_channel);
    	return SS$_INSFMEM;
    }

/*
**  Let the ASTs do all the work.  They will set the Done flag
**  when it's time for us to shut down.
*/
    while (!Done) {
    	sys$hiber();
    }

/*
**  Close down the network socket and terminal channels.
*/
    netlib_shutdown(&network_context);
    netlib_close(&network_context);
    sys$dassgn(input_channel);
    sys$dassgn(output_channel);

    return SS$_NORMAL;

} /* main */

/*
**++
**  ROUTINE:	Terminal_Read_AST
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Read completion AST for terminal input.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	Terminal_Read_AST(ior)
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
void Terminal_Read_AST (IORequest *ior) {

    unsigned int status;
/*
**  Exit on error or ctrl/Z.
*/
    if (!$VMS_STATUS_SUCCESS(ior->iosb.iosb_w_status)
    	  || ior->buf[0] == 26) {
    	if (!Done) completion_status = ior->iosb.iosb_w_status;
    	Done = 1;
    	sys$wake(0,0);
    	return;
    }

/*
**  Write the data to the server.
*/
    status = netlib_write(&network_context, &ior->bufdsc,
    	    	0, 0, &ior->iosb, Write_AST, ior);
    if ($VMS_STATUS_SUCCESS(status)) {
    	ior = Get_IOR();
    	if (ior == 0) status = SS$_INSFMEM;
    	else status = sys$qio(0, input_channel,
    	    	    	IO$_READVBLK|IO$M_NOECHO|IO$M_NOFILTR,
    	    	    	&ior->iosb, Terminal_Read_AST, ior,
    	    	    	ior->buf, 1, 0, 0, 0, 0);
    }

    if (!$VMS_STATUS_SUCCESS(status)) {
    	if (!Done) completion_status = ior->iosb.iosb_w_status;
    	Done = 1;
    	sys$wake(0,0);
    	return;
    }

} /* Terminal_Read_AST */

/*
**++
**  ROUTINE:	Network_Read_AST
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Completion AST for network read.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	Network_Read_AST(ior)
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
void Network_Read_AST (IORequest *ior) {

    unsigned int status;
    int len;

/*
**  If the read completed successfully, format the data we got
**  and write it to the terminal.
*/
    status = ior->iosb.iosb_w_status;
    if ($VMS_STATUS_SUCCESS(status)) {
    	len = 1;
    	if (ior->buf[0] == 0x7f) {
    	    ior->buf[0] = '^';
    	    ior->buf[1] = '?';
    	    len = 2;
    	} else if (ior->buf[0] == '\r' || ior->buf[0] == '\n') {
    	    ior->buf[0] = '\r';
    	    ior->buf[1] = '\n';
    	    len = 2;
    	} else if (ior->buf[0] < ' ') {
    	    ior->buf[1] = ior->buf[0] + '@';
    	    ior->buf[0] = '^';
    	    len = 2;
    	}
    	status = sys$qio(0, output_channel, IO$_WRITEVBLK,
    	    	    &ior->iosb, Write_AST, ior, ior->buf, len,
    	    	    0, 0, 0, 0);
    	if ($VMS_STATUS_SUCCESS(status)) {
    	    ior = Get_IOR();
    	    if (ior == 0) status = SS$_INSFMEM;
    	    else status = netlib_read(&network_context, &ior->bufdsc,
    	    	    	    	0, 0, 0, 0, &ior->iosb,
    	    	    	    	Network_Read_AST, ior);
    	}
    }

    if (!$VMS_STATUS_SUCCESS(status)) {
    	if (!Done) completion_status = ior->iosb.iosb_w_status;
    	Done = 1;
    	sys$wake(0,0);
    }

} /* Network_Read_AST */

/*
**++
**  ROUTINE:	Write_AST
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Completion AST for terminal and network writes.  It checks
**  for successful completion and frees the I/O request block.
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

    if (!$VMS_STATUS_SUCCESS(ior->iosb.iosb_w_status)) {
    	if (!Done) completion_status = ior->iosb.iosb_w_status;
    	Done = 1;
    	sys$wake(0,0);
    	return;
    }

    _INSQUE(ior, Free_Queue.tail);

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

    return ior;

} /* Allocate_IOR */

/*
**++
**  ROUTINE:	Get_IOR
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Gets a free IORequest, either off the free queue or by
**  allocating one.
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

    return ior;

} /* Get_IOR */
