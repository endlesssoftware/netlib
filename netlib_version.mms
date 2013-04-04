!++
!   DESCRIP.MMS
!
!	NETLIB version numbers...
!
!   21-SEP-1994	V1.0	Madison	    Initial commenting.
!   31-OCT-1994	V2.0	Madison	    Rewrite for NETLIB V2.
!   30-SEP-1996	V2.1	Madison	    Fixups for non-MadGoat builds.
!   27-NOV-1997	V2.2	Madison	    Separate out version info.
!   12-MAR-1998	V2.2A	Madison	    CMSized, TCPware build change.
!   23-APR-1998	V2.2B	Madison	    Version check on install.
!   02-AUG-1998	V2.2C	Madison	    CMU name-to-address lookup fix.
!   15-NOV-1998	V2.2D	Madison	    MXLOOK fix.
!   12-DEC-1998	V2.2E	Madison	    Another TCPware fix.
!   26-DEC-2000	V2.3	Madison	    Kit installs .EXEs now.
!   18-JAN-2001	V2.3A	Madison	    DNS query fixes.
!   03-MAR-2001 V2.3B   Madison     Linemode returned length fix.
!   10-JAN-2002 V2.3C   Madison     strtoaddr fix.
!   04-FEB-2002 V2.3D   Madison     Correct strtoaddr fix.
!   21-SEP-2003 V2.3E   Madison     Fix for dns_skipname.
!   07-NOV-2004 V2.4    Madison     IA64 support.  Removed VAX.
!   04-APR-2013 V3.0	Sneddon     VAX support. SSL support.
!--

!+
! The following three lines control all the version information
! embedded throughout the code, linker options files, and documentation.
!-
NUM_VERSION  = 030
TEXT_VERSION = V3.0
GSMATCH      = LEQUAL,1,7
