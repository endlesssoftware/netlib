$! [NETLIB]NETLIB_INSTALL.COM
$!
$!  KITINSTAL procedure for installing NETLIB.
$!
$! Copyright (c) 2005, Matthew Madison.
$! Copyright (c) 2013, Endless Software Solutions.
$!
$! All rights reserved.
$!
$! Redistribution and use in source and binary forms, with or without
$! modification, are permitted provided that the following conditions
$! are met:
$!
$!     * Redistributions of source code must retain the above
$!       copyright notice, this list of conditions and the following
$!       disclaimer.
$!     * Redistributions in binary form must reproduce the above
$!       copyright notice, this list of conditions and the following
$!       disclaimer in the documentation and/or other materials provided
$!       with the distribution.
$!     * Neither the name of the copyright owner nor the names of any
$!       other contributors may be used to endorse or promote products
$!       derived from this software without specific prior written
$!       permission.
$!
$! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
$! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
$! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
$! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
$! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
$! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
$! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
$! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
$! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
$! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
$! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
$!
$!  31-JAN-1991	V1.0	Madison	    Initial coding.
$!  05-FEB-1991	V1.0-1	Madison	    Two parts for integration into other kits.
$!  26-APR-1991	V1.0-2	Madison	    Nameserver changes.
$!  20-JUN-1991	V1.0-3	Madison	    Newer versions of some pkgs supported.
$!  03-DEC-1991	V1.0-4	Madison	    A bit of rearranging of the startup file.
$!  01-FEB-1992 V1.1	Volz	    Added TCPware support.
$!  17-FEB-1992	V1.1-1	Madison	    LINK/NOUSERLIB.
$!  08-OCT-1992	V1.2	Madison	    Alpha AXP.
$!  19-FEB-1993	V1.2-1	Madison	    Accommodate TCPware V3.0 filename changes.
$!  22-MAR-1993	V1.2-2	Madison	    TCPware now runs on AXP systems.
$!  26-MAR-1993	V1.2-3	Madison	    Eliminate IP0/IPA0 confusion for CMU-Tek.
$!  21-DEC-1993	V1.3	Madison	    First crack at WINTCP support.
$!  27-DEC-1993	V1.3-1	Madison	    Second crack at WINTCP support.
$!  27-JAN-1994	V1.3-2	Madison	    TCP0 device can exist with MultiNet too.
$!  21-SEP-1994	V1.4	Madison	    Moved AXP objects to separate save set.
$!  04-NOV-1994	V1.5	Madison	    Update for V2.0.
$!  03-JAN-1995	V1.5-1	Madison	    Update for OEM kitting.
$!  08-JAN-1996	V1.5-2	Madison	    Fix for OEM kitting.
$!  17-FEB-1997	V1.6	Madison	    Set NETLIB_SHR at startup time.
$!  27-NOV-1997	V1.7	Madison	    Version info in separate OPT file.
$!  08-APR-1998	V1.7-1	Madison	    UCX can have an INET0 device.
$!  22-DEC-1998	V1.8	Madison	    Add support for UCX V5.0.
$!  26-DEC-2000	V2.0	Madison	    Remove link step.
$!  07-NOV-2004 V2.1    Madison     IA64 support.  Remove VAX support.
$!  05-SEP-2013 V2.2    Sneddon     VAX support.
$!
$ ON CONTROL_Y THEN GOTO NETLIB_CONTROL_Y
$ ON WARNING THEN GOTO NETLIB_FAIL
$!
$ GOTO NETLIB_INSTALL
$!
$NETLIB_CONTROL_Y:
$ IF F$TRNLNM ("NETLIB_STUP") .NES. "" THEN CLOSE NETLIB_STUP
$ VMI$CALLBACK CONTROL_Y
$!
$NETLIB_FAIL:
$ NETLIB_STATUS == $STATUS
$ IF F$TRNLNM ("NETLIB_STUP") .NES. "" THEN CLOSE NETLIB_STUP
$ EXIT 'NETLIB_STATUS
$!
$NETLIB_INSTALL:
$!
$ IF P2 THEN SET VERIFY
$ NETLIB_SAY := WRITE SYS$OUTPUT
$!
$ OPEN/READ NETLIB_TMP VMI$KWD:NETLIB_INSTALLING_VERSION.DAT
$ READ NETLIB_TMP NETLIB_VERSION
$ CLOSE NETLIB_TMP
$!
$ NETLIB_SAY ""
$ VMI$CALLBACK MESSAGE I INSTALVERS "Installing ''NETLIB_VERSION'."
$!
$ NETLIB_VERSION_FULL = NETLIB_VERSION
$ NETLIB_VERSION = F$EDIT (NETLIB_VERSION - "NETLIB ", "TRIM,UPCASE,COMPRESS")
$ NETLIB_V_TAG = F$EXTRACT (0, 1, NETLIB_VERSION)
$ NETLIB_VERSION = F$EXTRACT (1, -1, NETLIB_VERSION)
$!
$ IF NETLIB_AXP THEN exe_sfx = "AXP_EXE"
$ IF NETLIB_I64 THEN exe_sfx = "I64_EXE"
$ IF NETLIB_I64 THEN exe_sfx = "VAX_EXE"
$ IF F$TRNLNM ("NETLIB_SHR") .NES. "" .AND. F$SEARCH (F$PARSE ("NETLIB_SHR", ".EXE")) .NES. ""
$ THEN
$   SET NOON
$   DEFINE/USER SYS$OUTPUT _NL:
$   DEFINE/USER SYS$ERROR _NL:
$   RUN VMI$KWD:NETLIB_GET_VERSION.'exe_sfx'
$   SET ON
$ ENDIF
$!
$!  An "OEM" install is one in which the NETLIB savesets are embedded
$!  in another kit.  The NETLIB programming files and documentation are
$!  not included in an OEM kit.
$!
$ NETLIB_OEM_INSTALL = F$TRNLNM("NETLIB_OEM_INSTALL","LNM$PROCESS") .NES. ""
$ NETLIB_OEM_PRODUCT = F$TRNLNM("NETLIB_OEM_PRODUCT","LNM$PROCESS")
$ IF NETLIB_OEM_PRODUCT .EQS. "" THEN NETLIB_OEM_PRODUCT = "this product"
$ NETLIB_REQD_VERSION = ""
$ IF NETLIB_OEM_INSTALL THEN
    NETLIB_REQD_VERSION = F$TRNLNM("NETLIB_OEM_REQUIRED_VERSION","LNM$PROCESS")
$!
$ IF "''NETLIB_OLD_VERSION'" .NES. ""
$ THEN
$   netlib_tmp = "YES"
$   IF NETLIB_VERSION .LTS. NETLIB_OLD_VERSION .OR. -
       (NETLIB_VERSION .EQS. NETLIB_OLD_VERSION .AND. NETLIB_V_TAG .LES. NETLIB_OLD_V_TAG) THEN -
    	netlib_tmp = "NO"
$   NETLIB_SAY ""
$   NETLIB_SAY "    NETLIB ", NETLIB_OLD_V_TAG, NETLIB_OLD_VERSION, " has been detected on your system."
$   IF NETLIB_OEM_INSTALL .AND. NETLIB_REQD_VERSION .NES. "" .AND. NETLIB_OLD_VERSION .LTS. NETLIB_REQD_VERSION THEN -
    	NETLIB_SAY "    Installation of ''NETLIB_OEM_PRODUCT' requires NETLIB V''NETLIB_REQD_VERSION' or later."
$   NETLIB_SAY ""
$   VMI$CALLBACK ASK NETLIB_OK "Do you want to install ''NETLIB_VERSION_FULL'" "''netlib_tmp'" B
$   IF .NOT. NETLIB_OK .AND. NETLIB_OEM_INSTALL .AND. NETLIB_REQD_VERSION .NES. "" .AND. -
    	    NETLIB_OLD_VERSION .LTS. NETLIB_REQD_VERSION
$   THEN
$   	NETLIB_SAY ""
$   	NETLIB_SAY "    Installation of ''NETLIB_OEM_PRODUCT' requires NETLIB V''NETLIB_REQD_VERSION' or later."
$   	NETLIB_SAY "    Skipping the NETLIB upgrade will cause the installation to fail."
$   	NETLIB_SAY ""
$   	VMI$CALLBACK ASK NETLIB_OK "Install ''NETLIB_VERSION_FULL'" "YES" B
$   	IF .NOT. NETLIB_OK THEN EXIT VMI$_FAILURE
$   ENDIF
$   IF .NOT. NETLIB_OK
$   THEN
$   	VMI$CALLBACK MESSAGE I SKIPINSTALL "Skipping installation of ''NETLIB_VERSION_FULL'."
$   	EXIT VMI$_SUCCESS
$   ENDIF
$ ENDIF
$!
$ NETLIB_INSTALL_NODE = F$GETSYI ("NODENAME")
$!
$NETLIB_DIR_ASK:
$!
$!
$ TYPE SYS$INPUT:


                     Choosing the NETLIB Directory

    The NETLIB libraries can be placed in any directory, as long
    as that directory is accessible to all users who plan to use
    or develop NETLIB-based applications.

$ NETLIB_DEFANS := SYS$COMMON:[SYSLIB]
$ IF F$TRNLNM ("NETLIB_DIR") .NES. "" THEN -
    NETLIB_DEFANS = F$TRNLNM ("NETLIB_DIR")
$ VMI$CALLBACK ASK NETLIB_DIR -
    "Where should the NETLIB libraries be placed" "''NETLIB_DEFANS'"
$ NETLIB_DIR = F$PARSE (NETLIB_DIR,,,"DEVICE") +-
     F$PARSE (NETLIB_DIR,,,"DIRECTORY")
$ IF F$SEARCH ("''NETLIB_DIR'X.X") .EQS. "" THEN -
    VMI$CALLBACK CREATE_DIRECTORY USER 'NETLIB_DIR -
    	"/OWNER=[1,4]/PROTECT=(S:RWE,O:RWE,G:RE,W:RE)"
$!
$!  Skip the support & docs questions if this is an "OEM" install.
$!
$ IF NETLIB_OEM_INSTALL
$ THEN
$   NETLIB_DO_PRG = 0
$   NETLIB_DO_DOC = 0
$   GOTO BEGIN_INSTALLING_FILES
$ ENDIF
$!
$ TYPE SYS$INPUT:

             NETLIB Programming Support and Documentation

    The NETLIB programming support files include:

        - include files for programming in C and BLISS
        - sample C source code client and server programs

    Installation of these files is optional, unless you or your users
    intend to develop NETLIB-based applications.  If you elect to install
    these files, they will be placed in the NETLIB directory.

    The documentation set includes a Programmer's Guide and an
    Installation Guide, in PostScript, PDF, HTML and plain ASCII forms.
    If you elect to install the documentation, you will be prompted
    for a directory into which the files will be placed; if that
    directory does not exist, it will be created for you.

$!
$ VMI$CALLBACK ASK NETLIB_DO_PRG -
    	"Would you like to install the NETLIB programming files" "YES" B
$!
$ VMI$CALLBACK ASK NETLIB_DO_DOC -
    	"Would you like to install the NETLIB documentation" "YES" B
$!
$ IF .NOT. NETLIB_DO_DOC THEN GOTO BEGIN_INSTALLING_FILES
$!
$ NETLIB_DEFANS = NETLIB_DIR
$ VMI$CALLBACK ASK NETLIB_DOC_DIR "Location for documentation" "''NETLIB_DEFANS'"
$ NETLIB_DOC_DIR = F$PARSE(NETLIB_DOC_DIR,,,"DEVICE") +-
    	    	     F$PARSE(NETLIB_DOC_DIR,,,"DIRECTORY")
$ IF F$PARSE ("''NETLIB_DOC_DIR'X.X") .EQS. "" THEN -
    	VMI$CALLBACK CREATE_DIRECTORY USER 'NETLIB_DOC_DIR -
    	    "/OWNER=[1,4]/PROTECT=(S:RWE,O:RWE,G:RE,W:RE)"
$!
$BEGIN_INSTALLING_FILES:
$!
$ IF NETLIB_OEM_INSTALL THEN GOTO SKIP_TIME_MESSAGE
$ TYPE SYS$INPUT:

    All installation questions have been asked.  Installation will
    continue for the next 5-15 minutes, depending on installation media
    type and options selected.

$!
$SKIP_TIME_MESSAGE:
$! 
$ RENAME VMI$KWD:NETLIB_SHR.'exe_sfx' .EXE
$ RENAME VMI$KWD:NETLIB_SSL.'exe_sfx' .EXE
$ SET PROTECTION=W:RE VMI$KWD:NETLIB_SHR.EXE
$ SET PROTECTION=W:RE VMI$KWD:NETLIB_SSL.EXE
$ VMI$CALLBACK PROVIDE_IMAGE NETLIB_IMGOK NETLIB_SHR.EXE 'NETLIB_DIR'
$ VMI$CALLBACK PROVIDE_IMAGE NETLIB_IMGOK NETLIB_SSL.EXE 'NETLIB_DIR'
$!
$ CREATE VMI$KWD:NETLIB_STARTUP.COM
$ OPEN/APPEND NETLIB_STUP VMI$KWD:NETLIB_STARTUP.COM
$ WRITE NETLIB_STUP "$ V = 'F$VERIFY(0)"
$ WRITE NETLIB_STUP "$! SYS$STARTUP:NETLIB_STARTUP.COM"
$ WRITE NETLIB_STUP "$! Startup command procedure for NETLIB library routines."
$ WRITE NETLIB_STUP "$!"
$ WRITE NETLIB_STUP "$ SET := SET"
$ WRITE NETLIB_STUP "$ SET SYMBOL/SCOPE=(NOLOCAL,NOGLOBAL)"
$ WRITE NETLIB_STUP "$ DEFINE/SYSTEM/EXEC/NOLOG NETLIB_DIR ''NETLIB_DIR'"
$ IF NETLIB_DO_DOC 
$ THEN
$   WRITE NETLIB_STUP "$ DEFINE/SYSTEM/EXEC/NOLOG NETLIB_DOC_DIR ''NETLIB_DOC_DIR'"
$ ELSE
$   netlib_tmp = F$TRNLNM ("NETLIB_DOC_DIR", "LNM$SYSTEM", "EXECUTIVE_MODE")
$   IF netlib_tmp .NES. "" .AND. F$PARSE (netlib_tmp) .NES. "" THEN -
    	WRITE NETLIB_STUP "$ DEFINE/SYSTEM/EXEC/NOLOG NETLIB_DOC_DIR ''netlib_tmp'"
$ ENDIF
$ CLOSE NETLIB_STUP
$ APPEND VMI$KWD:NETLIB_STARTUP_TEMPLATE.COM VMI$KWD:NETLIB_STARTUP.COM
$ VMI$CALLBACK PROVIDE_FILE NETLIB_STUPFILE NETLIB_STARTUP.COM VMI$ROOT:[SYS$STARTUP]
$!
$!
$ IF NETLIB_DO_PRG .OR. NETLIB_DO_DOC THEN VMI$CALLBACK RESTORE_SAVESET B
$ IF NETLIB_DO_PRG
$ THEN
$   VMI$CALLBACK MESSAGE I INSTALLPRG "Installing NETLIB programming support..."
$   SET PROTECTION=W:RE VMI$KWD:NETLIBDEF.*
$   SET PROTECTION=W:RE VMI$KWD:ECHO*.*
$   DEFINE NETLIB_DIR 'NETLIB_DIR
$   VMI$CALLBACK PROVIDE_FILE "" NETLIBDEF_LIST.DAT "" T
$   VMI$CALLBACK PROVIDE_FILE NETLIB_OK ECHOCLIENT.C 'NETLIB_DIR
$   VMI$CALLBACK PROVIDE_FILE NETLIB_OK ECHOSERVER.C 'NETLIB_DIR
$   VMI$CALLBACK PROVIDE_FILE NETLIB_OK ECHOSERVER_STANDALONE.C 'NETLIB_DIR
$ ENDIF
$!
$!
$ IF NETLIB_DO_DOC
$ THEN
$   VMI$CALLBACK MESSAGE I INSTALLDOC "Installing NETLIB documentation..."
$   DEFINE NETLIB_DOC_DIR 'NETLIB_DOC_DIR
$   VMI$CALLBACK PROVIDE_FILE "" NETLIB_DOC_LIST.DAT "" T
$   SET PROTECTION=W:RE NETLIB_DOC_DIR:*.*
$ ENDIF
$!
$ NETLIB_INSTALLED == "YES"
$!
$ NETLIB_STATUS == VMI$_SUCCESS
$!
$ EXIT 'NETLIB_STATUS
