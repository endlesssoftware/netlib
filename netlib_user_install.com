$! [NETLIB]NETLIB_USER_INSTALL.COM
$!
$!  Procedure for private installation of NETLIB.
$!  To use:
$!
$!  	$ CREATE/DIRECTORY [some working directory]
$!  	$ SET DEFAULT [working directory]
$!  	$ BACKUP NETLIBvvu.A/SAVE []
$!  	$ @NETLIB_USER_INSTALL
$!
$!  08-FEB-1991	V1.0	Madison	    Initial coding.
$!  01-FEB-1992 V1.1	Volz	    Added TCPware support.
$!  19-FEB-1993	V1.2	Madison	    AXP support, TCPware updates.
$!  22-MAR-1993	V1.2-1	Madison	    TCPware now runs on AXP systems.
$!  26-MAR-1993	V1.2-2	Madison	    Eliminate IP0/IPA0 confusion for CMU-Tek.
$!  13-JAN-1994	V1.2-3	Madison	    Update for WIN/TCP.
$!  21-SEP-1994	V1.3	Madison	    Update for V1.7.
$!  05-NOV-1994	V1.4	Madison	    Update for V2.0.
$!  02-AUG-1998	V1.4-1	Madison	    Put executables in NETLIB_DIR.
$!  22-DEC-1998	V1.5	Madison	    Add support for UCX V5.0.
$!  07-NOV-2004 V1.6    Madison     IA64 support.
$!  10-SEP-2013 V1.7    Sneddon     VAX support.
$!
$ ON CONTROL_Y THEN GOTO NETLIB_CONTROL_Y
$ ON WARNING THEN GOTO NETLIB_FAIL
$!
$ NETLIB_SAY := WRITE SYS$OUTPUT
$ tmp = F$GETSYI("ARCH_NAME")
$ IF tmp .EQS. "Alpha"
$ THEN
$   NETLIB_AXP = 1
$   NETLIB_I64 = 0
$   NETLIB_VAX = 0
$   exe_sfx = "AXP_EXE"
$ ENDIF
$ IF tmp .EQS. "IA64"
$ THEN
$   NETLIB_AXP = 0
$   NETLIB_I64 = 1
$   NETLIB_VAX = 0
$   exe_sfx = "I64_EXE"
$ ENDIF
$ IF tmp .EQS. "VAX"
$ THEN
$   NETLIB_AXP = 0
$   NETLIB_I64 = 0
$   NETLIB_VAX = 1
$   exe_sfx = "VAX_EXE"
$ ENDIF
$ DEFINE VMI$KWD SYS$DISK:[]
$!
$ TYPE SYS$INPUT:

                      NETLIB Installation Procedure

        Copyright (c) 1992,1994,1996-2005, Matthew Madison.
        Copyright (c) 2013, Endless Software Solutions.

        All rights reserved.
        Redistribution and use in source and binary forms, with or without
        modification, are permitted provided that the following conditions
        are met:

            * Redistributions of source code must retain the above
              copyright notice, this list of conditions and the following
              disclaimer.
            * Redistributions in binary form must reproduce the above
              copyright notice, this list of conditions and the following

              disclaimer in the documentation and/or other materials provided
              with the distribution.
            * Neither the name of the copyright owner nor the names of any
              other contributors may be used to endorse or promote products
              derived from this software without specific prior written
              permission.

        THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
        "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
        LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
        A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
        OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
        SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
        LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
        DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
        THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
        (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
        OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

$!
$ GOTO NETLIB_INSTALL
$!
$NETLIB_CONTROL_Y:
$ IF F$TRNLNM ("NETLIB_STUP") .NES. "" THEN CLOSE NETLIB_STUP
$ NETLIB_SAY "Installation cancelled by CTRL/Y."
$ EXIT 1
$!
$NETLIB_FAIL:
$ NETLIB_STATUS == $STATUS
$ IF F$TRNLNM ("NETLIB_STUP") .NES. "" THEN CLOSE NETLIB_STUP
$ EXIT 'NETLIB_STATUS
$!
$NETLIB_INSTALL:
$!
$ NETLIB_INSTALL_NODE = F$GETSYI ("NODENAME")
$!
$NETLIB_DIR_ASK:
$!
$ TYPE SYS$INPUT:


    Choosing the NETLIB Directory

    The NETLIB libraries may go in either your login directory
    or some other directory you specify.

$ NETLIB_DEFANS := SYS$LOGIN:
$ IF F$TRNLNM ("NETLIB_DIR","LNM$PROCESS") .NES. "" THEN -
    NETLIB_DEFANS = F$TRNLNM ("NETLIB_DIR","LNM$PROCESS")
$ CALL NETLIB_ASK NETLIB_DIR -
    "Where should the NETLIB libraries be placed" "''NETLIB_DEFANS'"
$ NETLIB_DIR = F$PARSE (NETLIB_DIR,,,"DEVICE") +-
     F$PARSE (NETLIB_DIR,,,"DIRECTORY")
$ IF F$PARSE ("''NETLIB_DIR'X.X") .EQS. "" THEN -
    CREATE/DIRECTORY 'NETLIB_DIR
$!
$ CREATE SYS$LOGIN:NETLIB_LOGIN.COM
$ OPEN/APPEND NETLIB_STUP SYS$LOGIN:NETLIB_LOGIN.COM
$ NETLIB_W = "WRITE NETLIB_STUP"
$ NETLIB_W "$! NETLIB_LOGIN.COM"
$ NETLIB_W "$! Login command procedure for NETLIB library routines."
$ NETLIB_W "$!"
$ NETLIB_W "$ DEFINE/NOLOG NETLIB_DIR ''NETLIB_DIR'"
$!
$ NETLIB_W "$!"
$ CLOSE NETLIB_STUP
$ APPEND VMI$KWD:NETLIB_LOGIN_TEMPLATE.COM SYS$LOGIN:NETLIB_LOGIN.COM
$!
$ RENAME VMI$KWD:NETLIB_SHR.'exe_sfx' .EXE
$ RENAME VMI$KWD:NETLIB_SSL.'exe_sfx' .EXE
$ COPY VMI$KWD:NETLIB_SHR.EXE 'NETLIB_DIR'
$ COPY VMI$KWD:NETLIB_SSL.EXE 'NETLIB_DIR'
$!
$NETLIB_FINISH_INSTALL:
$!
$ TYPE SYS$INPUT:

    The file SYS$LOGIN:NETLIB_LOGIN.COM includes commands that should
    be added to your login command procedure to set up NETLIB logical
    names.

    NETLIB installation complete.

$!
$ @SYS$LOGIN:NETLIB_LOGIN
$ EXIT 1
$!
$NETLIB_ASK: SUBROUTINE
$ PROMPT = "* " + P2
$ IF "''P4'" .EQS. "B" THEN PROMPT = PROMPT + "?"
$ IF "''P3'" .NES. "" THEN PROMPT = PROMPT + " [''P3']"
$ PROMPT = PROMPT + ": "
$ASK1:
$ ANSWER = ""
$ READ SYS$COMMAND/PROMPT="''PROMPT'" ANSWER
$ IF ANSWER .EQS. "" THEN ANSWER = "''P3'"
$ IF ANSWER .EQS. ""
$ THEN
$   NETLIB_SAY "%NETLIB-I-ANSREQD, A response is required."
$   GOTO ASK1
$ ENDIF
$ ANSWER = F$EDIT (ANSWER,"UPCASE")
$ IF "''P4'" .EQS. "B"
$ THEN
$   ANSWER = F$EXTRACT (0,1,ANSWER)
$   IF ANSWER .NES. "Y" .AND. ANSWER .NES. "N"
$   THEN
$   	NETLIB_SAY "%NETLIB-I-ANSYN, please answer YES or NO."
$   	GOTO ASK1
$   ENDIF
$ ENDIF
$ 'P1 == ANSWER
$ ENDSUBROUTINE
