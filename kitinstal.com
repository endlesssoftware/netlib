$! [NETLIB]KITINSTAL.COM
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
$!  21-SEP-1994	V1.1	Madison	    Moved AXP objects to separate save set.
$!  23-APR-1998	V1.2	Madison	    Check for something actually installed.
$!  19-NOV-2012 V1.3	Sneddon	    Reinstate VAX support.
$!  10-SEP-2013 V1.4    Sneddon     Add SSL.
$!
$ ON CONTROL_Y THEN GOTO NETLIB_CONTROL_Y
$ ON WARNING THEN GOTO NETLIB_FAIL
$!
$ IF P1 .EQS. "VMI$_INSTALL" THEN GOTO NETLIB_INSTALL
$ EXIT VMI$_UNSUPPORTED
$!
$NETLIB_CONTROL_Y:
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
$!
$ NETLIB_SAY := WRITE SYS$OUTPUT
$!
$ tmp = F$GETSYI ("HW_MODEL")
$ IF tmp .GT. 0 .AND. tmp .LT. 1024
$ THEN
$   NETLIB_AXP = 0
$   NETLIB_I64 = 0
$   NETLIB_VAX = 1
$ ELSE
$   tmp = F$GETSYI ("ARCH_NAME")
$   IF tmp .EQS. "Alpha"
$   THEN
$     NETLIB_AXP = 1
$     NETLIB_I64 = 0
$     NETLIB_VAX = 0
$   ENDIF
$!
$   IF tmp .EQS. "IA64"
$   THEN
$     NETLIB_AXP = 0
$     NETLIB_I64 = 1
$     NETLIB_VAX = 0
$   ENDIF
$ ENDIF
$!
$!
$ VMI$CALLBACK CHECK_NET_UTILIZATION NETLIB_ENOUGHDISK 3000 30 3000
$ IF .NOT. NETLIB_ENOUGHDISK THEN EXIT VMI$_FAILURE
$!
$ VMI$CALLBACK SET SAFETY CONDITIONAL 5000
$!
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
$!
$ VMI$CALLBACK SET PURGE ASK
$ @VMI$KWD:NETLIB_INSTALL 'P1 'P2 'P3 'P4 'P5 'P6 'P7 'P8
$ NETLIB_STATUS == $STATUS
$ IF NETLIB_STATUS .EQ. VMI$_SUCCESS .AND. "''NETLIB_INSTALLED'" .EQS. "YES"
$ THEN
$   TYPE SYS$INPUT

    All NETLIB images have been linked.  Remember to edit your system
    startup command procedure to add the following commmand:

        $ @SYS$STARTUP:NETLIB_STARTUP

    to ensure that the NETLIB logical names are defined and the
    NETLIB shareable images are installed at system startup.

$!
$ ENDIF
$!
$ EXIT 'NETLIB_STATUS
