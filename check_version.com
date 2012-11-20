$ v = 'f$verify(0)
$ SET := SET
$ SET SYMBOL/SCOPE=(NOLOCAL,NOGLOBAL)/VERB
$!
$ IF F$SEARCH(p1) .EQS. "" THEN GOTO Generate
$ OPEN/READ x 'p1
$readloop:
$ READ/END=endread x line
$ IF F$EXTRACT(0,1,line) .EQS. "!" THEN GOTO readloop
$ which = F$EDIT(F$ELEMENT(0,"=",line),"TRIM,UPCASE")
$ value = F$EDIT(F$ELEMENT(1,"=",line),"TRIM,UPCASE")
$ IF which .EQS. "IDENT" .AND. value .NES. """NETLIB ''p2'""" THEN GOTO Close_And_Generate
$ IF which .EQS. "GSMATCH" .AND. value .NES. "''p3'" THEN GOTO Close_And_Generate
$ GOTO readloop
$endread:
$ CLOSE x
$ EXIT 1+0*F$VERIFY(v)
$Close_And_Generate:
$ CLOSE x
$Generate:
$ DEFINE/USER SYS$INPUT _NL:
$ CREATE 'p1'
$ OPEN/APPEND x 'p1'
$ WRITE x "IDENT=""NETLIB ", p2, """"
$ WRITE X "GSMATCH=", p3
$ CLOSE x
$ EXIT 1+0*F$VERIFY(v)
