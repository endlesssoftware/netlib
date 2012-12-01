$ v = 'F$VERIFY (0)
$ SET := SET
$ SET SYMBOL/SCOPE=(NOLOCAL,NOGLOBAL)/VERB
$!
$ months = "/January/February/March/April/May/June/July/August/September/October/November/December"
$ now = F$CVTIME (, "ABSOLUTE")
$ year = F$CVTIME  (now,, "YEAR")
$ mon  = F$INTEGER (F$CVTIME (now,, "MONTH"))
$ month = F$ELEMENT (mon, "/", months)
$ amonth = F$CVTIME (now, "ABSOLUTE", "MONTH")
$ day = F$CVTIME (now,, "DAY")
$!
$ OPEN/READ in 'p1'
$ READ in line
$ CLOSE in
$ 'line'
$ major = F$ELEMENT(0, ".", ident) - "V"
$ minor = F$ELEMENT(1, ".", ident)
$ IF F$LENGTH(major) .EQ. 1 THEN major = "0" + major
$ CREATE 'p2'
$ OPEN/APPEND out 'p2'
$ WRITE out "<DEFINE_SYMBOL>(COPYYEAR\''year')"
$ WRITE out "<DEFINE_SYMBOL>(RELDATE\''day'-''amonth'-''year')"
$ WRITE out "<DEFINE_SYMBOL>(RELMONTH\''month', ''year')"
$ WRITE out "<DEFINE_SYMBOL>(PRTDATE\''day' ''month' ''year')"
$ WRITE out "<DEFINE_SYMBOL>(VER\''ident')"
$ WRITE out "<DEFINE_SYMBOL>(KITVER\''major'''minor')"
$ CLOSE out
$ EXIT 1+0*F$VERIFY(v)
