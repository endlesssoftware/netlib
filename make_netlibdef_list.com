$ IF p1 .EQS. ""
$ THEN src_dir = "MG_SRC:[NETLIB]"
$ ELSE src_dir = p1
$ ENDIF
$ IF p2 .EQS. ""
$ THEN outfile = "MG_KIT:[]NETLIBDEF_LIST.DAT"
$ ELSE outfile = p2
$ ENDIF
$ create 'outfile
$ close/nolog netlibdef_list
$ open/append netlibdef_list 'outfile
$ write netlibdef_list "!"
$ write netlibdef_list "! NETLIB programming support files."
$ write netlibdef_list "!"
$ call make_list "''SRC_DIR'NETLIBDEF.*"
$ close/nolog netlibdef_list
$ write sys$output "''outfile' created"
$ exit
$ MAKE_LIST: SUBROUTINE
$  _Loop:
$	file = f$search(p1)
$	if file.eqs."" then exit
$	name = f$parse(file,"","","NAME")+f$parse(file,"","","TYPE")
$	write netlibdef_list f$fao("NETLIB_TMP !32AS NETLIB_DIR:", name)
$	goto _loop
$ ENDSUBROUTINE
