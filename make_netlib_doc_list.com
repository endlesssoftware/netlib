$ IF p1 .EQS. ""
$ THEN doc_dir = "MG_KIT:[NETLIB]"
$ ELSE doc_dir = p1
$ ENDIF
$ IF p2 .EQS. ""
$ THEN outfile = "MG_KIT:[]NETLIB_DOC_LIST.DAT"
$ ELSE outfile = p2
$ ENDIF
$ create 'outfile
$ close/nolog netlib_doc_list
$ open/append netlib_doc_list 'outfile
$ write netlib_doc_list "!"
$ write netlib_doc_list "! NETLIB documentation files."
$ write netlib_doc_list "!"
$ call make_list "''DOC_DIR'NETLIB*.PS"
$ call make_list "''DOC_DIR'NETLIB*.PDF"
$ call make_list "''DOC_DIR'NETLIB*.TXT"
$ call make_list "''DOC_DIR'NETLIB*.HTML"
$ close/nolog netlib_doc_list
$ write sys$output "''outfile' created"
$ exit
$ MAKE_LIST: SUBROUTINE
$  _Loop:
$	file = f$search(p1)
$	if file.eqs."" then exit
$	name = f$parse(file,"","","NAME")+f$parse(file,"","","TYPE")
$	write netlib_doc_list f$fao("NETLIB_TMP !32AS NETLIB_DOC_DIR:", name)
$	goto _loop
$ ENDSUBROUTINE
