/*[[ CMS REPLACEMENT HISTORY, Element MISC.C]]*/
/*[[ *4    16-FEB-2002 12:04:08 MADISON ""]]*/
/*[[ *3    20-JAN-2002 10:01:08 MADISON "Updates for V2.3C"]]*/
/*[[ *2    19-JAN-2001 08:35:52 MADISON "Move version-dependent source files to ETCDIR"]]*/
/*[[ *1    12-MAR-1998 16:41:00 MADISON "Initial CMS population"]]*/
/*[[ CMS REPLACEMENT HISTORY, Element MISC.C]]*/
/*
**++
**  FACILITY:	NETLIB
**
**  ABSTRACT:	Miscellaneous routines.
**
**  MODULE DESCRIPTION:
**
**  	tbs
**
**  AUTHOR: 	    M. Madison
**  	    	    COPYRIGHT © 1994, 2002 MADGOAT SOFTWARE.  ALL RIGHTS RESERVED.
**
**  CREATION DATE:  22-OCT-1994
**
**  MODIFICATION HISTORY:
**
**  	22-OCT-1994 V1.0    Madison 	Initial coding.
**  	27-NOV-1997 V1.1    Madison 	Added version info.
**      10-JAN-2002 V1.2    Madison     Don't allow extra dot after dotted-quad.
**      04-FEB-2002 V1.2-1  Madison     Last change was not correctly checking
**                                      final length.
**	07-FEB-2013 V1.3    Sneddon	Updated copyright message.
**--
*/
#include "netlib.h"
#include "etc_dir:netlib_version.h"

#ifdef __DECC
#pragma extern_model save
#pragma extern_model common_block shr
#endif
    char $$$Copyright[] = "Copyright © 1992,1994,1996-2002  MadGoat Software. All Rights Reserved.\n" \
			  "Copyright © 2013  Endless Software Solutions.";
#ifdef __DECC
#pragma extern_model restore
#endif

/*
**  Forward declarations
*/
    unsigned int netlib_strtoaddr(struct dsc$descriptor *dsc, struct INADDRDEF *a);
    unsigned int netlib_addrtostr(struct INADDRDEF *, struct dsc$descriptor *,
    	    	    	    	    unsigned short *);
    unsigned int netlib___find_symbol(char *imgnam, char *symnam, void *symptr);
    static unsigned int find_image_symbol(struct dsc$descriptor *imgdsc,
    	    	    	struct dsc$descriptor *symdsc, void *symptr);
    unsigned int netlib_hton_long(unsigned int *);
    unsigned int netlib_ntoh_long(unsigned int *);
    unsigned short netlib_hton_word(unsigned short *);
    unsigned short netlib_ntoh_short(unsigned short *);
    unsigned int netlib_version(struct dsc$descriptor *ver, unsigned short *retlen);

/*
**++
**  ROUTINE:	netlib_strtoaddr
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	tbs
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib_strtoaddr (struct dsc$descriptor *dsc, struct INADDRDEF *a) {

    unsigned short len;
    char *cp, *anchor;
    unsigned int status, parts[4], val;
    int i;

    status = lib$analyze_sdesc(dsc, &len, &cp);
    if (!OK(status)) return status;

    for (i = 0; len > 0 && i < 4; i++) {
    	val = 0;
    	while (*cp != '.' && len > 0) {
    	    if (!isdigit(*cp)) return SS$_IVADDR;
    	    val = val * 10 + (*cp - '0');
    	    cp++; len--;
    	}
    	parts[i] = val;
        if (len > 0) {
            cp++; len--;
        }
    }

    switch (i) {
    	case 0: return SS$_IVADDR;
    	case 1: val = parts[0];
    	    	break;
    	case 2: val = (parts[0] << 24) | (parts[1] & 0xffffff);
    	    	break;
    	case 3: val = (parts[0] << 24) | ((parts[1] & 0xff) << 16) |
    	    	    	    (parts[2] & 0xffff);
    	    	break;
    	case 4: if (len > 0)
                    return SS$_IVADDR;
                val = (parts[0] << 24) | ((parts[1] & 0xff) << 16) |
    	    	      ((parts[2] & 0xff) << 8) | (parts[3] & 0xff);
    	    	break;
    }

    a->inaddr_l_addr = netlib_long_swap(val);

    return SS$_NORMAL;

} /* netlib_strtoaddr */

/*
**++
**  ROUTINE:	netlib_addrtostr
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	tbs
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib_addrtostr (struct INADDRDEF *addr,
    	    	    	    	struct dsc$descriptor *dsc, unsigned short *len) {

    int argc;
    static $DESCRIPTOR(faodsc, "!UB.!UB.!UB.!UB");
    SETARGCOUNT(argc);
    return lib$sys_fao(&faodsc, (argc > 2) ? len : 0, dsc,
    	    	    (addr->inaddr_l_addr)       & 0xff,
    	    	    (addr->inaddr_l_addr >> 8)  & 0xff,
    	    	    (addr->inaddr_l_addr >> 16) & 0xff,
    	    	    (addr->inaddr_l_addr >> 24) & 0xff);

} /* netlib_addrtostr */

/*
**++
**  ROUTINE:	netlib___find_symbol
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	tbs
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib___find_symbol (char *imgnam, char *symnam, void *symptr) {

    struct dsc$descriptor imgdsc, symdsc;

    lib$establish(lib$sig_to_ret);

    INIT_SDESC(imgdsc, strlen(imgnam), imgnam);
    INIT_SDESC(symdsc, strlen(symnam), symnam);
    return find_image_symbol(&imgdsc, &symdsc, symptr);

} /* netlib___find_symbol */

/*
**++
**  ROUTINE:	find_image_symbol
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	tbs
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static unsigned int find_image_symbol (struct dsc$descriptor *imgdsc,
    	    	    	struct dsc$descriptor *symdsc, void *symptr) {

    return lib$find_image_symbol(imgdsc, symdsc, symptr);

} /* find_image_symbol */

/*
**++
**  ROUTINE:	netlib_hton_long
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	tbs
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib_hton_long (unsigned int *value) {

    return netlib_long_swap(*value);

} /* netlib_hton_long */

/*
**++
**  ROUTINE:	netlib_ntoh_long
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	tbs
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int netlib_ntoh_long (unsigned int *value) {

    return netlib_long_swap(*value);

} /* netlib_ntoh_long */

/*
**++
**  ROUTINE:	netlib_hton_word
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
**
**  RETURNS:	cond_value, wordword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	tbs
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned short netlib_hton_word (unsigned short *value) {

    return netlib_word_swap(*value);

} /* netlib_hton_word */

/*
**++
**  ROUTINE:	netlib_ntoh_word
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
**
**  RETURNS:	cond_value, wordword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	tbs
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned short netlib_ntoh_word (unsigned short *value) {

    return netlib_word_swap(*value);

} /* netlib_ntoh_word */

/*
**++
**  ROUTINE:	netlib_version
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Returns the NETLIB version string.
**
**  RETURNS:	cond_value, condition value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	NETLIB_VERSION  ver [,retlen]
**
**  ver:    	char_string, character string, write only, by descriptor
**  retlen: 	word_unsigned, word (unsigned), write only, by reference
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
unsigned int netlib_version (struct dsc$descriptor *ver, unsigned short *retlen) {

    int argc;
    unsigned int status;
    static char version_string[] = NETLIB_T_VERSION;
    static $DESCRIPTOR(faodsc, "!AD");

    SETARGCOUNT(argc);
    if (argc < 1) return SS$_INSFARG;
    if (ver == 0) return SS$_BADPARAM;

    return lib$sys_fao(&faodsc, (argc > 1 ? retlen : 0), ver,
    	    	    	sizeof(version_string)-1, version_string);

} /* netlib_version */
