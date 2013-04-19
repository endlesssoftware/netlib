/*
**++
**  FACILITY:   NETLIB
**
**  ABSTRACT:   Load SSL routines and libraries.
**
**  MODULE DESCRIPTION
**
**      The OpenSSL support for NETLIB is dynamically loaded as the RTLs
**  are not compatible.  So, there is a need to re-link everytime the
**  OpenSSL software is upgrade (or changed in some way)
**
**	The code in this module loads the OpenSSL routines we need
**  dynamically as well as initializing the OpenSSL environment.
**
**  AUTHOR:         Tim Sneddon
**
**  Copyright (c) 2013, Endless Software Solutions.
**
**  All rights reserved.
**
**  Redistribution and use in source and binary forms, with or without
**  modification, are permitted provided that the following conditions
**  are met:
**
**      * Redistributions of source code must retain the above
**        copyright notice, this list of conditions and the following
**        disclaimer
**      * Redistributions in binary form must reproduce the above
**        copyright notice, this list of conditions and the following
**        disclaimer in the documentation and/or other materials provided
**        with the distribution.
**      * Neither the name of the copyright owner nor the names of any
**        other contributors may be used to endorse or promote products
**        derived from this software without specific prior written
**        permission
**
**  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
**  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
**  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
**  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
**  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
**  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
**  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USER
**  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
**  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
**  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
**  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
**  CREATION DATE:  07-FEB-2013
**
**  MODIFICATION HISTORY:
**
**      07-FEB-2013 V1.0    Sneddon     Initial coding.
**--
*/
#include <descrip.h>
#include <lib$routines.h>

/*
**  Forward declarations
*/

    static void netlib___ssl_initialize(void);
    static void netlib___ssl_fis(char *symbol, int len, void (*value)(void));

/*
**  Global storage
*/

    void (*BIO_free)(void);
    void (*BIO_new)(void);
    void (*BIO_s_mem)(void);
    void (*SSLv2_method)(void);
    void (*SSLv23_method)(void);
    void (*SSLv3_method)(void);
    void (*SSL_CTX_check_private_key)(void);
    void (*SSL_CTX_free)(void);
    void (*SSL_CTX_new)(void);
    void (*SSL_CTX_use_PrivateKey_file)(void);
    void (*SSL_CTX_use_certificate_file)(void);
    void (*SSL_accept)(void);
    void (*SSL_connect)(void);
    void (*SSL_free)(void);
    void (*SSL_library_init)(void);
    void (*SSL_new)(void);
    void (*SSL_set_bio)(void);
    void (*SSL_shutdown)(void);
    void (*TLSv1_method)(void);

/*
**  Some helpful macro
*/

#define ssl_fis(name) netlib___ssl_fis(#name, sizeof(#name), name)

/*
**  Setup LIB$INTIAILIZE..
*/
#pragma nostandard
#pragma extern_model save
#ifdef __VAX
#  pragma extern_model strict_refdef "LIB$INITIALIZE" nowrt,long,nopic
#else
#  pragma extern_model strict_refdef "LIB$INITIALIZE" nowrt,long
#  if __INITIAL_POINTER_SIZE
#    pragma __pointer_size __save
#    pragma __pointer_size 32
#  else
#    pragma __required_pointer_size __save
#    pragma __required_pointer_size 32
#  endif
#endif
    void (* const iniarray[])(void) = { netlib___ssl_initialize, };
#ifndef __VAX
#  if __INITIAL_POINTER_SIZE
#    pragma __pointer_size __restore
#  else
#    pragma __required_pointer_size __restore
#  endif
#endif

/*
** Force a reference to LIB$INITIALIZE to ensure it exists
** in the image.
*/

#pragma extern_model strict_refdef
    int LIB$INITIALIZE(void);
    int lib_init_ref = (int) LIB$INITIALIZE;
#pragma extern_model restore
#pragma standard

static void netlib___ssl_initialize(void) {

    ssl_fis(BIO_free);
    ssl_fis(BIO_new);
    ssl_fis(BIO_s_mem);
    ssl_fis(SSLv2_method);
    ssl_fis(SSLv23_method);
    ssl_fis(SSLv3_method);
    ssl_fis(SSL_CTX_check_private_key);
    ssl_fis(SSL_CTX_free);
    ssl_fis(SSL_CTX_new);
    ssl_fis(SSL_CTX_use_PrivateKey_file);
    ssl_fis(SSL_CTX_use_certificate_file);
    ssl_fis(SSL_accept);
    ssl_fis(SSL_connect);
    ssl_fis(SSL_free);
    ssl_fis(SSL_library_init);
    ssl_fis(SSL_new);
    ssl_fis(SSL_set_bio);
    ssl_fis(SSL_shutdown);
    ssl_fis(TLSv1_method);
    SSL_library_init();
}

static void netlib___ssl_fis(char *symbol, int len, void (*value)(void)) {
    static $DESCRIPTOR(libssl, "SSL$LIBSSL_SHR32");

    struct dsc$descriptor symbol_d;

    symbol_d.dsc$w_length = len;
    symbol_d.dsc$b_dtype = DSC$K_DTYPE_T;
    symbol_d.dsc$b_class = DSC$K_CLASS_S;
    symbol_d.dsc$a_pointer = symbol;
lib$put_output(&symbol_d);
    lib$stop(lib$find_image_symbol(&libssl, &symbol_d, value));
}
