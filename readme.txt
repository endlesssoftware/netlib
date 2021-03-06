	NETLIB -- a library for writing TCP/IP based network applications

    Once upon a time, there were several TCP/IP packages available for VMS
systems.  Each provided a VMS-style programming interface, using the $QIO
system service, and most provided a "socket" programming library, based on
the communications model developed for BSD UNIX.

Unfortunately, there was no standard among any of the packages for the
$QIO-based programming interface (most, but not all, emulated Digital's,
at least to some degree), and the $QIO-based interface is not very easy
to use.

The socket libraries provided with these packages provided a somewhat
easier-to-use programming interface, but didn't permit VMS-style asynchronous
programming (using ASTs), and generally required at least a re-LINK, and
sometimes source modifications, when moving from one vendor's library
to another.

So, enter NETLIB.  NETLIB was originally developed to support MadGoat
Software's Message Exchange (now maintained by Endless Software Solutions)
mail package, which needed to support many TCP/IP packages doing VMS-style
asynchronous programming.  NETLIB provides a consistent, VMS-style
interface for TCP/IP-based network programs, operating with all of the
currently available TCP/IP packages available today for VMS (with one
minor exception).  In addition, NETLIB allows for flexibility in
in the use of a TCP/IP package, by selecting the vendor-dependent
library code at run-time, rather than link-time.

Now, fast-forward to the 21st century and OpenVMS is avilable on three
architectures (VAX, Alpha and Industry Standard 64).  There are only three
major TCP/IP interfaces still in development on OpenVMS.  These are:

    * HP TCP/IP Services for OpenVMS (formerly Digital UCX);
    * Process Multinet; and
    * Process TCPware.

The last two now have excellent emulation of HP TCP/IP $QIO-interface and
so support for all packages, except TCP/IP, has been dropped.  However,
NETLIB is still a relevant and useful package.  As well as being a major
component of MX and other TCP/IP based application it also now provides an
asynchronous interface to the OpenSSL package, as well as the existing API.
