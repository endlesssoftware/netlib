#ifndef QUEDEF_H_LOADED
#define QUEDEF_H_LOADED
/*
** QUEDEF.H
**
**  #include file for queue definitions.
**
**  COPYRIGHT © 1993,1997,2004 MADGOAT SOFTWARE.  ALL RIGHTS RESERVED.
**
**  MODIFICATION HISTORY:
**
**  02-Aug-2013	    Sneddon 	Moved out of netlib.h
**  05-Aug-2013	    Sneddon 	Moved INIT_QUEUE in here.
*/
#ifdef __DECC
#include <builtins.h>
#else
#pragma builtins
#endif

#if defined(__ALPHA) || defined(__ia64__)
#pragma member_alignment save
#pragma nomember_alignment
#endif
typedef struct { void *head, *tail; } QUEUE;
typedef struct { unsigned int long1, long2; } TIME;
typedef struct dsc$descriptor DESCRIP;
typedef struct { unsigned short bufsiz, itmcod; void *bufadr, *retlen; } ITMLST;
#if defined(__ALPHA) || defined(__ia64__)
#pragma member_alignment restore
#endif

/*
** Handy macros
*/

#define INIT_QUEUE(que) {que.head = que.tail = &que;}
#if defined(__ALPHA) || defined(__ia64__)
#define queue_insert(item,pred) __PAL_INSQUEL((void *)(pred),(void *)(item))
#define queue_remove(entry,addr) (__PAL_REMQUEL((void *)(entry),(void *)(addr)) >= 0)
#else
#define queue_insert(item,pred) _INSQUE(item,pred)
#define queue_remove(entry,addr) (_REMQUE(entry,addr) != 2)
#endif

#endif /* QUEDEF_H_LOADED */
