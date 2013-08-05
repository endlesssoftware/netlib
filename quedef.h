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
**  05-Aug-2013	    Sneddon 	Moved INIT_QUEUE in here.  Add queue_empty
**				and queue_single.
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
#if defined(__ALPHA) || defined(__ia64__)
#pragma member_alignment restore
#endif

/*
** Handy macros
*/

#define INIT_QUEUE(que) do {que.head = que.tail = &que;} while (0)
#if defined(__ALPHA) || defined(__ia64__)
#define queue_insert(item,pred) __PAL_INSQUEL((void *)(pred),(void *)(item))
#define queue_remove(entry,addr) (__PAL_REMQUEL((void *)(entry),(void *)(addr)) >= 0)
#else
#define queue_insert(item,pred) _INSQUE(item,pred)
#define queue_remove(entry,addr) (_REMQUE(entry,addr) != 2)
#endif
#define queue_empty(que) (((que).head == &(que)) && ((que).tail == &(que)))
#define queue_single(que) (((que).tail == (que).head) && ((que).head != &(que)))

#endif /* QUEDEF_H_LOADED */
