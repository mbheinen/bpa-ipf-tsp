/* @(#)types.h	6.1	(ULTRIX)	11/19/91 */

/************************************************************************
 *									*
 *			Copyright (c) 1984 - 1989 by			*
 *		Digital Equipment Corporation, Maynard, MA		*
 *			All rights reserved.				*
 *									*
 *   This software is furnished under a license and may be used and	*
 *   copied  only  in accordance with the terms of such license and	*
 *   with the  inclusion  of  the  above  copyright  notice.   This	*
 *   software  or  any  other copies thereof may not be provided or	*
 *   otherwise made available to any other person.  No title to and	*
 *   ownership of the software is hereby transferred.			*
 *									*
 *   This software is  derived  from  software  received  from  the	*
 *   University    of   California,   Berkeley,   and   from   Bell	*
 *   Laboratories.  Use, duplication, or disclosure is  subject  to	*
 *   restrictions  under  license  agreements  with  University  of	*
 *   California and with AT&T.						*
 *									*
 *   The information in this software is subject to change  without	*
 *   notice  and should not be construed as a commitment by Digital	*
 *   Equipment Corporation.						*
 *									*
 *   Digital assumes no responsibility for the use  or  reliability	*
 *   of its software on equipment which is not supplied by Digital.	*
 *									*
 ************************************************************************/

/* ------------------------------------------------------------------------
 * Modification History: /sys/h/types.h
 *
 * 20-Dec-1989  Larry Scott
 *	added #ifdef's for compliance
 *
 * 16-Jun-1989	Jon Reeves
 *	size_t must be unsigned, per ANSI and X/Open
 *
 * 05-Jun-1989	Jon Reeves
 *	Change latch name for size_t; it's in lots of headers.
 *
 * 12-May-1989	Todd M. Katz		TMK0001
 *	Added volatile type definitions: vu_long, vu_short, vu_char,
 *	v_long, v_short, and v_char.
 *
 * 08-May-1989 -- Ken Lesniak
 *	Conditionalize items also defined in time.h.
 *
 *  1-Feb-89 -- jmartin
 *	typedef s_char
 *
 * 15-Jan-88	lp
 *	Merge of final 43BSD changes.
 * 
 * 31-August-1987 -- Mark Parenti
 *	Add definitions needed for POSIX compliance
 *	
 * 27-April-1987 -- Larry Cohen
 *	Modify the typedef "fd_set" to accomodate 64 file descriptors.
 *
 * 	David L Ballenger, 8-Mar-1985
 * 0002	Add types for System V compatibility.
 *
 * 23 Oct 84 -- jrs
 *	Add ifdef so we can be nested without problem
 *	Derived from 4.2BSD, labeled:
 *		types.h 6.2	84/06/09
 *
 * -----------------------------------------------------------------------
 */

#ifdef VMS
#include <types.h>
#define bool_t  int
#define enum_t  int
#define FALSE   (0)
#define TRUE    (1)
#ifndef NULL
#define NULL 0
#endif
#define __dontcare__    -1
#endif

#ifndef _TYPES_
#define	_TYPES_

#ifndef VMS
#ifdef KERNEL
#include "../h/ansi_compat.h"
#else
#include <ansi_compat.h>
#endif
#endif

#if !defined(_POSIX_SOURCE)
/*
 * Basic system types and major/minor device constructing/busting macros.
 */

/* major part of a device */
#define	major(x)	((int)(((unsigned)(x)>>8)&0377))

/* minor part of a device */
#define	minor(x)	((int)((x)&0377))

/* make a device number */
#define	makedev(x,y)	((dev_t)(((x)<<8) | (y)))

#ifndef VMS
typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned int	uint;		/* sys V compatibility */
typedef	unsigned long	u_long;
typedef	unsigned short	ushort;		/* sys III compat */

typedef	volatile char		v_char;
typedef	volatile short		v_short;
typedef	volatile long		v_long;
typedef	volatile unsigned char	vu_char;
typedef	volatile unsigned short	vu_short;
typedef	volatile unsigned long	vu_long;
#endif

typedef
#ifdef __mips
	signed
#endif /* __mips */
		char	s_char;

#ifdef __vax
typedef	struct	_physadr { int r[1]; } *physadr;
typedef	struct	label_t	{
	int	val[14];
} label_t;
#endif /* __vax */

#ifdef __mips
typedef	struct	_physadr { int r[1]; } *physadr;
/*
 * WARNING:
 * this must match the definition of kernel jmpbuf's in machine/pcb.h
 */
typedef	struct	label_t	{
	int	val[12];
} label_t;
#endif /* __mips */

#ifndef VMS

typedef	struct	_quad { long val[2]; } quad;
typedef	long	daddr_t;
#ifdef NOALPHA
typedef	char *	caddr_t;
#endif
typedef u_long	gno_t;
typedef short	cnt_t;			/* sys V compatibility */
typedef	long	swblk_t;
typedef long	paddr_t;		/* sys V compatibility */
typedef	long	audit_ID_t;
#endif /* VMS */
#endif /* !defined(_POSIX_SOURCE) */

#ifndef VMS
typedef	short	dev_t;
typedef short	gid_t;			/* POSIX compliance    */
typedef	unsigned long	ino_t;
typedef unsigned short	mode_t;		/* POSIX compliance    */
typedef short	nlink_t;		/* POSIX compliance    */
typedef	int	off_t;
#ifndef _PID_T_
#define _PID_T_
typedef int	pid_t;			/* POSIX compliance    */
#endif /* _PID_T_ */
typedef short	uid_t;			/* POSIX compliance    */
#endif /* VMS */

#ifndef VMS

#ifndef _TIME_T_
#define _TIME_T_
typedef int	time_t;
#endif /* _TIME_T_ */

#if !defined(_POSIX_SOURCE) || defined(_XOPEN_SOURCE)
#ifndef	_SIZE_T_
#define	_SIZE_T_
typedef	unsigned int	size_t;
#endif	/* _SIZE_T_ */
#ifndef _CLOCK_T_
#define _CLOCK_T_
typedef int	clock_t;			/* POSIX compliance */
#endif /* _CLOCK_T_ */
typedef long	key_t;			/* sys V compatibility */
#endif /* !defined(_POSIX_SOURCE) || defined(_XOPEN_SOURCE) */
#endif /* VMS */
#if !defined(_POSIX_SOURCE)

/*
 * The maximum number of file descriptors is now a configurable option
 * (max_nofile variable in /sys/conf/{mips|vax}/param.c).
 * The getdtablesize(2) system call should be used to obtain the
 * current limit. The value returned by getdtablesize() must be greater
 * than 64, and less than or equal to MAX_NOFILE in types.h . The
 * MAX_NOFILE define is needed for backward compatability with broken
 * programs that need a static sized array for selecting. These programs
 * should be modified to use the getdtablesize() interface for sizing.
 */
#define MAX_NOFILE	4096	/* This should not exist ! */
#define	NBBY	8		/* number of bits in a byte */
/*
 * Select uses bit masks of file descriptors in longs.
 * These macros manipulate such bit fields (the filesystem macros use chars).
 * FD_SETSIZE may be defined by the user, but the default here
 * should be >= NOFILE (param.h).
 */
#ifndef	FD_SETSIZE
#define	FD_SETSIZE	MAX_NOFILE
#endif	/* FD_SETSIZE */

/* How many things we'll allow select to use. 0 if unlimited */
#define MAXSELFD	MAX_NOFILE
typedef long	fd_mask;
#define NFDBITS	(sizeof(fd_mask) * NBBY)	/* bits per mask (power of 2!)*/
#define NFDSHIFT 5				/* Shift based on above */
#ifndef howmany
#define	howmany(x, y)	(((x)+((y)-1))/(y))
#endif /* howmany */

typedef	struct fd_set {
	fd_mask	fds_bits[howmany(FD_SETSIZE, NFDBITS)];
} fd_set;

#define	FD_SET(n, p)	((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))
#define	FD_CLR(n, p)	((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#define	FD_ISSET(n, p)	((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))
#define FD_ZERO(p)	memset((char *)(p), 0, sizeof(*(p)))
#endif /* !defined(_POSIX_SOURCE) */

#endif /* _TYPES_ */
