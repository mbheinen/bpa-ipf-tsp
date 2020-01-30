#define NBBY    8               /* number of bits in a byte */
/*
 * Select uses bit masks of file descriptors in longs.
 * These macros manipulate such bit fields (the filesystem macros use chars).
 * FD_SETSIZE may be defined by the user, but the default here is equal
 * to OPEN_MAX_SYSTEM in param.h since that defines the absolute maximum
 * number of file descriptors that a process can open.
 */
#define MAX_NOFILE      4096
#ifndef FD_SETSIZE
#define FD_SETSIZE      MAX_NOFILE
#endif

/* How many things we'll allow select to use. 0 if unlimited */
#define MAXSELFD        MAX_NOFILE
typedef int     fd_mask;
#define NFDBITS (sizeof(fd_mask) * NBBY)        /* bits per mask */

#ifndef howmany
#define howmany(x, y)   (((x)+((y)-1))/(y))
#endif

typedef struct fd_set {
        fd_mask fds_bits[howmany(FD_SETSIZE, NFDBITS)];
} fd_set;

#define FD_SET(n, p)    ((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))
#define FD_CLR(n, p)    ((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#define FD_ISSET(n, p)  ((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))
#define FD_ZERO(p)      memset((char *)(p), 0, sizeof(*(p)))

