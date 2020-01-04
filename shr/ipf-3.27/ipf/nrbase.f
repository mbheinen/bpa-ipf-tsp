C    @(#)nrbase.f	20.3 2/13/96
      subroutine nrbase   ! update QGEN and DC lines to
C                             reflect a base solution.

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		pnetu(r*8), qnetu(r*8)
      include 'ipfinc/beta.inc'      
c	Global variables used:
c		pk(r*8), qk(r*8), kt
      include 'ipfinc/blank.inc'
c	Global variables used:
c		lskp, ntot
c
      real temp_pk, temp_qk      !subroutine nrpqv expecting single precision.

      lskp = 1
      do 2410 kt = 1,ntot
c
c	nrpqv is expecting single precision arguments.
c	double precision: pk, qk, and vk(read only in subroutine).
c
         temp_pk = sngl(pk)
         temp_qk = sngl(qk)
         call nrpqv (kt,temp_pk,dpk,temp_qk,dqk,sngl(vk))
         pk = temp_pk
         qk = temp_qk
         pnetu(kt) = pk
         qnetu(kt) = qk
 2410 continue

      call nrdc
      call nrdcup
      call dcfinl
      return
      end
