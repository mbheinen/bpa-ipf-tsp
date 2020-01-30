C    @(#)ck_topol.f	20.3 2/13/96
      subroutine ck_topol (ktot, kerrsw)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		None
      include 'ipfinc/blank.inc'
      include 'ipfinc/elim2.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/norder.inc'
      include 'ipfinc/red2.inc'
c	Global variables used:
c		kolum, korder, loc, 
      include 'ipfinc/red7.inc'
c	Global variables used:
c		None

c
C     Check connection matrix symmetry

      do i = 1, ktot
        l1 = loc(i)
        do while (l1 .gt. 0)

          k = kolum(l1)
          l2 = loc(k)
          do while (l2 .gt. 0)
            if (kolum(l2) .eq. i) goto 110
            if (kolum(l2) .gt. i) goto 100
            l2 = korder(l2)
          enddo
  100     write (dbug, 10010) i, k, kolum(l2)
10010     format (' ASYMMETRIC BRANCH LOCATED (', i4, ')<-->(', i4, 
     &     ')<-->(', i4, ')')
          kerrsw = 1
  110     l1 = korder(l1)
        enddo
      enddo

      return
      end
