C    @(#)cktopol2.f	20.3 2/13/96
      subroutine cktopol2 (kerrsw)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		None
      include 'ipfinc/blank.inc'
      include 'ipfinc/elim.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/bus.inc'
c	Global variables used:
c		bus, base
      include 'ipfinc/renum.inc'

C     Check connection matrix symmetry

      do i = 1, ntot
        l1 = loc1(i)
        do while (l1 .gt. 0)

          k = kolum1(l1)
          l2 = loc2(k)
          do while (l2 .gt. 0)
            if (kolum2(l2) .eq. i) goto 110
            if (kolum2(l2) .gt. i) goto 100
            l2 = kordr2(l2)
          enddo
  100     write (errbuf(1), 10010) i, bus(i), base(i), k, bus(k),
     &      base(k)
10010     format('0 non-symmetric branch located: ', i4, ' (', a8, f6.1,
     &  ') ---> ', i4, ' (', a8, f6.1, ')')
          call prterx ('W',1)
          kerrsw = 1
  110     l1 = kordr1(l1)
        enddo
      enddo

      do i = 1, ntot
        l2 = loc2(i)
        do while (l2 .gt. 0)

          k = kolum2(l2)
          l1 = loc1(k)
          do while (l1 .gt. 0)
            if (kolum1(l1) .eq. i) goto 130
            if (kolum1(l1) .gt. i) goto 120
            l1 = kordr1(l1)
          enddo
  120     write (errbuf(1), 10020) i, bus(i), base(i), k, bus(k),
     &      base(k)
10020     format('0 non-symmetric branch located: ',i4,' (', a8, f6.1,
     &  ') <--- ', i4, ' (bus ', a8, f6.1, ')')
          call prterx ('W',1)
          kerrsw = 1
  130     l2 = kordr2(l2)
        enddo
      enddo

      return
      end
