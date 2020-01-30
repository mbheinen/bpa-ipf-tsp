C    @(#)chkchdup.f	20.3 2/13/96
      subroutine chkchdup
 
C     This subroutine checks chgcrd() for duplicates.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/changr.inc'
      include 'ipfinc/prt.inc'
 
      external kmpchg, swpchg

c     Flag and remove switched reactance buses..                *
 
      j = 1
      k = 2
 
      do while (k .le. numchg)
         if ( kmpchg(j,k) .lt. 0 ) then
            j = j + 1
            if (j .lt. k) call swpchg(j,k)
            k = k + 1
         else
            write (errbuf(1),150)
  150       format ('Duplicate chgcrd records ...')
            write (errbuf(2),160) chgcrd(k)(1:40)
  160       format (' Record ', a)
            call prterx ('W',2)
            k = k + 1
         endif
 
      enddo
      numchg = j
      return
      end
