C    @(#)chk_cbsdta.f	20.2 7/18/96
      integer function chk_cbsdta(ptr, field, count, out_buffer)
      integer field, ptr, count
      character out_buffer(10)*120

C     This subroutine checks BCTBL(*,PTR) extensively for data errors.

      include 'ipfinc/parametr.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/lfiles.inc'

      chk_cbsdta = 0
      if (ptr .eq. 0) go to 220

      if ((field .eq. 0 .or. field .eq. 21) .and.
     &    (bctbl(2,ptr) .lt. 0.0 .or. bctbl(2,ptr) .gt. 3000.0)) then
        chk_cbsdta = 1
        count = min0 (count + 1, 10)
        write (out_buffer(count), 10010) bctbl(2,ptr)
10010   format (' Pload (', f7.1, ') < 0 or > 3000 ')
      endif
      if ((field .eq. 0 .or. field .eq. 26) .and.
     &    (bctbl(3,ptr) .lt. 0.0 .or. bctbl(3,ptr) .gt. 3000.0)) then
        chk_cbsdta = 1
        count = min0 (count + 1, 10)
        write (out_buffer(count), 10020) bctbl(3,ptr)
10020   format (' Qload (', f7.1, ') < 0 or > 3000 ')
      endif
      if ((field .eq. 0 .or. field .eq. 31) .and.
     &    (bctbl(4,ptr) .ne. 0.0)) then
        chk_cbsdta = 1
        count = min0 (count + 1, 10)
        write (out_buffer(count), 10030) bctbl(4,ptr)
10030   format (' Gshunt (', f7.1, ') >< 0 ')
      endif
      if ((field .eq. 0 .or. field .eq. 35) .and.
     &    (bctbl(5,ptr) .lt. -3000.0 .or. bctbl(5,ptr) .gt. 3000.0)) 
     &  then
        chk_cbsdta = 1
        count = min0 (count + 1, 10)
        write (out_buffer(count), 10040) bctbl(5,ptr)
10040   format (' Bshunt (', f7.1, ') < -3000 or > 3000 ')
      endif
      if ((field .eq. 0 .or. field .eq. 43) .and.
     &    (bctbl(6,ptr) .lt. 0.0 .or. bctbl(6,ptr) .gt. 3000.0)) then
        chk_cbsdta = 1
        count = min0 (count + 1, 10)
        write (out_buffer(count), 10050) bctbl(6,ptr)
10050   format (' Pgen (', f7.1, ') < 0 or > 3000 ')
      endif
      if ((field .eq. 0 .or. field .eq. 43 .or. field .eq. 48 .or.
     &     field .eq. 53) .and.
     &    (bctbl(6,ptr) .gt. 0.0 .and. 
     &    (bctbl(11,ptr) .gt. bctbl(6,ptr) .or. 
     &     bctbl(12,ptr) .lt. -bctbl(6,ptr)))) then
        chk_cbsdta = 1
        count = min0 (count + 1, 10)
        write (out_buffer(count), 10060) bctbl(6,ptr), bctbl(11,ptr), 
     &    bctbl(12,ptr)
10060   format (' Pgen (', f7.1, ') < Qmax (', f7.1, 
     &    ') or -Pgen > Qmin (', f7.1, ')')
      endif
      if ((field .eq. 0 .or. field .eq. 48 .or. field .eq. 53) .and.
     &    (bctbl(11,ptr) .lt. bctbl(12,ptr))) then
        chk_cbsdta = 1
        count = min0 (count + 1, 10)
        write (out_buffer(count), 10070) bctbl(11,ptr), bctbl(12,ptr)
10070   format (' Qmax (', f7.1, ') < Qmin (', f7.1, ')')
      endif

  220 return
      end
