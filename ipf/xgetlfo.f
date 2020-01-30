C    @(#)xgetlfo.f	20.3 2/13/96
      subroutine xgetlfo (ptr, mtrpnt, pin, qin )
      integer ptr
 
C     This subroutine computes the line flow of data in the alternate  *
C     base case with respect to the specified metering point.          *
C                                                                      *
C     Input parameters:                                                *
C                                                                      *
C     PTR    - index to KX, KY
C     MTRPNT - 0 : Determine metering point                            *
C              1 : Compute flows at bus1 end                           *
C              2 : Compute flowa at bus2 end                           *
C                                                                      *
C     Output Parameters:                                               *
C                                                                      *
C     PIN - Flow (MW) in line (form bus1 to bus 2) computed at         *
C           metering point.                                            *
C     QIN - Flow (MVAR) in line (form bus1 to bus 2) computed at       *
C           metering point.                                            *
C                                                                      *
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/alt_case.inc'
      include 'ipfinc/prt.inc'
 
      dimension locsec(10)
      character id * 1, nxid * 1, lown * 4, flag * 1, xbuf * 120
      complex y(2,2), a(2), v(2), s(2)
      integer error, p, qptr
 
      ltype = obrtype(ptr)
      k1 = okx(ptr)
      k2 = oky(ptr)
      id = obrid(ptr)
      loc = mtrpnt
 
      if (loc .eq. 0) then
C                                                           
C        Metering point not set.
c        First pass: determine metering point.  
C                                                           
         p = ptr
         jt = 0
         loc = 0
 
         do while (p .gt. 0 .and. (oky(p) .eq. k2))
            qptr = obrnch_ptr(p)
            nbr = iabs (qptr)
            intovr = okbrnch(15,nbr)
            if (intovr .gt. 0 .and. qptr .lt. 0) invtovr = 3 - intovr
            if (obrtype(p) .eq. 4 .or. obrtype(p) .eq. 9) then
            else
               if (obrtype(p) .eq. 7) then
 
C                 If d-c line, set metering point at receiving end
 
                  if (intovr .eq. 0) then
                     pdc = obrnch(8,nbr)
                     if ((pdc .lt. 0 .and. qptr .gt. 0) .or.
     &                   (pdc .gt. 0 .and. qptr .lt. 0)) then
                        intovr = 1
                     else
                        intovr = 2
                     endif
                     if (qptr .gt. 0) then
                        okbrnch(15,nbr) = intovr
                     else
                        okbrnch(15,nbr) = 3 - intovr
                     endif
                  endif
               else
                  nxid = obrid(p)
                  if (id .ne. nxid) go to 130
                  loc = 0
               endif
               if (obrtype(p) .ne. 1) then
                  if (intovr .ne. 0) then
                     loc = intovr + 100
                  else
                     call getchr(3,lown,okbrnch(3,nbr))
                     if (oldowner(k1) .eq. oldowner(k2)) then
                        loc = 1
                        if (oinp2alf(k1) .gt. oinp2alf(k2)) loc = 2
                     else
                        if (lown .eq. oldowner(k1) .and.
     &                      lown .ne. oldowner(k2)) then
                           loc = 2 + 10
                        else if (lown .eq. oldowner(k2) .and.
     &                           lown .ne. oldowner(k1)) then
                           loc = 1 + 10
                        else
                           loc = 1
                           if (oinp2alf(k1) .gt. oinp2alf(k2)) loc = 2
                        endif
                     endif
                  endif
               endif
            endif
            jt = jt + 1 
            locsec(jt) = loc
            p = obrnch_nxt(p)
         enddo
 
C        Evaluate metering point by weighed criteria
 
  130    loc1 = 0
         loc2 = 0
 
         do 140 j = 1, jt
            if (mod(locsec(j),10) .eq. 1) loc1 = loc1 + locsec(j)
            if (mod(locsec(j),10) .eq. 2) loc2 = loc2 + locsec(j) - 1
  140    continue
 
         ksw = 1
         if (loc1 .gt. loc2) then
            loc = 1
         else if (loc2 .gt. loc1) then
            loc = 2
         else if (oinp2alf(k1) .lt. oinp2alf(k2)) then
            loc = 1
         else
            loc = 2
         endif
 
         if (loc1 .ne. 0 .and. loc2 .ne. 0) go to 150
         ksw = 2
c
c        If no sections are present, no ambiguity is possible.
c
         if (jt .eq. 1) go to 220

  150    if (ksw .eq. 1) then
            write (errbuf(1),160)
  160       format (
     1         '0 Inconsistent metering point on Area Interchange ',
     2         'tie line. ("*" indicates selected metering point.)')
            write (errbuf(2),170)
  170       format (2x, 'Metering point     -------- Line -----------')
         endif
c
c        Second pass: Stamp metering point into branch data
c
         p = ptr
         i = 2
         do while (p .gt. 0 .and. (oky(p) .eq. k2))
            qptr = obrnch_ptr(p)
            nbr = iabs(qptr)
            if (obrtype(p) .eq. 4) then
            else if (obrtype(p) .eq. 1) then
               if (qptr .gt. 0) then
                  okbrnch(15,nbr) = loc
               else
                  okbrnch(15,nbr) = 3 - loc
               endif
            else
               i = i + 1
               errbuf(i) = ' '
               call obcdbrn(p,xbuf)
               loc1 = mod (locsec(p),10)
               flag = ' '
               if (loc .eq. loc1) flag = '*'
               if (ksw .eq. 1) write (errbuf(i),180) loc1, flag,
     1            xbuf(1:80)
  180          format (10x, i1, a1, 9x, '(', a80, ')')
               if (qptr .gt. 0) then
                  okbrnch(15,nbr) = loc
               else
                  okbrnch(15,nbr) = 3 - loc
               endif
            endif
            p = obrnch_nxt(p)
         enddo
         if (ksw .eq. 1) call prterx ('W',i)
  220    continue

      endif
 
      kt = oinp2opt(k1)
      mt = oinp2opt(k2)
      if (ltype .eq. 2) then
C                                                      
C        Compute multi-terminal d-c line flows         
C                                                      
         k1x = min0 (k1, k2 )
         k2x = max0 (k1, k2 )
 
         do 222 jdc = 1, omtdcln
            l1x = min0 (okdcmtl(1, jdc ), okdcmtl(2, jdc ) )
            l2x = max0 (okdcmtl(1, jdc ), okdcmtl(2, jdc ) )
            if ( k1x .eq. l1x .and. k2x .eq. l2x ) go to 224
  222    continue
         call erexit
 
  224    if (k1 .ne. okdcmtl(1, jdc)) then
            l1 = okdcmtl(9, jdc)
            l2 = okdcmtl(8, jdc)
         else
            l1 = okdcmtl(8, jdc)
            l2 = okdcmtl(9, jdc)
         end if
         v1 = odcmtbs(20,l1)
         v2 = odcmtbs(20,l2)
         ain = (v1 - v2) / odcmtln(4, jdc)
         if (loc .eq. 1) then
            pin = v1 * ain
            qin = 0.0
         else
            pin = v2 * ain
            qin = 0.0
         endif
 
      else if (ltype .eq. 7) then
C                                                      
C        Compute 2-terminal d-c quantities             
C                                                      
         k1x = min0 (k1, k2 )
         k2x = max0 (k1, k2 )
 
         do 226 jdc = 1, okdtot
            l1x = min0 (okdc2t(1, jdc ), okdc2t(3, jdc ) )
            l2x = max0 (okdc2t(1, jdc ), okdc2t(3, jdc ) )
            if ( k1x .eq. l1x .and. k2x .eq. l2x ) go to 228
  226    continue
 
         call erexit
 
  228    if (okdc2t(1, jdc) .eq. k1) then
            v1 = odc2t(40, jdc)
            v2 = odc2t(41, jdc)
            ain = 0.001 * odc2t(39, jdc)
         else
            v1 = odc2t(41, jdc)
            v2 = odc2t(40, jdc)
            ain = -0.001 * odc2t(39, jdc)
         endif
         if (loc .eq. 1) then
            pin = v1 * ain
            qin = 0.0
         else
            pin = v2 * ain
            qin = 0.0
         endif
      else
         call opieqiv (ptr, y, error)
         v(1) = cmplx(olde(kt), oldf(kt))
         v(2) = cmplx(olde(mt), oldf(mt))
         do 240 i = 1, 2
            a(i) = cmplx(0.0, 0.0)
            do 230 j = 1, 2
               a(i) = a(i) + y(i,j) * v(j)
  230       continue
            s(i) = v(i) * conjg(a(i))
  240    continue
         if (loc .eq. 1) then
            pin = real(s(1)) * bmva
            qin = aimag(s(1)) * bmva
         else
            pin = -real(s(2)) * bmva
            qin = -aimag(s(2)) * bmva
         endif
      endif
      return
      end
