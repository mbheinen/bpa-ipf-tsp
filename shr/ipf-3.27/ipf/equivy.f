C    @(#)equivy.f	20.5 11/11/97
      subroutine equivy

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/apcom.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cont.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/norder.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/red2.inc'
      include 'ipfinc/red5.inc'
      include 'ipfinc/red7.inc'
      include 'ipfinc/reliab.inc'
      include 'ipfinc/comm_mode.inc'
      include 'ipfinc/miscfile.inc'

      common /pkqkxx/pk, qk
      common /intown/intown(MAXBUS)
      common /time1/time(10), idebug

      common /scratch/numfree, free(2, MAXBUS)
      
      integer free
c
c***KLN Single to double precision conversion.
c
      complex * 16  a(2), v(2), y(2, 2), oldy(2, 2)
c
c***KLN Single to double precision conversion.
c
      double precision g(2000), b(2000), ek, fk, vk
      double precision xa, xb, ra, rb, cg, c
c
      real etol
c
      double precision ai1(2, 3)
c
      character intown*3

      external kmpr10, swap10, komp_ykm, swap_ykm
c
      integer status, open_file, count, temp_mtrx(MAXMTX)
c
      etol = case1(31)
      call orei()

C     Flag retained border nodes

      do kt = 1, ntot
        if (ikk(1,kt) .eq. 0) then
          lf = km(kt)
          ls = lf + kmlen(kt) - 1
          do l = lf, ls
            mt = ikmu(l)
            ikk(3, mt) = 1
          enddo
        endif
      enddo

c     Begin reduction -- nodes of each set are identified from the
c     attributes of the "ikk" array

c      1  = 0/1:  node is not/is retained
c      3  = 0/n:  node belongs to rei cluster n (n>0)

      if (idebug .ne. 0) then

        write (*, 10000)
10000   format (' EQUIVY (post REI) - check injections ')
        call check_pq

        write (dbug, 10002)
10002   format ('1 DEBUG DUMP OF ARRAYS '//'     BUS     BASE     IKK'
     &   /)
        do kt = 1, ntot
          write (dbug, 10010) kt, intbus(kt), intbas(kt), (ikk(i, kt), 
     &     i = 1, 5)
10010     format (i6, 2x, a10, f6.1, 5i4)
        enddo
      endif

C     NETSW identifies the pass:
c       0 -- eliminate and normalize all eliminated nodes
c       1 -- perform partial elimination on all retained nodes

      ik = 1
      do netsw = 0, 1
        do kt = 1, ntot	! end of netsw loop
          if (kmlen(kt) .gt. 0) then
            if (ikk(1, kt) .eq. netsw) then

c             1. Build packed working row KT in arrays KOLUMS,
C                KORDER, G, and B.
c             2. Convert distributed generations and loads of
c                eliminated nodes into appropriate current and
c                admittance.

              ikkind(1, kt) = ik
              ek = e(kt)
              fk = f(kt)
              vk = ek*ek + fk*fk
              if (vk .ne. 0) then
                if (netsw .ne. 1) then
                  a(1) = dcmplx(inetr(kt), ineti(kt))
                  v(1) = dcmplx(e(kt), f(kt))
                  a(2) = a(1)*cdabs(v(1))/conjg(v(1))
                  ai(1, kt) = (pnetu(kt)*ek+qnetu(kt)*fk)/vk 
     &                      - dble(a(2))
                  ai(2, kt) = (pnetu(kt)*fk-qnetu(kt)*ek)/vk 
     &                      - dimag(a(2))
                  goto 100
                endif
              endif
              ai(1, kt) = 0.0d0
              ai(2, kt) = 0.0d0
  100         do i = 3, 6
                ai(i, kt) = 0.0d0
              enddo
              lp = 0
              kl = 0
              ks = kt
              if (ikk(1, kt) .eq. 0) ks =  - kt
              lf = km(kt)
              ls = lf + kmlen(kt) - 1
              do lsw = 0, 1
                do l = lf, ls
                  mt = ikmu(l)
                  if (ikk(1, mt) .eq. lsw) then
                    ms = mt
                    if (ikk(1, mt) .eq. 0) ms =  - mt
                    if (kl .le. 0) then
                      if (isign(1, ks) .eq. isign(1, ms)) then
                        if (mt .lt. kt) goto 110

                      elseif (ms .lt. ks) then
                        goto 110
                      endif
                      lp = lp + 1
                      kl = lp
                      korder(lp) = lp + 1
                    endif
  110               lp = lp + 1
                    kolum(lp) = ms
                    korder(lp) = lp + 1
                    g(lp) = gkmu(l)
                    b(lp) = bkmu(l)
                  endif
                enddo
              enddo

              if (kl .eq. 0) then
                lp = lp + 1
                kl = lp
                ms = ks
              endif
              kolum(kl) = ks
              g(kl) = gkku(kt)
              b(kl) = bkku(kt)
              max = ms
              korder(lp) = 0
              mend = lp + 1
              ko = lp
              mel = 1
              if (idebug .gt. 1) then
                write (dbug, 10020) ks, ai(1, kt), ai(2, kt), (l, kolum
     &           (l), korder(l), g(l), b(l), l = 1, lp)
10020           format ('0 DUMP OF ROW ', i5, ' I =', 2e13.5//
     &           ' INDEX    KOLUMN    KORDER            ', 
     &           'G              B    '//(3i8, 2e15.6))
              endif
              do while (.true.)
                ms = kolum(mel)
                mt = iabs(ms)
                if (netsw .ne. 1) then
                  if (isign(1, ms) .ne. isign(1, ks)) call erexit()
                  if (mt .ge. kt) goto 160

                elseif (ms .gt. 0) then
                  goto 160
                endif

c               Eliminate column MS from the packed working row.  
C               MS then assumes the values of ascending column numbers 
C               as the elimination proceedes.

                ik = ikkind(1, mt)
                ikstop = ikkind(2, mt)
                krw = mel
                ra = g(mel)
                rb = b(mel)
                if (idebug .gt. 1) then
                  write (dbug, 10030) ms, ra, rb, ik, ikstop
10030             format (' ELIMINATING COLUMN ', i5, ' YKK =', 2e13.5, 
     &             ' Pointers= ', 2i6)
                endif
                ai(1, kt) = ai(1, kt) - ra*ai(1, mt) + rb*ai(2, mt)
                ai(2, kt) = ai(2, kt) - ra*ai(2, mt) - rb*ai(1, mt)
                do while (ik .le. ikstop)
                  ms = amtrx(ik)
                  mt = iabs(ms)
                  xa = amtrx(ik+1)
                  xb = amtrx(ik+2)

c                 Search packed working row for column MS.  If found, 
C                 identify with MLC. If not found, insert into row and 
C                 identify with MLC.

                  if (isign(1, ms) .eq. isign(1, max)) then
                    if (mt .gt. iabs(max)) goto 130

                  elseif (ms .gt. max) then
                    goto 130
                  endif
                  do while (.true.)
                    ka = kolum(krw)
                    if (isign(1, ka) .eq. isign(1, ms)) then
                      if (mt .lt. iabs(ka)) goto 140
                      if (mt .le. iabs(ka)) goto 120
                    elseif (ms .lt. ka) then
                      goto 140
                    elseif (ms .eq. ka) then
                      goto 120
                    endif
                    ko = krw
                    krw = korder(krw)
                    if (krw .eq. 0) call erexit()
                  enddo

  120             mlc = krw
                  goto 150

  130             max = ms
                  ko = lp
                  lp = mend
  140             korder(mend) = korder(ko)
                  kolum(mend) = ms
                  korder(ko) = mend
                  ko = mend
                  mlc = mend
                  mend = mend + 1
                  if (mend .gt. 601) call erexit()
                  g(mlc) = 0.0
                  b(mlc) = 0.0
  150             g(mlc) = g(mlc) - ra*xa + rb*xb
                  b(mlc) = b(mlc) - ra*xb - rb*xa
                  ik = ik + 3
                enddo

                mel = korder(mel)
                if (mel .eq. 0) call erexit()
              enddo

c             Begin normalization of row if eliminated.  Otherwise, 
C             store values of residual reduced elements.

  160         ik = ikkind(1, kt)
              if (netsw .eq. 0) then
                if (ms .ne. ks) call erexit()
                ra = g(mel)
                rb = b(mel)
                c = 1.0/(ra**2+rb**2)
                ra = ra*c
                rb =  - rb*c
                cg = ai(1, kt)*ra - ai(2, kt)*rb
                ai(2, kt) = ai(2, kt)*ra + ai(1, kt)*rb
                ai(1, kt) = cg
                mel = korder(mel)
              endif
              do while (mel .gt. 0)
                amtrx(ik) = kolum(mel)
                if (netsw .eq. 0) then
                  amtrx(ik+1) = g(mel)*ra - b(mel)*rb
                  amtrx(ik+2) = b(mel)*ra + g(mel)*rb
                else
                  amtrx(ik+1) = g(mel)
                  amtrx(ik+2) = b(mel)
                endif
                ik = ik + 3
                mel = korder(mel)
              enddo

              ikstop = ik - 1
              ikstrt = ikkind(1, kt)
              ikkind(2, kt) = ikstop
              if (idebug .gt. 1) then
                write (dbug, 10040) ks, ai(1, kt), ai(2, kt), ikstrt, 
     &           ikstop, (ifix(sngl(amtrx(i))), amtrx(i+1), amtrx(i+2), 
     &           i = ikstrt, ikstop, 3)
10040           format ('  REDUCED ROW ', i5, ' I =', 2e13.5, 
     &           ' Pointers=', 
     &           2i6, /'      KOLUM           G               B  ', //
     &           (i10, 2e15.5))
              endif
            endif
          endif
        enddo
      enddo

      ntotr = 0
      ltotr = 0
      numfree = 0
      do kt = 1, ntot
        if (ikk(1, kt) .eq. 0) then
          km(kt) =  -km(kt)
          goto 250
        endif
          ntotr = ntotr + 1
          ltotr = ltotr + (ikkind(2, kt)-ikkind(1, kt)+1)/3 - 1
          if (ikk(3, kt) .eq. 0) goto 250
          ek = e(kt)
          fk = f(kt)
          vk = sqrt(ek**2+fk**2)
          j5 = ikkind(1, kt)
          j6 = ikkind(2, kt) - j5 + 1
          js = j6/3
          do i = 1, js
            j = j5 + 3*i - 3
            yred(1, i) = amtrx(j)
            yred(2, i) = amtrx(j+1)
            yred(3, i) = amtrx(j+2)
          enddo

          do kl = 1, js
            if (yred(1, kl) .eq. kt) goto 170
          enddo
          call erexit()
  170     lf = km(kt)
          ls = lf + kmlen(kt) - 1

C         A check has not been made for ZERO connections--maybe OK.

          ksw = 1
          jt = 0
          lt = lf - 1
          inext = 660

C       Increment indices to IKMU

        do while (.true.)
          lt = lt + 1
          if (lt .le. ls) then
            m1 = ikmu(lt)
          elseif (ksw .eq. 1 .or. ksw .eq. 3) then
            ksw = ksw + 1
          endif
          if (inext .eq. 690) goto 190

C         Increment branches pertaining to YRED

  180     jt = jt + 1
          if (jt .le. js) then
            m2 = yred(1, jt)
            if (m2 .lt. 0) call erexit()
            if (m2 .eq. kt) goto 180
          elseif (ksw .le. 2) then
            ksw = ksw + 2
          endif

c         Perform a merge pass between YMTRX and YRED to determine low
c         admittance equivalent branches which could be replaced with 
C         current injections to conserve sparsity.

  190     if (ksw .ne. 2) then
            if (ksw .eq. 3 .or. ksw .eq. 4) goto 240

            if (idebug .gt. 1) then
              write (dbug, 10050) intbus(m1), intbas(m1), intbus(m2), 
     &         intbas(m2)
10050         format (' BRANCH COMPARISION -- "YMTRX" ', a8, f7.1, 
     &         ' "YRED" ', a8, f7.1)
            endif

            if (m2 .ge. m1) goto 210
          endif

C         Check G/B ratio of equivalent branches

          oldy(1, 2) = dcmplx(yred(2, jt), yred(3, jt))
          oldy(1, 1) =  - oldy(1, 2)
          oldy(2, 1) = oldy(1, 2)
          oldy(2, 2) = oldy(1, 1)
          do i = 1, 2
            do j = 1, 2
              y(i, j) = oldy(i, j)
            enddo
          enddo
          v(1) = dcmplx(e(kt), f(kt))
          v(2) = dcmplx(e(m2), f(m2))
          call fxrxrt(y, v, etol)

          if (idebug .gt. 1) write (dbug, 10060) intbus(m2), intbas(m2)
10060     format (' ADDED EQUIVALENT BRANCH ', a8, f7.1)
          if (y(1, 2) .ne. oldy(1, 2)) then
            if (cdabs(y(1, 2)) .eq. 0.0) then
              yred(1, jt) =  - m2
              ltotr = ltotr - 1
            else
              yred(2, jt) = yred(2, jt) + dreal(y(1, 2)-oldy(1, 2))
              yred(3, jt) = yred(3, jt) + dimag(y(1, 2)-oldy(1, 2))
            endif
            do i = 1, js
              if (yred(1, i) .eq. kt) goto 200
            enddo
            call erexit()
            goto 180
  200       yred(2, i) = yred(2, i) + dreal(y(1, 1)-oldy(1, 1))
            yred(3, i) = yred(3, i) + dimag(y(1, 1)-oldy(1, 1))
            if (idebug .gt. 1) write (dbug, 10070) intbus(kt), intbas
     &       (kt), intbus(m2), intbas(m2), oldy(1, 1), oldy(1, 2), y(1, 
     &       1), y(1, 2), yred(2, i), yred(3, i), yred(2, jt), yred(3, 
     &       jt)
          endif
          goto 180
  210     if (m2 .eq. m1) then

C           Check G/B ratio of equivalent parallels

            oldy(1, 2) = dcmplx(yred(2, jt)-gkmu(lt), yred(3, jt)-bkmu
     &       (lt))
            oldy(1, 1) =  - oldy(1, 2)
            oldy(2, 1) = oldy(1, 2)
            oldy(2, 2) = oldy(1, 1)
            do i = 1, 2
              do j = 1, 2
                y(i, j) = oldy(i, j)
              enddo
            enddo
            v(1) = dcmplx(e(kt), f(kt))
            v(2) = dcmplx(e(m2), f(m2))
            call fxrxrt(y, v, 0.0)
            if (y(1, 2) .ne. oldy(1, 2)) then
              yred(2, jt) = yred(2, jt) + dreal(y(1, 2)-oldy(1, 2))
              yred(3, jt) = yred(3, jt) + dimag(y(1, 2)-oldy(1, 2))
              do i = 1, js
                if (yred(1, i) .eq. kt) goto 220
              enddo
              call erexit()
              goto 230
  220         yred(2, i) = yred(2, i) + dreal(y(1, 1)-oldy(1, 1))
              yred(3, i) = yred(3, i) + dimag(y(1, 1)-oldy(1, 1))
              if (idebug .gt. 1) write (dbug, 10070) intbus(kt), intbas
     &         (kt), intbus(m2), intbas(m2), oldy(1, 1), oldy(1, 2), y
     &         (1, 1), y(1, 2), yred(2, i), yred(3, i), yred(2, jt), 
     &         yred(3, jt)
10070         format (' Eliminate G on branch ', a8, f7.1, 1x, a8, f7.1
     &         /'         2-port Y(old) ', 4e12.5, /, 
     &         '         2-port Y(new) ', 4e12.5, /, 
     &         '                Y      ', 4e12.5)
            endif
  230       continue
            inext = 660
          else
            inext = 690
          endif
        enddo

  240   continue
   
        call pkqk1(kt, js, yred, ai(1, kt))
        pnetu(kt) = pnetu(kt) + ek*ai(1, kt) + fk*ai(2, kt)
        qnetu(kt) = qnetu(kt) + fk*ai(1, kt) - ek*ai(2, kt)

C       Relocate y-matrix if kmlen(kt) < count

        lf = km(kt)
        ls = kmlen(kt)
        count = 0
        do i = 1, js
          if (yred(1,i) .gt. 0 .and. yred(1,i) .ne. kt) 
     &      count = count + 1
        enddo
        if (ls .lt. count) then
          i = 1
          numfree = numfree + 1
          free(1, numfree) = lf
          free(2, numfree) = ls
          lf = 0
          do while (i .lt. numfree .and. lf .eq. 0)
            if (free(2, i) .gt. 0 .and. free(2, i) .ge. count) then
              lf = free(1, i)
              free(1, i) = 0
              free(2, i) = 0
            else
              i = i + 1
            endif
          enddo
          if (lf .eq. 0) then
            if (yptr+count .ge. MAXYE) then
              write (errbuf(1), 10080) MAXYE
10080         format (' More than ', i5, ' Y-matrix entities')
              call prterx('F', 1)
              yptr = 1
              kmlen(kt) = 0
            else
              lf = yptr
              yptr = yptr + count
            endif
          endif
        endif

        ls = lf - 1
        do j = 1, js
          if (yred(1, j) .gt. 0) then
            if (j .eq. kl) then
              gkku(kt) = yred(2, j)
              bkku(kt) = yred(3, j)
            else
              ls = ls + 1
              ikmu(ls) = yred(1, j)
              gkmu(ls) = yred(2, j)
              bkmu(ls) = yred(3, j)
            endif
          endif
        enddo

        km(kt) = lf
        kmlen(kt) = ls - lf + 1
c
c       If all branches have been deleted, delete that bus also
c
        if (kmlen(kt) .eq. 0) then
           ntotr = ntotr - 1
           ikk(1,kt) = 0
           km(kt) = -lf
        endif

  250   continue
      enddo
      ltotr = ltotr/2
      ltot = ltotr
      write (outbuf, 10090) ntotr, ltotr
      call prtout(1)
10090 format ('0 REDUCED NETWORK HAS ', i4, ' BUSES AND ', i4, 
     & ' BRANCHES')

C     Compress y-matrix

      do kt = 1, ntot
        norder(kt) = kt
      enddo

      call qiksrt(1, ntot, kmpr10, swap10)
      yptr = 1
      do jt = 1, ntot
        kt = norder(jt)
        if (idebug .gt. 0) then
          write (dbug, 10100) jt, kt, km(kt), kmlen(kt), yptr
10100     format (' JT ', i4, ' KT ', i4, ' km() ', i8, ' kmlen() ', 
     &     i4, ' yptr ', i8)
        endif
        if (km(kt) .le. 0 .or. kmlen(kt) .eq. 0) goto 260
        if (km(kt) .ne. yptr) then
          if (km(kt) .le. yptr) call erexit()
          lf = km(kt)
          do l = 1, kmlen(kt)
            ikmu(l+yptr-1) = ikmu(l+lf-1)
            gkmu(l+yptr-1) = gkmu(l+lf-1)
            bkmu(l+yptr-1) = bkmu(l+lf-1)
          enddo
          km(kt) = yptr
          yptr = yptr + kmlen(kt)
          if (yptr .gt. MAXYE) then
            write (errbuf(1), 10110) yptr
10110       format ('0 REI SYSTEM OVERFLOWED ALLOCATED SPACE IN "uu" :'
     &       , i5)
            call prterx('W', 1)
            yptr = 1
          endif
        else
          yptr = yptr + kmlen(kt)
        endif
      enddo
      jt = ntot + 1
  260 if (jt-1 .ne. ntotr) call erexit()

C     Determine residual order of remaining nodes

      j = ntot + 1
      i = 0
      do kt = 1, ntot
        if (ikk(1, kt) .eq. 1) then
          i = i + 1
          norder(i) = kt
          oldord(kt) = i
          if (idebug .ne. 0) then
            write (dbug, 10120) i, kt, km(kt), kmlen(kt)
10120       format (' NEW ORDER ', i4, ' OLD ORDER ', i4, ' Pointers', 
     &       2i6)
          endif
        else
          j = j - 1
          norder(j) = kt
          oldord(kt) = j
        endif
      enddo
      ktot = i
      if (ktot .ne. ntotr) call erexit()
      ksy = yptr

C     Reorder KK ARRAY

      call mvnew1 (km, oldord, ntot)
      call mvnew1 (kmlen, oldord, ntot)

C     Reorder Y-matrix elements

      do kt = 1, ktot
        nt = 0
        ksort = 0
        lf = km(kt)
        ls = lf + kmlen(kt) - 1
        do l = lf, ls
          mt = ikmu(l)
          if (mt .gt. 0) then
            mt = oldord(mt)
            if (mt .lt. nt) ksort = 1
            ikmu(l) = mt
            nt = mt
          endif
        enddo
        if (ksort .ne. 0) call qiksrt(lf, ls, komp_ykm, swap_ykm)
      enddo

C     Reorder reduced network

      ktotzz = ktot

C     Open scratch file

      call close_file(lunscr)
      status = open_file(lunscr, ' ', 'U', 'W', iostat)

      rewind lunscr
      write (lunscr) (norder(i), i = 1, ntot)
      if (ktot .gt. 1000) then
        do i = 1, ktot
          norder(i) = i
        enddo
      else
        call eordr2(ktot, idebug)
      endif

C     Move isolated buses to bottom and truncate list. Isolation may
C     occur when min_equiv_y tolerance is large enough to eliminate
C     all equivalent branches)

      call chknet(ktot, krem, amtrx)
      do jt = 1, krem
        kt = amtrx(jt)
        do i = 1, ktot
          if (norder(i) .gt. norder(kt)) norder(i) = norder(i) - 1
        enddo
        norder(kt) = ktot
      enddo

      if (krem .gt. 0) then
        write (outbuf, 10130) krem, case1(31)
10130   format (1x, i4, ' buses were removed because of isolation ', 
     &   'by the elimination of low-admittance equivalent ', 
     &   'branches (less than ', f6.3, ' p.u.) ')
        call prtout(1)

      endif

c     Determine new amtrx array:  OLD = AMTRX(NEW)

      do i = 1, ktot
        j = norder(i)
        amtrx(j) = i
      enddo
      ktot = ktot - krem

c     Reorder KK array
c
c	Since amtrx is a double precision array,
c	use a temporary array for the routine call.
c
      do kt = 1, ktot
         temp_mtrx(kt) = amtrx(kt)
      enddo
      call movol1 (km, temp_mtrx, ktot)
      call movol1 (kmlen, temp_mtrx, ktot)
      do kt = 1, ktot
         amtrx(kt) = temp_mtrx(kt)
      enddo
c
c     Reorder Y matrix

      do kt = 1, ktot
        ksort = 0
        nt = 0
        lf = km(kt)
        ls = lf + kmlen(kt) - 1
        do l = lf, ls
          mt = ikmu(l)
          mt = norder(mt)
          if (mt .lt. nt) ksort = 1
          ikmu(l) = mt
          nt = mt
        enddo
        if (ksort .ne. 0) call qiksrt(lf, ls, komp_ykm, swap_ykm)
      enddo

c     Define new norder array:  OLD = NORDER(NEW)

      rewind lunscr
      read (lunscr) (norder(i), i = 1, ntot)
      do i = 1, ktotzz
        j = amtrx(i)
        k = norder(j)
        oldord(k) = i
      enddo
      do i = 1, ntot
        j = oldord(i)
        norder(j) = i
      enddo

c     redefine INP2OPT and OPT2INP

      do i = 1, ntot
        inp2opt(i) = 0
      enddo

      do i = 1, ktot
        j = norder(i)
        k = opt2inp(j)
        inp2opt(k) = i
      enddo

      k = 0
      do i = 1, ntot
        j = inp2opt(i)
        if (j .ne. 0) then
          k = k + 1
          opt2inp(j) = k
          inp2opt(k) = j
        endif
      enddo

c     Reorder bus injection data

      call movol1d(gkku, norder, ktot)
      call movol1d(bkku, norder, ktot)
      call movol1d(pnetu, norder, ktot)
      call movol1d(qnetu, norder, ktot)
      call movol1d(inetr, norder, ktot)
      call movol1d(ineti, norder, ktot)
      call movol1d(ploadu, norder, ktot)
      call movol1d(qloadu, norder, ktot)
      call movol1(vlimn, norder, ktot)
      call movol1(vlimx, norder, ktot)
      call movol1(ntypu, norder, ktot)

c     Reorder E, F arrays

      call movol1d(e, norder, ktot)
      call movol1d(f, norder, ktot)

C     Reorder VANGLE and VMAG arrays

      do kt = 1, ktot
        vmag(kt) = dsqrt(e(kt)**2+f(kt)**2)
        vangle(kt) = datan2(f(kt), e(kt))
      enddo

C     Reorder INTBUS, INTBAS arrays

      call movolc(intbus, norder, ktot)
      call movol1r(intbas, norder, ktot)

      if (idebug .ne. 0) then
        write (dbug, 10140) (i, inp2opt(i), opt2inp(i), intbus(i), 
     &   intbas(i), i = 1, ktot)
10140   format ('0     inp2opt opt2inp  BUS '/(3i6, 3x, a8, f6.1))
      endif

C     Reorder OWNER array

      call movolc(intown, norder, ktot)

C     Reorder V limits

      if (irect .ne. 1) then
        do kt = 1, ktot
          vlow(kt) = vlimn(kt)
          vhi(kt) = vlimx(kt)
        enddo

C       Reorder Q limits

        if (igenq .ne. 0) then
          do i = 1, igenq
            kt = iqlim(i)
            kt = oldord(kt)
            if ( kt .le. ntot ) then
               iqlim(i) = kt

C              Restore QNET into Q-limits. The QNET "bias" was removed before
C              EQUIVY to accomodate distributed injections.

               qhi(i) = qhi(i) + qnetu(kt)
               qlow(i) = qlow(i) + qnetu(kt)
            else
               iqlim(i) = -kt
            endif
          enddo
        endif
      endif

C     Reorder KLNC array

      do i = 1, nout
        k1 = klnc(1, i)
        k2 = klnc(2, i)
        klnc(1, i) = oldord(k1)
        klnc(2, i) = oldord(k2)
      enddo

C     Reorder KLNO array

      do i = 1, novl
        k1 = klno(1, i)
        k2 = klno(2, i)
        klno(1, i) = oldord(k1)
        klno(2, i) = oldord(k2)
      enddo


C     Reorder ORIG_TYPE array

      do i = 1, num_types
        k1 = orig_type(3, i)
        k2 = orig_type(4, i)
        orig_type(3, i) = oldord(k1)
        if (k2 .gt. 0) orig_type(4, i) = oldord(k2)
      enddo

C     Reschedule tableau

      if (nrelib .ne. 0) then
        do i = 1, nrelib
          k1 = nbsout(1, i)
          nbsout(1, i) = oldord(k1)
        enddo
      endif

C     Relocation of arrays completed.

      ntot = ktot

C     Modify PNET, QNET for single precision accuracy.

      do i = 1, ntot
        do j = 1, 6
          ai(j, i) = 0.0
        enddo
      enddo
      do j = 1, 3
        ai1(1, j) = 0.0
        ai1(2, j) = 0.0
      enddo
      do kt = 1, ntot
        lf = km(kt)
        ls = kmlen(kt)
        do l = 1, ls
          yred(1, l) = ikmu(lf+l-1)
          yred(2, l) = gkmu(lf+l-1)
          yred(3, l) = bkmu(lf+l-1)
        enddo
        ls = ls + 1
        yred(1, ls) = kt
        yred(2, ls) = gkku(kt)
        yred(3, ls) = bkku(kt)
        vk = dsqrt(e(kt)**2+f(kt)**2)
        pnetu(kt) = pnetu(kt) - vk*inetr(kt)
        qnetu(kt) = qnetu(kt) + vk*ineti(kt)
        inetr(kt) = 0.0
        ineti(kt) = 0.0
        call pkqk1(kt, ls, yred, ai1)
        pnetu(kt) = pk
        qnetu(kt) = qk
      enddo
      do kt = 1, nbslck
        if (ntypu(kt) .ne. 3) then
          write (errbuf(1)(1:120), 10150) intbus(kt), intbas(kt)
10150     format (
     &     ' SYSTEM SLACK BUS HAS BEEN DELETED.  NEW SLACK BUS IS ', 
     &     a8, f7.1)
          call prterx('W', 1)
          ntypu(kt) = 3
        endif
      enddo
      return
      end
