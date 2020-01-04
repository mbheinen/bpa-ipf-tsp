C    %W% %G%
      subroutine cntinp

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/apcom.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cont.inc'
      include 'ipfinc/dflrat.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/intrst.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/outxrf.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/zbdata.inc'
      include 'ipfinc/comm_mode.inc'

      common /time1/time(10), idebug
      common /iflag/iflag(MAXBUS)
      common /intown/intown(MAXBUS)

      common /common_mode_only/ common_mode_only
      logical common_mode_only
c
      character intown*3, code*8, cbuf(10)*8
      dimension oldg(20), linrat(2, 10), lintyp(2, 10), linbas(10)
      real linrat, linbas
      real*8 c, d, ratio
      complex*16 y(2, 2), v(2), a(2), vx, vy
      complex ynom(2, 2)
      character jown*3, kown*3, id*1, tag*3, rattag*1, linown(10)*3
      logical ovldk1, outgk1, ovldk2, outgk2, label
      integer ptr, p, oldp, ierr
c
C     INPUT DATA, PARAMETERS AND PROGRAM INITIALIZATION
C     ----- ----- ---------- --- ------- --------------

      call forbtm()

      write (outbuf, 10000)
10000 format ('C O N T I N G E N C Y   A N A L Y S I S ')
      call rpnlod()
      outbuf = ' Listing of Input Command Records'
      call shdlod(1)
      outbuf = ' '
      do i = 2, 5
        call shdlod(i)
      enddo

      call fortop()

C     INITILIZE PROGRAM PARAMETERS

      tpc = 1.0
      tps = 0.8
      npass = 12
      irect = 0
      iequiv = 1
      tol = 0.02
      psm = 6.28
      icon2 = 0
      ilinbc = 0
      olinbc = 0.02
      nrat = 0
      ivr = 2
      trending = .false.

C     BRANCH SPECIFICATIONS AND OPTIONS
C     ------ -------------- --- -------

C     VOL,VCL,VOH,VCH, ARE V LIMITS FOR OVERLOAD AND CONTINGENCY RESP.

      vol = 0.0
      voh = 9999.
      vcl = 0.0
      vch = 9999.
      vl_network = 0.0
      vh_network = 9999.0
      vll = 0.95
      vlh = 1.05

      do i = 1, 40
        case1(i) = 0.0
      enddo
      case1(31) = 0.2

C     READ INPUT COMMANDS

      call otext()

      idebug = max0(kase1(8), kase1(30))

C     DISPATCH ON?, THEN OUTAGE-OVERLOAD LIST IS REQUIRED

      if (kase1(23) .ne. 0) kase1(3) = 2

C     CONVERT BUS DATA TO INTERNAL ORDER

      do i = 1, ntot
        nb = opt2inp(i)
        intbus(i) = bus(nb)
        intbas(i) = base(nb)
        intown(i) = owner(nb)
      enddo

C     Convert R/X ratios of all branches to R .FLT. X

      call rgone(kerr)

      do i = 1, ntot
        intrst(i) = 0
      enddo

C     If only a single >OVERLOAD or >OUTAGE command is processed,
C     default the missing command to a duplicate of the submitted
C     one.

      if (cntngn .and. .not. loadck) then
        vol = vcl
        voh = vch
        do i = 1, nc
          zno(i) = znc(i)
        enddo
        no = nc
      endif
      if (.not. cntngn .and. loadck) then
        vcl = vol
        vch = voh
        do i = 1, no
          znc(i) = zno(i)
        enddo
        nc = no
      endif

C     PRINT CAP HEADING

      call head()

C     CHANGE ALL PHASE SHIFTERS INTO SYMMETRIC PI BRANCHES

      do i = 1, jphno
        jphid(1, i) =  - jphid(1, i)
        jphid(2, i) =  - jphid(2, i)
      enddo
      call sympha(idebug)
C
C     Define intrst() array for any common-mode outages
C
      do i = 1, num_types
         if (orig_type(1,i) .eq. 1 .or. orig_type(1,i) .eq. 2) then
            kt = orig_type(3,i)
            intrst(kt) = 1
         else if (orig_type(1,i) .eq. 3 .or. orig_type(1,i) .eq. 4) then
            kt = orig_type(3,i)
            intrst(kt) = 1
            mt = orig_type(4,i)
            intrst(mt) = 1
         endif
      enddo

C     NODE AND BRANCH SEARCH & SELECTION
C     ---- --- ------ ------ - ---------

      nout = 0
      novl = 0
      label = .false.
      eovl = 1.0 + olinbc

C     ****************************************************************
C     *               LOOP THRU ALL BUS NODES ....                   *
C     ****************************************************************

      do nbx = 1, ntot_alf
        nb = alf2inp(nbx)
        kt = inp2opt(nb)

C       EXCLUDE DC BUSES

        ktyp = kbsdta(1, nb)
        if (ktyp .ne. 5 .and. ktyp .ne. 12) then

C       END OF BUS-BRANCH LOOP

C         Check whether bus is in overload or contingency list

          call finzon(zone(nb), base(nb), outgk1, ovldk1)
          if (.not. outgk1 .and. .not. ovldk1) then

C           If zone only matches, the branch is not an overload or outage
C           candidate.  However, flag all load and generator nodes in the
C           specified zones as "interesting". This will insure their 
C           eventual retention in the reduced equivalent network.

            call finzon(zone(nb), 0.0, outgk1, ovldk1)
            if (outgk1 .or. ovldk1) then
              if ( dabs(pnetu(kt)) .gt. 1.0d0 .or. 
     &             dabs(qnetu(kt)) .gt. 1.0d0 ) then
                intrst(kt) = 1
                if (ntypu(kt) .ne. 3) ntypu(kt) = 2
              endif
            endif
          else
            if (ovldk1 .or. outgk1) intrst(kt) = 1

            k2old = 0
            ltc = 0
            ksect = 1

C           LOOP THROUGH BRANCHES

            ptr = kbsdta(16, nb)
            do while (ptr .gt. 0)

C             EXCLUDE DC LINES

              ltype = brtype(ptr)
              if (ltype .ne. 2 .and. ltype .ne. 7) then

                k1 = kx(ptr)
                k2 = ky(ptr)
                if (k2old .ne. k2) ltc = 0
                k2old = k2
                nbr = iabs(brnch_ptr(ptr))
                id = brid(ptr)
                call getchr(3, jown, kbrnch(3, nbr))
                if (ltype .eq. 1) jown = '  '
                mt = inp2opt(k2)

C               CHECK FOR AUTO TRANSFORMER

                if (ltype .eq. 4) then
                  ltc = ptr
                else
                  if (ltype .eq. 1) then

C                   Use caution with equivalent pi-sections (LTYPE=1).
C                   PUTRAT overwrites BRNCH(4,*) - GK11 for equivalent
C                   branches.

                    rating = 0
                    call newrat(k1, k2, id, rating)
                  else
                    call getrat(ptr, rating, rattag, ratnom, ratthr, 
     &               ratllf, ratbtl)
                    call newrat(k1, k2, id, rating)
                    call putrat(ptr, rating)
                  endif
                  tmiles = 0.
                  if (ltype .eq. 3) tmiles = brnch(9, nbr)

C                 Check whether bus K2 is in overload or contingency list

                  call finzon(zone(k2), base(k2), outgk2, ovldk2)

                  if (ovldk2 .or. outgk2) then
                     intrst(mt) = 1
                     intrst(kt) = 1
                  endif

                  if (base(k2) .lt. 30.0) then
                     goto 160
                  else if ((inp2alf(k1) .lt. inp2alf(k2)) .and.
     &                     (ovldk1 .or. outgk1 .or. 
     &                      ovldk2 .or. outgk2)) then
                  else if ((inp2alf(k1) .gt. inp2alf(k2)) .and.
     &                     (ovldk1 .or. outgk1) .and.
     &                     (.not. ovldk2 .and. .not. outgk2)) then
                  else
                     goto 160
                  endif

C                   Check branch type -- equiv, section, or normal

                    if (ltype .eq. 1) then
                      ksect = 3
                      ck = 0
                      cm = 0
                      ba = base(nb)
                      rat = 0

C                     Find miniumum rating of each ownership in the 
c                     section string, or minimum line AND transformer 
c                     rating if both coexist in the section string.

                      numown = 0

                      oldp = 0
                      p = brnch_nxt(ptr)

                      do while (p .gt. 0 .and. (ky(p) .eq. ky(ptr) .and.
     &                          brid(p) .eq. brid(ptr)))

C                       Sections encountered. Find total miles, TX 
c                       conversion factors, and minimum non-zero rating.

                        ltype = brtype(p)
                        nbr = iabs(brnch_ptr(p))
                        call getchr(3, kown, kbrnch(3, nbr))
                        if (jown .eq. ' ' .and. kown .ne. ' ') 
     &                   jown = kown
                        do k = 1, numown
                          if (linown(k) .eq. kown) then
                            if (ltype .eq. 3 .or. ltype .eq. 8) then
                              if (lintyp(1, k) .eq. 1) goto 100
                            elseif (ltype .eq. 5) then
                              if (lintyp(1, k) .eq. 2) goto 100
                            elseif (ltype .eq. 6) then
                              if (lintyp(1, k) .eq. 3) goto 100
                            endif
                          endif
                        enddo
                        numown = numown + 1
                        linown(numown) = kown
                        linrat(1, numown) = 0.0
                        linrat(2, numown) = 0.0
                        lintyp(2, numown) = 0
                        linbas(numown) = ba
                        if (ltype .eq. 3 .or. ltype .eq. 8) then
                          ityp = 1
                        elseif (ltype .eq. 5) then
                          ityp = 2
                          ba = base(k2)
                        elseif (ltype .eq. 6) then
                          ityp = 3
                          ba = base(k2)
                        else
                          ityp = 0
                        endif
                        lintyp(1, numown) = ityp
  100                   continue
                        oldp = p
                        p = brnch_nxt(p)
                      enddo

                      do ix = 1, numown

                        p = brnch_nxt(ptr)
                        do while (p .gt. 0 .and. (ky(p) .eq. ky(ptr) 
     &                           .and. brid(p) .eq. brid(ptr)))

C                         Sections encountered. Find total miles, TX 
c                         conversion factors, and minimum non-zero 
c                         rating.

                          ltype = brtype(p)
                          nbr = iabs(brnch_ptr(p))
                          call getchr(3, kown, kbrnch(3, nbr))
                          if (linown(ix) .eq. kown) then
                            if (ltype .eq. 3 .or. ltype .eq. 8) then
                              if (lintyp(1, ix) .ne. 1) goto 110
                            elseif (ltype .eq. 5) then
                              if (lintyp(1, ix) .ne. 2) goto 110
                            elseif (ltype .eq. 6) then
                              if (lintyp(1, ix) .ne. 3) goto 110
                            endif

                            call getrat(p, rating, rattag, ratnom, 
     &                       ratthr, ratllf, ratbtl)
                            p9 = rating
                            if (ltype .eq. 5 .or. ltype .eq. 6) then

                              if (rating .eq. 0.0) then
                                n = nschrt(base(nb), base(k2))
                                if (n .gt. 0) then
                                  rating = zrat(3, n)
                                  p9 = rating
                                  call putrat(ptr, rating)
                                endif
                              endif
                              if (rattag .eq. 'B') then
                                ratsec = rating/bmva
                              else
                                ratsec = rating
     &                                 / (bmva*dsqrt(e(kt)**2+f(kt)**2))
                              endif
                              if (brnch_ptr(p) .gt. 0) then
                                ck = sqrt(brnch(9, nbr)/base(k1))
                                cm = sqrt(brnch(10, nbr)/base(k2))
                              else
                                ck = sqrt(brnch(10, nbr)/base(k1))
                                cm = sqrt(brnch(9, nbr)/base(k2))
                              endif
                            else
                              if (rating .eq. 0) then
                                n = nschrt(base(nb), 0)
                                if (n .gt. 0.0) then
                                  rating = zrat(3, n)
                                  p9 = rating
                                  call putrat(p, rating)
                                endif
                              endif
                              ratsec = 0.00173205808*rating*linbas(ix)
     &                         /bmva
                            endif
                            if (rating .gt. 0.0 .and. 
     &                         (rat .eq. 0.0 .or. ratsec .le. rat)) then
                              rat = ratsec
                              p8 = p9
                              lintyp(2, ix) = p
                              linrat(1, ix) = p8
                              linrat(2, ix) = rat
                            endif
                          endif
  110                     p = brnch_nxt(p)
                        enddo
                      enddo
                    else

C                     PROCESS NON-SECTIONS

                      ksect = 1
                      numown = 1
                      linown(numown) = jown
                      lintyp(2, numown) = ptr
                      linbas(numown) = base(nb)
                      if (ltype .eq. 5) then

C                       PROCESS TRANSFORMER

                        ityp = 2
                        lintyp(1, numown) = ityp
                        if (rating .eq. 0) then
                          n = nschrt(base(k1), base(k2))
                          if (n .gt. 0) then
                            rating = zrat(3, n)
                            call putrat(ptr, rating)
                          endif
                        endif
                        p8 = rating
                        if (rattag .eq. 'B') then
                          rat = rating/bmva
                        else
                          rat = rating/(bmva*dsqrt(e(kt)**2+f(kt)**2))
                        endif
                        if (brnch_ptr(ptr) .gt. 0) then
                          ck = sqrt(brnch(9, nbr)/base(k1))
                          cm = sqrt(brnch(10, nbr)/base(k2))
                        else
                          ck = sqrt(brnch(10, nbr)/base(k1))
                          cm = sqrt(brnch(9, nbr)/base(k2))
                        endif
                        linrat(1, numown) = p8
                        linrat(2, numown) = rat
                      elseif (ltype .eq. 3 .or. ltype .eq. 8) then

C                       PROCESS LINE

                        ityp = 1
                        lintyp(1, numown) = ityp
                        if (rating .eq. 0.0) then
                          n = nschrt(base(k1), 0)
                          if (n .gt. 0) then
                            rating = zrat(3, n)
                            call putrat(ptr, rating)
                          endif
                        endif
                        p8 = rating
                        rat = 0.00173205808*rating*base(k1)/bmva
                        linrat(1, numown) = p8
                        linrat(2, numown) = rat
                      else

C                       PROCESS PHASE SHIFTER - IF AUTOMATIC

                        ityp = 3
                        lintyp(1, numown) = ityp
                        if (ltc .eq. 0) then
                          write (errbuf(1), 10010) bus(k1), base(k1), 
     &                     bus(k2), base(k2)
10010                     format (' PHASE SHIFTER ', a8, f6.1, ' TO ', 
     &                     a8, f6.1, 
     &                     ' IS NOT AUTOMATIC. IT WILL NOT BE OUTAGED OR
     & CHECKED FOR OVERLOAD'
     &                     )
                          call prterx('W', 1)
                          goto 160
                        else

                          if (rating .eq. 0) then
                            n = nschrt(base(k1), 0)
                            if (n .gt. 0) then
                              rating = zrat(3, n)
                              call putrat(ptr, rating)
                            endif
                          endif
                          p8 = rating
                          if (rattag .eq. 'B') then
                            rat = rating/bmva
                          else
                            rat = rating/(bmva*dsqrt(e(kt)**2+f(kt)**2))
                          endif
                          linrat(1, numown) = p8
                          linrat(2, numown) = rat
                        endif
                      endif
                    endif

C                   RETRIVE BRANCH  PI-EQUIVALENT DATA

                    call pieqiv(ptr, y, ierr)

C                   Check branch for outage interest (either terminal must reside
C                   in the specified zones/bases).

                    if (outgk1 .or. outgk2) then

C                     CHECK FOR ACCEPTABLE  R/X RATIO

                      kf = km(kt)
                      kl = kf + kmlen(kt) - 1
                      do l = kf, kl
                        if (ikmu(l) .eq. mt) goto 120
                      enddo
                      call erexit()
                      goto 130
  120                 if (dabs(gkmu(l)) .gt. dabs(bkmu(l))) then
                        write (errbuf(1), 10020) intbus(kt), intbas(kt),
     &                    intbus(mt), intbas(mt), id
10020                   format (' Branch ', a8, f6.1, ' TO ', a8, f6.1, 
     &                   1x, a, ' is rejected as an outage candidate ', 
     &                   'because of its poor R/X ratio.')
                        call prterx('E', 1)
                        goto 140
                      endif

C                     Branch outage table formation
C                     ------ ------ ----- ---------

  130                 if (nout .eq. MXCNBR) then
                        write (errbuf(1), 10030) MXCNBR
10030                   format ('0MORE THAN ', i4, 
     &                   ' SINGLE-CONTINTENCY BRANCHES ', 
     &                   'IN "CAP". REMAINING CANDIDATES IGNORED.')
                        call prterx('W', 1)
                        nout = MXCNBR + 1
                        goto 160
                      elseif (nout .gt. MXCNBR) then
                        goto 160
                      else

                        nout = nout + 1
                        klnc(1, nout) = kt
                        klnc(2, nout) = mt
                        call putchr(3, jown, klnc(3, nout))

                        clnc(1, nout) =  - sngl( dreal(y(1, 2)) )
                        clnc(2, nout) =  - sngl( dimag(y(1, 2)) )
                        bk = sngl( dimag(y(1, 1)+y(1, 2)) )
                        bm = sngl( dimag(y(2, 2)+y(2, 1)) )
                        clnc(3, nout) = bk
                        clnc(4, nout) = bm
                        klnc(4, nout) = 1000 + ichar(id)
                        if (ityp .eq. 1) then

C                         LINE

                          clnc(5, nout) = 0
                          clnc(6, nout) = tmiles
                        elseif (ityp .eq. 2) then

C                         TRANSFORMER

                          clnc(5, nout) = ck
                          clnc(6, nout) = cm
                        else

C                         PHASE SHIFTER

                          clnc(6, nout) =  - 1
                          v(1) = dcmplx(e(kt), f(kt))
                          v(2) = dcmplx(e(mt), f(mt))
                          a(1) = v(1)*y(1, 1) + v(2)*y(1, 2)
                          a(2) = v(1)*y(2, 1) + v(2)*y(2, 2)
                          pk = sngl( dreal(v(1)*dconjg(a(1))) )
                          qk = sngl( dimag(v(1)*dconjg(a(1))) )
                          pm = sngl( dreal(v(2)*dconjg(a(2))) )
                          qm = sngl( dimag(v(2)*dconjg(a(2))) )
                          clnc(1, nout) = pk
                          clnc(2, nout) = pm
                          clnc(3, nout) = qk
                          clnc(4, nout) = qm
                          write (outbuf, 10040) bus(k1), base(k1), bus
     &                     (k2), base(k2), pk, qk, pm, qm
10040                     format (' PHASE SHIFTER OUTAGE ', a8, f7.1, 
     &                     2x, a8, f7.1, 5e12.4)
                          call prtout(1)
                        endif
                      endif
                    endif

C                   Check branch for overload interest (either terminal must reside
C                   in the specified zones/bases).

  140               if (ovldk1 .or. ovldk2) then

C                     BRANCH OVERLOAD TABLE FORMATION
C                     ------ -------- ----- ---------

C                     Loop through section ownerships
C                     ____ _______ _______ __________

                      do ix = 1, numown

C                     ADVANCE BRANCH INDEX

                        p8 = linrat(1, ix)
                        rat = linrat(2, ix)
                        jtype = lintyp(1, ix)
                        p = lintyp(2, ix)
                        nbr = iabs (brnch_ptr(p))
                        jown = linown(ix)
                        if (rat .ne. 0) then
                          p9 = p8
                          p8 = p8*tpc
                          rat = rat*tpc

C                         RELAX RATINGS OF ANY INITIALLY LOADED(BASE) LINES

                          v(1) = dcmplx(e(kt), f(kt))
                          v(2) = dcmplx(e(mt), f(mt))
                          a(1) = v(1)*y(1, 1) + v(2)*y(1, 2)
                          a(2) = v(1)*y(2, 1) + v(2)*y(2, 2)
                          c1 = sngl( cdabs(a(1))/rat )
                          c2 = sngl( cdabs(a(2))/rat )
                          baslod = amax1(abs(c1), abs(c2))
                          if (baslod .gt. 1.0 / eovl) then
                            if (.not. label) then

                              call forbtm()
                              write (outbuf, 10050) 100.0*eovl
10050                         format (
     &  ' Summary of overloaded branches in base case which have ratings
     & relaxed', f7.1, ' % of base value')
                              call shdlod(1)
                              write (outbuf, 10060)
10060                         format ('0 BRANCH', t33, 'PAR', t38, 
     &                         'SEC', t43, 'OWN', t49, '% LOADING', 
     &                         t65, 'RATING', t82, 'NEW RATING ')
                              call shdlod(2)
                              outbuf = '0'
                              call shdlod(3)
                              outbuf = ' '
                              do i = 4, 5
                                call shdlod(i)
                              enddo
                              call fortop()
                              label = .true.
                            endif

                            if (jtype .eq. 2) then
                              tag = 'MVA'
                            else
                              tag = 'AMP'
                            endif

                            pclod = baslod*tpc*100.
                            basrat = p9
                            ratnew = amax1(p9*baslod*eovl*tpc, basrat)
                            if (.not. trending) then
                              rat = rat*baslod*eovl
                              p8 = p8*baslod*eovl
                            endif
                            isect = brsect(p)
                            write (outbuf, 10070) bus(k1), base(k1), 
     &                       bus(k2), base(k2), id, isect, jown, pclod, 
     &                       basrat, tag, rattag, ratnew, tag, rattag
10070                       format (2x, a8, f6.1, 2x, a8, f6.1, 2x, a1, 
     &                       3x, i2, 3x, a3, 2x, f7.0, 1x, f12.0, 1x, 
     &                       a3, 2x, a1, f11.0, 1x, a3, 2x, a1)
                            call prtout(1)
                          endif

                          if (jtype .ne. 3) then
                            if (jtype .eq. 2) p8 =  - p8

C                           COMPUTE UPPER BOUND (FORWARD) PSI LIMITS 
C                           (LINE OR TRANSFORMER)

                            vx = y(1,1)*dconjg(y(1,2))*v(1)*dconjg(v(2))
                            c = cdabs(vx)
                            d = dble(rat*tps)**2 
     &                        - (cdabs(y(1,1)) * cdabs(v(1))) ** 2
     &                        - (cdabs(y(1,2)) * cdabs(v(2))) ** 2
                            if (c .eq. 0.0) c = 1.0d-10
                            ratio = 0.5 * d / c 
                            if (dabs(ratio) .gt. dble(1.000) .and.
     &                          dabs(ratio) .lt. dble(1.001)) then
c
c                             Use the approximate formula for th12
c
                              ratio = dble (rat*tps) 
     &                              / (cdabs(v(2)) * cdabs(y(1,2)))

                              if (ratio .lt. 1.0) then
                                px = dabs (dasin (ratio))
                                py = -px
                              else
                                write (errbuf(1), 10080) bus(k1), 
     &                            base(k1), bus(k2), base(k2), id
10080                           format ('0 BRANCH ', a8, f6.1, ' TO ', 
     &                            a8, f6.1, ' PAR ', a1, 
     &                       ' HAS  AN INDETERMINABLE OVERLOAD ANGLE')
                                call prterx ('W', 1)
                                goto 150
                              endif
                            else if (dabs(ratio) .gt. dble(1.0)) then

C                             CALCULATE PSI LIMITS (PHASE SHIFTER)

                              write (errbuf(1), 10080) bus(k1), 
     &                         base(k1), bus(k2), base(k2), id
                              call prterx ('W', 1)
                              goto 150
                            else
                              theta = dacos(ratio)
                              vy = y(1,1)*dconjg(y(1,2))
                              th12 = datan2(dimag(vy), dreal(vy))

                              px = theta - th12
                              if (abs(px) .gt. 3.1415926) px =  - sign
     &                         (6.2831853077, px) + px
                              py =  - theta - th12
                              if (abs(py) .gt. 3.1415926) py =  - sign
     &                         (6.2831853077, py) + py
                            endif
                            p1 = amax1(px, py)
                            p2 = amin1(px, py)

C                           COMPUTE LOWER BOUND(REVERSE) PSI LIMITS
C                           (LINE OR TRANSFORMER)

                            vx = y(2,2) * dconjg(y(2,1))
     &                         * v(2) * dconjg(v(1))
                            c = cdabs(vx)
                            d = dble(rat*tps)**2 
     &                        - (cdabs(y(2,2)) * cdabs(v(2))) ** 2
     &                        - (cdabs(y(2,1)) * cdabs(v(1))) ** 2
                            if (c .eq. 0.0) c = 1.0d-10
                            ratio = 0.5 * d / c 
                            if (dabs(ratio) .gt. dble(1.000) .and.
     &                          dabs(ratio) .lt. dble(1.001)) then
c
c                             Use the approximate formula for th12
c
                              ratio = dble (rat*tps) 
     &                              / (cdabs(v(2)) * cdabs(y(1,2)))

                              if (ratio .lt. 1.0) then
                                px = dabs (dasin (ratio))
                                py = -px
                              else
                                write (errbuf(1), 10080) bus(k1), 
     &                            base(k1), bus(k2), base(k2), id
                                call prterx ('W', 1)
                                goto 150
                              endif
                            else if (abs(ratio) .gt. 1.0) then

C                             CALCULATE PSI LIMITS (PHASE SHIFTER)

                              write (errbuf(1), 10080) bus(k1), 
     &                         base(k1), bus(k2), base(k2), id
                              call prterx ('W', 1)
                              goto 150
                            else
                              theta = dacos(ratio)
                              vy = y(2,2) * dconjg(y(2,1))
                              th12 = datan2(dimag(vy), dreal(vy))

                              px = theta - th12
                              if (abs(px) .gt. 3.1415926) 
     &                          px =  -sign(6.2831853077, px) + px
                              py =  - theta - th12
                              if (abs(py) .gt. 3.1415926) 
     &                          py =  -sign(6.2831853077, py) + py
                            endif
                            p3 = amax1(px, py)
                            p4 = amin1(px, py)
                            if (idebug .ne. 0) then
                              write (dbug, 10090) bus(k1), base(k1),
     &                          bus(k2), base(k2), id, p1, p2, p3, 
     &                          p4, rat, theta, th12
10090                         format (' PSI LIMITS: ', a8, f6.1, 
     &                          2x, a8, f6.1, 1x, a1, 7e10.3)
                            endif

C                           NOMINALIZE Y-MATRIX

                            do j = 1, 2
                              do i = 1, 2
                                ynom(i, j) = y(i, j)/rat
                              enddo
                            enddo

                            oldg(1) = real(ynom(1, 2))
                            oldg(2) = aimag(ynom(1, 2))
                            oldg(3) = real(ynom(1, 1))
                            oldg(4) = aimag(ynom(1, 1))
                            oldg(5) = real(ynom(2, 1))
                            oldg(6) = aimag(ynom(2, 1))
                            oldg(7) = real(ynom(2, 2))
                            oldg(8) = aimag(ynom(2, 2))

C                           CALCULATE LOWER BOUND PSI LIMITS

                            isw = 1
                            jsw = 1
                            if (p1 .ge. -p4) then
                              p1 =  - p4
                              isw =  - 1
                            endif
                            if (p2 .lt. -p3) then
                              p2 =  - p3
                              jsw =  - 1
                            endif

                            if (p1 .lt. 0.) p1 = .00000001
                            if (p2 .gt. 0.) p2 =  - .00000001

                            oldp8 = p8
                            oldp9 = p9
                            i6 = isw
                            i7 = jsw
                          elseif (ltc .eq. 0) then
                            goto 150
                          else
                            p3 = datan2(f(kt), e(kt)) 
     &                         - datan2(f(mt), e(mt))
                            if (brnch_ptr(p) .gt. 0) then
                              ck = brnch(9, nbr)/57.29577
                            else
                              ck = -brnch(9, nbr)/57.29577
                            endif
                            cm = brnch(9, ltc)/bmva
                            tmin = brnch(7, ltc)/57.29577
                            tmax = brnch(6, ltc)/57.29577
                            p1 = tmax - ck + p3
                            p2 = tmin - ck + p3
                            oldg(1) = ck
                            oldg(3) = tmin
                            oldg(5) = tmax
                            oldg(7) = cm
                            if (p1 .lt. 0.) p1 = .0000001
                            if (p2 .gt. 0.) p2 =  - .0000001
                          endif
                          if (novl .eq. MXOLBR) then
                            write (errbuf(1), 10100) MXOLBR
10100                       format ('0MORE THAN ', i4, 
     &                       ' OVERLOADED BRANCHES IN "CAP". ', 
     &                       'REMAINING CANDIDATES IGNORED.')
                            call prterx('W', 1)

C                           MARK THE CRITICAL OVERLOAD BRANCH

                            novl = MXOLBR + 1
                          elseif (novl .le. MXOLBR) then

                            novl = novl + 1
                            klno(1, novl) = kt
                            klno(2, novl) = mt
                            call putchr(3, jown, klno(3, novl))
                            if (jtype .ne. 3) jtype = 0
                            klno(4, novl) = jtype
                            klno(5, novl) = ichar(id)
                            klno(6, novl) = i6
                            klno(7, novl) = i7
                            clno(1, novl) = p1
                            clno(2, novl) = p2
                            outxrf(novl) = p
                            clnobase(novl) = baslod * tpc
                            do i = 1, 8
                              clno(i+2, novl) = oldg(i)
                            enddo

                            clno(11, novl) = oldp8
                            clno(12, novl) = oldp9
                          endif
                        endif

C                       End of Ownership loop
C                       ___ __ _________ ____

  150                   continue
                      enddo
                    endif
                endif
              endif

  160         continue
              if (ksect .eq. 3) then
                ksect = 1
                ptr = brnch_nxt(oldp)
              else
                ptr = brnch_nxt(ptr)
              endif
            enddo
          endif
        endif
      enddo

      novl = min0(novl, MXOLBR)
      nout = min0(nout, MXCNBR)

C     DEBUG PRINTOUT

      if (idebug .ne. 0) then

C       OUTPUT OVERLOAD LINES

        if (novl .ne. 0) then
          write (dbug, 10110)
10110     format ('0', 40x, 'LINES CHECKED FOR OVERLOAD'//48x, 'RATING'
     &     , 4x, 'ANG. LIMIT', 13x, '(LEFT)', 7x, 'ADMITTANCE', 6x, 
     &     '(RIGHT)'/16x, 'NAME', 15x, 
     &     'PAR. OWN.  LEFT RIGHT  LEFT RIGHT   G(KM)   ', 'B(KM)', 3x, 
     &     'G(KK)', 3x, 'B(KK)', 3x, 'G(MK)', 3x, 'B(MK)', 3x, 'G(MM)', 
     &     3x, 'B(MM)')
          do j = 1, novl
            k = klno(1, j)
            m = klno(2, j)
            do i = 3, 10
              if (clno(i, j) .ge. 10000.0 .or. clno(i, j) .le. -1000.0)
     &          then
                cbuf(i) = code (clno(i, j), 8, 3)
              else
                write (cbuf(i), '(f8.3)') clno(i, j)
              endif
            enddo
            write (dbug, 10120) j, intbus(k), intbas(k), intbus(m), 
     &       intbas(m), klno(5, j), klno(4, j), klno(3, j), clno(11, j)
     &       , clno(12, j), (clno(i, j), i = 1, 2), (cbuf(i), i = 3, 10)
10120       format (1x, i4, 1x, a8, f6.1, 1x, a8, f6.1, 1x, a1, i2, 1x, 
     &       a4, 1x, 2f6.0, 2f6.3, 8a8)
          enddo
        endif

C       OUTPUT OUTAGE LINES

        if (nout .ne. 0) then
          write (dbug, 10130)
10130     format ('0', 40x, 'LINES OUTAGED'//19x, 'NAME', 12x, 'PAR', 
     &     1x, 'OWN', 5x, 'CTK', 6x, 'CTM', 6x, 'GKM', 6x, 'BKM', 7x, 
     &     'BK', 7x, 'BM')

          do j = 1, nout
            k = klnc(1, j)
            m = klnc(2, j)
            nlin = klnc(4, j)/1000
            i = mod(klnc(4, j), 1000)
            id = char(i)
            do i = 1, 6
              if (clnc(i, j) .ge. 10000.0 .or.
     &            clnc(i, j) .le. -1000.0) then
                cbuf(i) = code (clnc(i, j), 8, 3)
              else
                write (cbuf(i), '(f8.3)') clnc(i, j)
              endif
            enddo
            write (dbug, 10140) j, intbus(k), intbas(k), intbus(m), 
     &       intbas(m), id, klnc(3, j), cbuf(5), cbuf(6),
     &       (cbuf(i), i = 1, 4), nlin
10140       format (1x, i4, 2(1x, a8, f6.1), 1x, a1, 1x, a3, 1x, 6a9, 
     &       i5)
          enddo
        endif
      endif

      ksw = 0
      if (nout .eq. 0) then
        write (errbuf(1), 10150)
10150   format ('No single_contingency outage branches exist for the zon
     &e and voltage class input')
        call prterx('W', 1)
        if (num_comm_mode .eq. 0) ksw = 1
      endif
      if (num_comm_mode .eq. 0 .and. nout .eq. 0) then
        write (errbuf(1), 10152)
10152   format ('No common_mode outages exist')
        call prterx('W', 1)
        ksw = 1
      endif
      if (novl .eq. 0) then
        write (errbuf(1), 10160)
10160   format (' NO OVERLOAD CHECK BRANCHES EXIST FOR THE ZONE AND', 
     &   ' VOLTAGE CLASS INPUT')
        call prterx('W', 1)
        ksw = 1
      endif
      if (ksw .ne. 0) call erexit()

C     PRINT #OUTAGES AND #OVERLOAD CHECKS

      if (label) then
        call forbtm()
        outbuf = ' '
        do i = 1, 5
          call shdlod(i)
        enddo
        call fortop()
      endif

      call space(2)
      nout_temp = nout
      if (common_mode_only) nout_temp = 0
      write (outbuf, 10170) nout_temp
10170 format (16x, 'NUMBER OF SINGLE-CONTINGENCY OUTAGES SIMULATED: ', 
     &   i5)
      call prtout(1)
      write (outbuf, 10172) num_comm_mode
10172 format (16x, 'NUMBER OF COMMON-MODE OUTAGES SIMULATED:        ', 
     &   i5)
      call prtout(1)
      write (outbuf, 10180) novl
10180 format (16x, 'NUMBER OF BRANCHES CHECKED FOR OVERLOAD:        ', 
     &   i5)
      call prtout(1)

      return
      end
