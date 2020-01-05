C    %W% %G%
      subroutine cmde2_inp(inpfil)
      integer inpfil

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/cmde_com.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cont.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/intrst.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/outxrf.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/comm_mode.inc'
      include 'ipfinc/zbdata.inc'
      include 'ipfinc/dflrat.inc'

      common /time1/time(10), idebug
      common /iflag/iflag(MAXBUS)
      common /intown/intown(MAXBUS)

      common /common_mode_only/ common_mode_only
      logical common_mode_only
c
      character intown*3, code*8, cbuf(10)*8
      dimension oldg(20), linrat(2, 10), lintyp(2, 10), linbas(10)
      real linrat, linbas
      complex * 16 y(2, 2), v(2), a(2)
      complex ynom(2, 2)
      character jown*3, kown*3, id*1, tag*3, rattag*1, linown(10)*3
      logical ovldk1, outgk1, ovldk2, outgk2, label
      integer ptr, p, oldp, ierr
c
C     INPUT DATA, PARAMETERS AND PROGRAM INITIALIZATION
C     ----- ----- ---------- --- ------- --------------

      flag = 'cmde_init'

      call forbtm()

      write (outbuf, 10000)
10000 format ('Common Mode Analysis')
      call rpnlod()
      outbuf = ' Listing of Input Records'
      call shdlod(1)
      outbuf = ' '
      do i = 2, 5
        call shdlod(i)
      enddo

      call fortop()

C     INITILIZE PROGRAM PARAMETERS

      nrat = 0
      novl = 0
      nout = 0
      no = 0
      nc = 0
      num_network = 0
      olinbc = 0.02
      eovl = 1.0 + olinbc
      tpc = 1.0
      tps = 0.8

C     BRANCH SPECIFICATIONS AND OPTIONS
C     ------ -------------- --- -------

C     VOL, VOH ARE V LIMITS FOR OVERLOAD AND CONTINGENCY RESP.

      vol = 0.0
      vcl = 0.0
      voh = 9999.
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

      call cmde2_txt(inpfil)

      idebug = max0(kase1(8), kase1(30))

C     CONVERT BUS DATA TO INTERNAL ORDER

      do i = 1, ntot
        nb = opt2inp(i)
        intbus(i) = bus(nb)
        intbas(i) = base(nb)
        intown(i) = owner(nb)
      enddo

      do i = 1, ntot
        intrst(i) = 0
      enddo

C     If only a >OUTAGE command is processed, use it as a template
C     for an implied >OVERLOAD command.

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

      call cmde2_head()

C     NODE AND BRANCH SEARCH & SELECTION
C     ---- --- ------ ------ - ---------

      label = .false.

C     ****************************************************************
C     *               LOOP THRU ALL BUS NODES ....                   *
C     ****************************************************************

      do nbx = 1, ntot_alf
        nb = alf2inp(nbx)
        kt = inp2opt(nb)

C       EXCLUDE DC BUSES

        ktyp = kbsdta(1, nb)
        if (ktyp .ne. 5 .and. ktyp .ne. 12) then

C         END OF BUS-BRANCH LOOP
C         Check whether bus is in overload or contingency list

          call finzon(zone(nb), base(nb), outgk1, ovldk1)
          if (.not. outgk1 .and. .not. ovldk1) then

C           If zone only matches, the branch is not an overload or outage
C           candidate.  However, flag all load and generator nodes in the
C           specified zones as "interesting". 

            call finzon(zone(nb), 0.0, outgk1, ovldk1)
            if (outgk1 .or. ovldk1) then
              if ( dabs(pnetu(kt)) .gt. 1.0d0 .or. 
     &             dabs(qnetu(kt)) .gt. 1.0d0 ) then
                intrst(nb) = 1
                if (ntypu(kt) .ne. 3) ntypu(kt) = 2
              endif
            endif
          else
            if (ovldk1 .or. outgk1) intrst(nb) = 1

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
                     intrst(k1) = 1
                     intrst(k2) = 1
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

C                       PROCESS PHASE SHIFTER

                        ityp = 3
                        lintyp(1, numown) = ityp
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

C                   RETRIVE BRANCH  PI-EQUIVALENT DATA

                    call pieqiv(ptr, y, ierr)

C                   Check branch for overload interest (either terminal 
C                   must reside in the specified zones/bases).

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

                            if (jtype .eq. 2 .or. jtype .eq. 3) then
                              tag = 'MVA'
                            else
                              tag = 'AMP'
                            endif

                            pclod = baslod*tpc*100.
                            basrat = p9
                            ratnew = amax1(p9*baslod*eovl*tpc, basrat)
                            rat = rat*baslod*eovl
                            p8 = p8*baslod*eovl
                            isect = brsect(p)
                            write (outbuf, 10070) bus(k1), base(k1), 
     &                       bus(k2), base(k2), id, isect, jown, pclod, 
     &                       basrat, tag, rattag, ratnew, tag, rattag
10070                       format (2x, a8, f6.1, 2x, a8, f6.1, 2x, a1, 
     &                       3x, i2, 3x, a3, 2x, f7.0, 1x, f12.0, 1x, 
     &                       a3, 2x, a1, f11.0, 1x, a3, 2x, a1)
                            call prtout(1)
                          endif

                          if (jtype .eq. 2 .or. jtype .eq. 3) p8 =  - p8

C                         COMPUTE UPPER BOUND (FORWARD) PSI LIMITS 
C                         (LINE OR TRANSFORMER)

                          v1 = dsqrt(e(kt)**2+f(kt)**2)
                          v2 = dsqrt(e(mt)**2+f(mt)**2)
                          v(1) = y(1, 2)*dconjg(y(1, 1))*v1*v2
                          d = (rat*tps)**2 - (cdabs(y(1, 1))*v1)**2
     &                         - (cdabs(y(1, 2))*v2)**2
                          c = sngl( cdabs(v(1)) )
                          th12 = datan2(dimag(v(1)), dreal(v(1)))
                          if (c .eq. 0.0) c = 1.0e-10
                          if (abs(0.5*d/c) .gt. 1.0) then

C                         CALCULATE PSI LIMITS (PHASE SHIFTER)

                            write (dbug, 10080) bus(k1), base(k1), 
     &                        bus(k2), base(k2), id
10080                       format ('0 BRANCH ', a8, f6.1, ' TO ', 
     &                        a8, f6.1, ' PAR ', a1, 
     &                        ' HAS  AN INDETERMINABLE OVERLOAD ANGLE'
     &                        )
                            goto 150
                          else
                            theta = acos(0.5*d/c)

                            px = theta - th12
                            if (abs(px) .gt. 3.1415926) 
     &                        px =  - sign (6.2831853077, px) + px
                            py =  - theta - th12
                            if (abs(py) .gt. 3.1415926) 
     &                        py =  - sign (6.2831853077, py) + py
                            p1 = amax1(px, py)
                            p2 = amin1(px, py)

C                           COMPUTE LOWER BOUND(REVERSE) PSI LIMITS
C                           (LINE OR TRANSFORMER)

                            v(1) = y(2, 1)*dconjg(y(2, 2))*v1*v2
                            d = (rat*tps)**2 
     &                        - sngl(cdabs(y(2,2))*v2)**2 
     &                        - (sngl(cdabs(y(2, 1)))*v1)**2
                            c = sngl( cdabs(v(1)) )
                            th12 = datan2(dimag(v(1)), dreal(v(1)))
                            if (c .eq. 0.0) c = 1.0e-10
                            if (abs(0.5*d/c) .gt. 1.0) then
                              write (dbug, 10080) bus(k1), base(k1), 
     &                         bus(k2), base(k2), id
                              goto 150
                            else
                              theta = acos(0.5*d/c)

                              px = theta - th12
                              if (abs(px) .gt. 3.1415926) 
     &                          px =  -sign(6.2831853077, px) + px
                              py =  - theta - th12
                              if (abs(py) .gt. 3.1415926) 
     &                          py =  -sign(6.2831853077, py) + py
                              p3 = amax1(px, py)
                              p4 = amin1(px, py)
                              if (idebug .ne. 0) then
                                write (dbug, 10090) bus(k1), base(k1),
     &                            bus(k2), base(k2), id, p1, p2, p3, 
     &                            p4, rat, theta, th12
10090                           format (' PSI LIMITS: ', a8, f6.1, 
     &                            2x, a8, f6.1, 1x, a1, 7e10.3)
                              endif

C                             NOMINALIZE Y-MATRIX

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

C                             CALCULATE LOWER BOUND PSI LIMITS

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
                            endif
                          endif
                          if (jtype .eq. 3 .or. ltc .gt. 0) then
                            p1 = 1.0e-6
                            p2 = -1.0e-6
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
                            klno(1, novl) = k1
                            klno(2, novl) = k2
                            call putchr(3, jown, klno(3, novl))
                            if (jtype .ne. 3) jtype = 0
                            klno(4, novl) = jtype
                            klno(5, novl) = ichar(id)
                            klno(6, novl) = i6
                            klno(7, novl) = i7
                            klno(8, novl) = p
                            clno(1, novl) = p1
                            clno(2, novl) = p2
                            outxrf(novl) = p
                            clnobase(novl) = baslod * tpc
                            do i = 1, 8
                              clno(i+2, novl) = oldg(i)
                            enddo

                            clno(11, novl) = oldp8
                            clno(12, novl) = oldp9
                            clno(13, novl) = rat
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
            write (dbug, 10120) j, bus(k), base(k), bus(m), base(m), 
     &        klno(5, j), klno(4, j), klno(3, j), clno(11, j), 
     &        clno(12, j), (clno(i, j), i = 1, 2), (cbuf(i), i = 3, 10)
10120       format (1x, i4, 1x, a8, f6.1, 1x, a8, f6.1, 1x, a1, i2, 1x, 
     &       a4, 1x, 2f6.0, 2f6.3, 8a8)
          enddo
        endif

      endif

      ksw = 0
      if (novl .eq. 0) then
        write (errbuf(1), 10160)
10160   format (' NO OVERLOAD CHECK BRANCHES EXIST FOR THE ZONE AND', 
     &   ' VOLTAGE CLASS INPUT')
        call prterx('W', 1)
        ksw = 1
      endif
      if (ksw .ne. 0) call erexit()

      if (label) then
        call forbtm()
        outbuf = ' '
        do i = 1, 5
          call shdlod(i)
        enddo
        call fortop()
      endif

      call space(2)
      write (outbuf, 10180) novl
10180 format (16x, 'NUMBER OF BRANCHES CHECKED FOR OVERLOAD:        ', 
     &   i5)
      call prtout(1)

      return
      end
