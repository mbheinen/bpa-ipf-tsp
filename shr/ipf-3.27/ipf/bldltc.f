C    @(#)bldltc.f	20.6 3/29/99
	subroutine bldltc(p, jtr, kerr)
c
c       This routine generates an LTC entity in "TRAN"
c
c       Input data:
c
c            p -   index to brnch_ptr(), which then points to
c                  "BRNCH" for the "R" record.
c                  (Restriction: valid transformer data must follow)
c
c       output data:
c
c           jtr - index of "TRAN" entity ("TRAN() - completed)
c
c           kerr - 0: normal
c                  1: R-record deleted, but T-record intact
c                  2: R-record deleted, no T-record
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/tran.inc'
        include 'ipfinc/lfiles.inc'

        integer rtype, p, pnxt, q, aq, qnxt, aqnxt, opnxt, oqnxt,
     &          aoqnxt, find_br, pold, orienttx

        character xbuf*120, rsbtyp  *1
        external find_br

        logical flag

        kerr = 0
        q = brnch_ptr(p)
        aq = iabs(q)
        pnxt = brnch_nxt(p)
        k1 = kx(p)
        k2 = ky(p)

        ntota=ntota+1
        if (ntota .gt. MAXLTC) then
          write (errbuf(1), 10000) MAXLTC
10000     format(' More than ',i4,
     &           ' LTC transformers. Remaining ones ignored.')
          errbuf(2) = ' '
          call bcdbrn (p, xbuf)
          write (errbuf(3), 10010) xbuf(1:80)
10010     format(13x, '(', a80, ')')
          call prterx ('W',3)
          kerr = 1
          go to 900
        endif

        jtr = ntota
        do i = 1,12
          ltran(i,jtr) = 0
        enddo

        write (rsbtyp, fmt='(a1)') brnch(3,aq)
        rtype = 1
        if (rsbtyp .eq. 'Q') rtype=2
        if (rsbtyp .eq. 'N') rtype=2
        if (rsbtyp .eq. 'P') rtype=3
        if (rsbtyp .eq. 'M') rtype=3
        if (rsbtyp .eq. 'O') rtype=4

        tmax=brnch(6,aq)
        tmin=brnch(7,aq)
c
c       check for missing controlled bus
c
        if (index(' VO',rsbtyp) .ne. 0) then
          if (kbrnch(4,aq).eq.0) then
            write (errbuf(1), 10020)
10020       format(' Missing controlled bus for "R" record. ',
     &             'R-record deleted.')
            errbuf(2) = ' '
            call bcdbrn (p,xbuf)
            write (errbuf(3), 10010) xbuf(1:80)
            call prterx ('W',3)
            kerr = 1
            go to 380
          endif
        endif
c
c       examine for missing tap limits
c
        if (rtype .ne. 3) then
          if (tmax .le. 0.0 .or. tmin .le. 0.0) then
            write (errbuf(1), 10030) tmax,tmin
10030       format('"R" data has incorrect tap limits: TMAX (', f6.2,
     &             ') .lt. TMIN (', f6.2, ')')
            errbuf(2) = ' '
            call bcdbrn (p,xbuf)
            write (errbuf(3), 10010) xbuf(1:80)
            call prterx ('E',3)
            if (tmax .le. 0.0) then
               brnch(6,aq) = 0.01
               tmax = brnch(6,aq)
            endif
            if (tmin .le. 0.0) then
               brnch(7,aq) = 0.01
               tmin = brnch(7,aq)
            endif
          endif
        endif
c
c       examine tap range
c
        if (tmax .lt. tmin) then
          write (errbuf(1), 10040) tmax, tmin
10040     format('"R" data has TMAX (', f6.2, ') .lt. TMIN (', f6.2,
     &           '). These limits have been interchanged.')
          errbuf(2) = ' '
          call bcdbrn (p,xbuf)
          write (errbuf(3), 10010) xbuf(1:80)
          brnch(6,aq) = tmin
          brnch(7,aq) = tmax
          tmax = brnch(6,aq)
          tmin = brnch(7,aq)
          call bcdbrn (p,xbuf)
          write (errbuf(4), 10010) xbuf(1:80)
          call prterx ('W',4)
        endif
c
c       Check for solitary R-record
c
        if (pnxt .eq. 0 .or. ky(pnxt) .ne. k2) then
          write (errbuf(1), 10050)
10050     format(' R-record is missing subsequent T record. R-record is 
     &deleted.')
          errbuf(2) = ' '
          call bcdbrn (p,xbuf)
          write (errbuf(3), 10010) xbuf(1:80)
          call prterx ('W',3)
          kerr = 2
          go to 380
        endif
        krmap = orienttx (p, brnch_nxt(p), k1x, k2x, tap1, tap2)

        ltran(1,jtr)=k1x
        ltran(9,jtr)=k2x
        ltran(10,jtr)=rtype
        tran(11,jtr)=brnch(8,aq)
        ltran(3,jtr)=0
        ltran(12,jtr)=0
c
c       krmap : Orientation of "R in KX(P), KY(P) with respect to 
c               TRAN(*,JTR):
c
c              1 = same orientation
c              2 = opposite orientation.
c
c       Determine controlled quantities
c
        go to (140,150,150,140) rtype
c
c       "R" and "RO"
c
  140   tran(4,jtr) = 0.0
        tran(5,jtr) = 0.0
        if (kbrnch(4,aq) .eq. k1x .or. kbrnch(4,aq) .eq. k2x) then
          kc = 1
          if (kbrnch(4,aq) .eq. k2x) kc = 2
          ltran(2,jtr) = -kc
        else 
          ltran(2,jtr)=kbrnch(4,aq)
        endif
        go to 170

  150   if (rsbtyp .eq. 'P' .or. rsbtyp .eq. 'Q') then
          ptol=sign(brnch(10,aq),brnch(9,aq))
          if ((krmap .eq. 1. and. q .gt. 0) .or.
     &        (krmap .eq. 2 .and. q .lt. 0)) then
            tran(4,jtr) = (brnch(9,aq) + ptol) / bmva
            tran(5,jtr) = (brnch(9,aq) - ptol) / bmva
          else
            tran(4,jtr) = (-brnch(9,aq) - ptol) / bmva
            tran(5,jtr) = (-brnch(9,aq) + ptol) / bmva
          endif
        else
          if ((krmap .eq. 1 .and. q .gt. 0) .or.
     &        (krmap .eq. 2 .and. q .lt. 0)) then
            tran(4,jtr) = brnch(9,aq) / bmva
            tran(5,jtr) = brnch(10,aq) / bmva
          else
            tran(4,jtr) = -brnch(10,aq) / bmva
            tran(5,jtr) = -brnch(9,aq) / bmva
          endif
        endif
c
c       Examine subsequent transformer card(s)
c
  170   icount=0

c       Begin branch (transformer) loop

        do while (pnxt .gt. 0)
          if (k2 .ne. ky(pnxt)) go to 370
          qnxt = brnch_ptr(pnxt)
          aqnxt = iabs(qnxt)

          ltype = brtype(pnxt)
c
c         check compatibility of "R" record with following records.
c
c                sec,  LM,   L,   R,   T,  TP,  LD,   E
          go to (190, 190, 190, 210, 230, 240, 190, 190) ltype

  190     write (errbuf(1), 10060)
10060     format(' Illegal branch type follows an "R" record. R-record
     &deleted.')
          errbuf(2) = ' '
          call bcdbrn (pnxt,xbuf)
          write (errbuf(3), 10010) xbuf(1:80)
          call prterx ('W',3)
          kerr = 2
          go to 380

  210     write (errbuf(1), 10070)
10070     format(' Duplicate "R" records. Second record deleted.')
          errbuf(2) = ' '
          call bcdbrn (p,xbuf)
          write (errbuf(3), 10010) xbuf(1:80)
          call bcdbrn (pnxt,xbuf)
          write (errbuf(4), 10010) xbuf(1:80)
          call prterx ('W',4)
          kerr = 1
          go to 380
c
  230     if (rtype .eq. 3) then
            write (errbuf(1), 10080)
10080       format(' Incompatible "RP" and "T " records. ',
     &             '"R" record deleted.')
            errbuf(2) = ' '
            call bcdbrn (p,xbuf)
            write (errbuf(3), 10010) xbuf(1:80)
            call bcdbrn (pnxt,xbuf)
            write (errbuf(4), 10010) xbuf(1:80)
            call prterx ('W',4)
            kerr = 1
            go to 380
          endif
          go to 270
c
  240     if (rtype .ne. 3) then
            write (errbuf(1), 10090)
10090       format(' Incompatible "R" and "TP" records. ',
     &             '"R" record deleted.')
            errbuf(2) = ' '
            call bcdbrn (p,xbuf)
            write (errbuf(3), 10010) xbuf(1:80)
            call bcdbrn (pnxt,xbuf)
            write (errbuf(4), 10010) xbuf(1:80)
            call prterx ('W',4)
            kerr = 1
            go to 380
          endif
  270     if (brsect(pnxt) .ne. 0) then
c
            write (errbuf(1), 10100)
10100       format(' Incompatible "R" record applies to branch ',
     &             'sections.  "R" record deleted.')
            errbuf(2) = ' '
            call bcdbrn (p,xbuf)
            write (errbuf(3), 10010) xbuf(1:80)
            call bcdbrn (pnxt,xbuf)
            write (errbuf(4), 10010) xbuf(1:80)
            call prterx ('W',4)
            kerr = 1
            go to 380
          endif
c
c         check validity of starting taps
c
  290     krmap = orienttx (p, pnxt, k1x, k2x, tap1, tap2)

          if (rtype .eq. 3) then
            if ((krmap .eq. 1 .and. q .lt. 0) .or.
     &          (krmap .eq. 2 .and. q. gt. 0)) then
              temp = tmax
              tmax = -tmin
              tmin = -temp
            endif
            t1 = dim(tap1, tmax) - dim(tmin, tap1)
          else
            t1 = dim(tap2, tmax) - dim(tmin, tap2)
          endif
c
          if (abs(t1) .gt. 0.005) then
c
            write (errbuf(1), 10110)
10110       format(' LTC tap range is incompatible with starting ',
     &             'taps of following transformer(s). ',
     &             '"R" record deleted.')
            errbuf(2) = ' '
            call bcdbrn (p,xbuf)
            write (errbuf(3), 10010) xbuf(1:80)
            call bcdbrn (pnxt,xbuf)
            write (errbuf(4), 10010) xbuf(1:80)
            call prterx ('W',4)
            kerr = 1
            go to 380
          endif
c
c         check if discrete taps match input data
c
          taps=brnch(8,aq)
          if (taps .gt. 0.0 .and. taps .lt. 2.0) then
            write (errbuf(1), 10120) taps
10120       format(' Illegal number of "taps" (', f4.0, ') ignored.')
            errbuf(2) = ' '
            call bcdbrn (p,xbuf)
            write (errbuf(3), 10010) xbuf(1:80)
            call prterx ('W', 3)
          else if (taps .ge. 2.0) then
            deltat = tmax - tmin
            dt = deltat/(taps-1.0)
            step = (tap2 - tmin)/dt
            at = amod(step,1.0)

            if (at .gt. 0.10 .and. at .lt. 0.90) then
              istep = step + 0.49
              tdisc = tmin + float(istep) * dt
              if (rtype .eq. 3) then
                tact = tap1
              else
                tact = tap2
              endif

              write (errbuf(1), 10130)
10130         format(' LTC starting tap is not at proper ',
     &               'discrete tap.')
              errbuf(2) = ' '
              write (errbuf(2), 10140) tact, step, tdisc
10140         format(' Starting tap ', f7.2, ' (tap no.',f7.2,
     &               ') should be = ',f7.2)
              errbuf(3) = ' '
              call bcdbrn (p,xbuf)
              write (errbuf(4), 10010) xbuf(1:80)
              call bcdbrn (pnxt,xbuf)
              write (errbuf(5), 10010) xbuf(1:80)
              call prterx ('I', 5)
            endif
          endif
c
c         check for homogeneity of subsequent parallels.
c
          icount = icount+1
          flag = .false.
          if (rtype .ne. 3) then   !Non phase-shifters

            tek = tap1 / base(k1x)
            told = tek / (tap2 / base(k2x))

            tminpu = tek/(tmax/base(k2x))
            tmaxpu = tek/(tmin/base(k2x))
            if (tmaxpu .gt. 1.400 .or. tminpu .lt. 0.600) then
              write (errbuf(1), 10150)
10150         format(' LTC transformer has excessive tap range')
              errbuf(2) = ' '
              call bcdbrn (p,xbuf)
              write (errbuf(3), 10010) xbuf(1:80)
              call prterx ('W',3)
            endif

            if (icount.eq.1) then
              tran(6,jtr) = tek
              tran(7,jtr) = tmaxpu
              tran(8,jtr) = tminpu
              tap(jtr) = told
              opnxt = pnxt
              oqnxt = qnxt
              aoqnxt = iabs(oqnxt)
            else
              if (tran(6,jtr) .ne. tek) flag = .true.
              if (tran(7,jtr) .ne. tmaxpu) flag = .true.
              if (tran(8,jtr) .ne. tminpu) flag = .true.
              if (tap(jtr) .ne. told) flag = .true.
            endif
          else            !Phase-shifters
            tek = tap2 / base(k2x)
            if (tek .eq. 0.0) tek = 1.0
            tmaxpu = 0.0174532925*tmax
            tminpu = 0.0174532925*tmin
            if (tmax .gt. 120.1 .or. tmin .lt. -120.1) then
              write (errbuf(1), 10160)
10160         format(' LTC phase shifter has excessive range.')
              errbuf(2) = ' '
              call bcdbrn (p,xbuf)
              write (errbuf(3), 10010) xbuf(1:80)
              call prterx ('W',3)
            endif
            told = 0.0174532925 * tap1
            if (icount .eq. 1) then
              tap(jtr) = told
              tran(6,jtr) = tek
              tran(7,jtr) = tmaxpu
              tran(8,jtr) = tminpu
            else
              if (tran(6,jtr) .ne. tek) flag = .true.
              if (tap(jtr) .ne. told) flag = .true.
            endif
          endif
          if (flag) then
c
c           non-homogeneous parallel ltc transformers detected.
c
            write (errbuf(1), 10170)
10170       format(' Parallel transformers have inconsistent taps.')
            errbuf(2) = ' '
            call bcdbrn (opnxt,xbuf)
            write (errbuf(3), 10010) xbuf(1:80)
            call bcdbrn (pnxt,xbuf)
            write (errbuf(4), 10010) xbuf(1:80)
            call prterx ('W',4)
c
c           Change taps of all parallels to be consistent with
c           the first tx
c
            if (rtype .ne. 3) then
              if (isign(1,qnxt) .eq. isign(1,oqnxt)) then
                brnch(9,aqnxt) = brnch(9,aoqnxt)
                brnch(10,aqnxt) = brnch(10,aoqnxt)
              else
                brnch(9,aqnxt) = brnch(10,aoqnxt)
                brnch(10,aqnxt) = brnch(9,aoqnxt)
              endif
            else
              if (isign(1,qnxt) .eq. isign(1,oqnxt)) then
                brnch(9,aqnxt) = brnch(9,aoqnxt)
              else
                brnch(9,aqnxt) = -brnch(9,aoqnxt)
              endif
            endif
          endif
          pnxt = brnch_nxt(pnxt)
        enddo
c
c       check whether "T" or "TP" records have been encountered.
c
  370   if (icount .eq. 0) then
          write (errbuf(1), 10180)
10180     format(' "R" record has no accompanying "T" records. ',
     &            '"R" record deleted.')
          errbuf(2) = ' '
          call bcdbrn (p,xbuf)
          write (errbuf(3), 10010) xbuf(1:80)
          call prterx ('W',3)
          kerr = 2
          go to 380
        endif
        go to 900
c
c       Error - delete "R" record from branch data
c
  380   kxtemp = k1
        kytemp = k2
        do while (kxtemp .ne. 0)
          pold = 0
          pnxt = kbsdta(16,kxtemp)
          flag = .true.
          do while (flag .and. pnxt .gt. 0)
            if (find_br (kxtemp, kytemp, ' ', 0, 4) .eq. pnxt) then
              if (pold .eq. 0) then
                kbsdta(16,kxtemp) = brnch_nxt(pnxt)
              else
                brnch_nxt(pold) = brnch_nxt(pnxt)
              endif
              kx(pnxt) = 0
              ky(pnxt) = 0
              brid(pnxt) = ' '
              brtype(pnxt) = 0
              brsect(pnxt) = 0
              brnch_ptr(pnxt) = 0
              flag = .false.
            endif
            pold = pnxt
            pnxt = brnch_nxt(pnxt)
          enddo
          if (kxtemp .eq. k1) then
            kxtemp = k2
            kytemp = k1
          else
            kxtemp = 0
            kytemp = 0
          endif
        enddo
c
c       Delete current entity in TRAN array
c
        do i = jtr+1,ntota
          do j = 1,12
            ltran(j,i-1) = ltran(j,i)
          enddo
          tap(i-1) = tap(i)
        enddo
        jtr = 0
        ntota = ntota-1

  900   return
        end
