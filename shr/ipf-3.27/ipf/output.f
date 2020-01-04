C    @(#)output.f	20.12 8/19/99
C     ****************************************************************

C     File: output.f

C     Purpose: Multi-purpose output routine:
C     1. Generate a BPA-formatted bus-branch output report
C     2. Update analysis arrays with new system data.

C     Author: BPA staff        Date: 18 May 1992
C     Called by: prodat.f

C     ****************************************************************

      subroutine output

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/anlys.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arsort.inc'
      include 'ipfinc/asort.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/changr.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/com008.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/diag.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lodtyp.inc'
      include 'ipfinc/mwm.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/outpt2.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/zonlst.inc'
      include 'ipfinc/zonsum.inc'
c
C     Set up the record type codes....

      include 'ipfinc/bstype.inc'
      include 'ipfinc/brtype.inc'
      include 'ipfinc/ownhash.inc'

      common /jbr/jbr, mbptr
      integer jbr, mbptr

C     JBR is the index to the branch tables for the main branch
C     pointed to by the main branch pointer, MBPTR

      common /scratch/scratch(22*MAXBUS)
      integer flag(MAXBUS)
      equivalence (flag, scratch(3*MAXBUS+1))

      real qlim(5), systot(14)
      integer fichsx, ptr, qptr, orienttx, gtmetpnt, oldptr

      logical newfrm

      character tmwrd1*5, tmwrd2*5, rtype*1, ang*5, komnt*4, cbname*8, 
     &          kode2*1, kownr2*3, namtyp*10, l_zname*2, xbuf*120,
     &          kodeyr*2, l_kowner*3

C     *******************************************************

C     Set up pointers to P/Q curve data (buspqptr), X data (busxdtptr),
C     and TBX data (ptrtbx)

      if (.not. pq_flag) then
        do nb = 1, ntot
          buspqptr(nb) = 0
        enddo

        do i = 1, numcurv
          ieq = pqbusptr(i)
          if (ieq .gt. 0) buspqptr(ieq) = i
        enddo
        pq_flag = .true.
      endif

      if (.not. xdt_flag) then
        do nb = 1, ntot
          busxdtptr(nb) = 0
        enddo
        do i = 1, kxtot
          kxd = xdata(1, i)
          if (kxd .gt. 0) busxdtptr(kxd) = i
        enddo
        xdt_flag = .true.
      endif

      do nb = 1, ntot
        ptrtbx(nb) = 0
      enddo

      do i = 1, ntotb
        kxd = tbx(2, i)
        if (kxd .gt. 0) ptrtbx(kxd) = i
      enddo

      tbx_loaded = ordtbx

C     BPA version - print "pnet,qnet...", i.e. iprnet = 1
C     WAPA version - do not print "pnet,qnet...", i.e. iprnet = 0

      iprnet = 1

C     Override output listing options if failed solution occurs.

      if (lskp .eq. 3) then
        i = mod(kspare(10), 10)
        if (i .eq. 0) then
          kspare(6) = 0
        elseif (i .eq. 1) then
          kspare(6) = 2
          npzout = 0
        else
          kspare(6) = 1
        endif
        i = kspare(10)/10
        if (i .eq. 0) then
          kspare(7) = 0
        elseif (i .eq. 1) then
          kspare(7) = 2
          nfzout = 0
        else
          kspare(7) = 1
        endif
      endif

      lprtsw = min0(kspare(6), 1)
      fichsw = min0(kspare(7), 1)

      lprtsx = lprtsw
      fichsx = fichsw

      kerrsw = 1
      if (numchg .gt. 0) call chglis()

C     KOUTSW: 1 - no output
C     2 - partial output of selected zones
C     3 - full output

      koutsw = 2
      if (lprtsw+fichsw .eq. 0) koutsw = 1
      kanal = min0(kase1(20), 1)

      if (koptsw .eq. 0) then
        kauto = 0
        kareas = 0
      else
        kauto = itsw
        kareas = iasw
      endif

      iznsw = kspare(11) + 1
      idebug = kase1(28)

      if (koutsw .gt. 1) then
        write (outbuf, 10000)
10000   format ('$MFCB     OUTPUT')
        call pfomf(2)
        iframe = 0
      endif

C     Initialize "CAPCOR"

      do i = 1, ntot
        do j = 1, 2
          capcor(j, i) = 0.0
        enddo
      enddo

C     Close previous report and set up new output heading.....

      call forbtm()

C     Check for failed solution
C     LSKP = 0 :  Initial value before any solution
C     1 :  Solved OK
C     2 :  OLD PF6022 case, not re-solved
C     3 :  Diverged  -  no solution

      if (lskp .eq. 1 .or. lskp .eq. 2) then
        outbuf = ' * * * * DETAILED OUTPUT LISTING * * * *'
      else
        outbuf = ' >>* FAILED SOLUTION OUTPUT LISTING * <<'
      endif

      call rpnlod()
      call fortop()
      call space(2)

      ratln = spare(30)
      rattx = spare(31)
      rateff = spare(29)
      ratxef = spare(27)
      ratcef = spare(28)

      if (rattx .eq. 0) rattx = 80.0
      if (ratln .eq. 0) ratln = 80.0
      if (rateff .eq. 0) rateff = 90.0
      if (ratxef .eq. 0) ratxef = 0.04
      if (ratcef .eq. 0) ratcef = 0.02


C     Print comment cards.

      if (ncom .gt. 0) then
        write (outbuf, 10010)
10010   format (50x, 'CASE COMMENTS')
        call prtout(1)

        write (outbuf, 10020)
10020   format (t21, 84('*'))
        call prtout(1)

        write (outbuf, 10030)
10030   format (t21, '*', t104, '*')
        call prtout(1)

        do i = 1, ncom
          write (outbuf, 10040) com(i)(1:80)
10040     format (t21, '* ', a, t104, '*')
          call prtout(1)
        enddo

        write (outbuf, 10030)
        call prtout(1)
        write (outbuf, 10020)
        call prtout(1)
      endif

      if (koutsw .eq. 1) then

        write (outbuf, 10050)
10050   format ('0 NO OUTPUT LISTING WILL BE GIVEN')
        call prtout(1)
        call space(1)
      else

C       Output list for restricted zone list.

        if (npzout .gt. 0) then
          lprtsw = 1
          fichsw = 0
          write (outbuf, 10060)
10060     format ('0', 26x, 
     &     'Selected OUTPUT Listing from the following ZONES:')
          call prtout(1)
          call space(1)

          do kst = 1, npzout, 20
            kend = min0(kst+19, npzout)
            write (outbuf, 10070) (pzolst(k), k = kst, kend)
10070       format ('0', 20x, 20(a2, 2x))
            call prtout(1)
          enddo

          call space(1)
        endif

        if (nfzout .gt. 0) then
          lprtsw = 0
          if (kspare(16) .ge. 0) fichsw = 1
          write (outbuf, 10060)
          call prtout(1)
          call space(1)
          do kst = 1, nfzout, 20
            kend = min0(kst+19, nfzout)
            write (outbuf, 10070) (fzolst(k), k = kst, kend)
            call prtout(1)
          enddo
          call space(1)
        endif

        lprtsw = lprtsx
        fichsw = fichsx

        if (npzout .eq. 0 .and. nfzout .eq. 0) then
          write (outbuf, 10080)
10080     format ('0A COMPLETE OUTPUT LISTING OF ALL BUSSES', 
     &     ' WILL BE GIVEN')
          call prtout(1)
          call space(1)
        endif
      endif

      if (iznsw .eq. 3) then
        if (natot .gt. 0) then

C         OUTPUT LIST SORT BY AREA-NAME

          write (outbuf, 10090)
10090     format ('0', 20x, 'OUTPUT LISTING SORTED BY AREAS ')
          call prtout(1)
          write (outbuf, 10100)
10100     format ('0', 20x, 'AREA        ZONE COMPOSITION  ')
          call prtout(1)
          write (outbuf, 10110) ' '
10110     format ('0', 20x, '----------', 2x, 10('--  '), a1)
          call prtout(2)
          do k = 1, natot
            write (outbuf, 10120) arsnam(k), (arsens(j, k), j = 1, 22)
10120       format ('0', 20x, a10, 2x, 22(a2, 2x))
            call prtout(1)
          enddo
          write (outbuf, 10110) ' '
          call prtout(1)
          call space(1)
        else
          iznsw = 2
          goto 100
        endif
      endif
      if (nztot .le. 0) then
        if (iznsw .lt. 4) iznsw = 1
        goto 120
      endif
  100 if (iznsw .eq. 2) then

C       OUTPUT LIST SORT BY ZONE-NAME

        write (outbuf, 10130)
10130   format ('0', 20x, 
     &   'OUTPUT LISTING SORTED BY THE FOLLOWING ZONES ')
        call prtout(3)
        write (outbuf, 10060)
        call prtout(2)
        kend = nztot
        kst = 1
        do while (.true.)
          if (kend .gt. (kst+19)) kend = kst + 19
          write (outbuf, 10070) (acznam(k), k = kst, kend)
          call prtout(1)
          if (kend .ge. nztot) goto 110
          kst = kend + 1
          kend = nztot
        enddo

  110   write (outbuf, 10060)
        call prtout(2)
        call space(1)
      endif
  120 if (iznsw .eq. 4) then

C       OUTPUT LIST SORT BY OWNER-NAME

        write (outbuf, 10140)
10140   format ('0', 20x, 
     &   'OUTPUT LISTING SORTED BY THE FOLLOWING OWNERS ')
        call prtout(3)
        write (outbuf, 10150)
10150   format ('0', 26x, 
     &   'Selected OUTPUT Listing from the following OWNERS:')
        call prtout(2)
        kend = numown
        kst = 1
        do while (.true.)
          if (kend .gt. (kst+15)) kend = kst + 15
          write (outbuf, 10160) (owner_o(alf2own(k)), k = kst, kend)
10160     format ('0', 20x, 16(a3, 2x))
          call prtout(1)
          if (kend .ge. numown) goto 130
          kst = kend + 1
          kend = numown
        enddo

  130   write (outbuf, 10150)
        call prtout(2)
        call space(1)
      endif

      if (nztot .gt. 0) call zonint()
      ksw = 1

C     "FLAG" DENOTES PRINT/MICROFICHE STATUS

      ityp = 0
      if (kspare(6) .eq. 2) ityp = 1
      if (kspare(7) .eq. 2) ityp = ityp + 2

      do i = 1, ntot
        flag(i) = ityp
        if (kspare(6) .eq. 1) then
          do j = 1, npzout
            if (zone(i) .eq. pzolst(j)) goto 140
          enddo
          goto 150
  140     flag(i) = flag(i) + 1
        endif

  150   if (kspare(7) .eq. 1) then
          do j = 1, nfzout
            if (zone(i) .eq. fzolst(j)) goto 160
          enddo
          goto 170

  160     flag(i) = flag(i) + 2
        endif

  170   continue
      enddo


C     INITIALIZE LOSS QUANTITIES

      pinsum = 0.0
      qinsum = 0.0
      poutsm = 0.0
      qoutsm = 0.0
      plssum = 0.0
      qlssum = 0.0
      do i = 1, 14
        systot(i) = 0
      enddo
      cutoff = option(4)*bmva
      call anal()

C     BEGIN BUSES

      nb = 0
      iznb = 0
      l_zname = '##'
      l_kowner = '###'
      iral = 0
      do while (.true.)
        iznb = iznb + 1
        if (iznb .le. ntot_alf) then
          nb = sorto(iznb)
          if (iznsw .eq. 3) ira = barea(iznb)
          iprntn = 1
          if (flag(nb) .ne. 0) iprntn = 2
          zname = zone(nb)
          kowner = owner(nb)
        else
          ira = 0
          zname = '**'
          kowner = '***'
        endif

C       (ALFA,ZONE,AREA)
        if (iznsw .ne. 1) then
          if (iznsw .eq. 3) then
            lprtsw = lprtsx
            fichsw = fichsx
            if (ira .ne. iral) then
              if (iral .ne. 0) call arsum(iral)
              iral = ira
            endif
          elseif (iznsw .eq. 2) then
            if (zname .ne. l_zname  .and.  l_zname .ne. '##') then
              if (nztot .gt. 0) then
                call znsum(l_zname)
                if (iprntn .eq. 2) then
                  call forbtm()
                  call fortop()
                endif
              endif
            endif
            l_zname = zname
          elseif (iznsw .eq. 4) then
            if (kowner .ne. l_kowner  .and.  l_kowner .ne. '###') then
c              fix this later to give "owner" summary
c              if (nztot .gt. 0) then 
c                call znsum(l_zname)
                if (iprntn .eq. 2) then
                  call forbtm()
                  call fortop()
                endif
c              endif
            endif
            l_kowner = kowner
          endif
        endif
        if (iznb .gt. ntot_alf) goto 460
        iprint = iprntn
        lprtsw = mod(flag(nb), 2)
        if (kspare(16) .ge. 0) fichsw = flag(nb)/2
        if (iprint .eq. 2) call space(1)

        ktype = kbsdta(1, nb)
        kowner = owner(nb)
        ibsown = kowner

C       Generation conventions follow:

C       PGEN,  QGEN  : generation derived from input bus data
C       ( QGEN is always calculated)
C       PGENX, QGENX : total generation derived from solution
C       PGENMW,QGENMW: total generation derived from input data
C       (bus and continuation bus data)

        bsname = bus(nb)
        bsvolt = base(nb)
        kt = inp2opt(nb)
        ek = e(kt)
        fk = f(kt)
        voltpu = sqrt(ek*ek+fk*fk)
        voltkv = voltpu*bsvolt
        degree = 57.2957795*atan2(fk, ek)
        if (degree .gt. -99.9) then
          write (ang, 10170) degree
10170     format (f5.1)
        else

          write (ang, 10180) degree
10180     format (f5.0)
        endif
        vmin = vlimn(kt)
        vmax = vlimx(kt)
        pgenx = (ploadu(kt)+pnetu(kt))*bmva
        qgenx = (qloadu(kt)+qnetu(kt))*bmva
        pload = busdta(3, nb)
        qload = busdta(4, nb)
        skcond = busdta(5, nb)
        sksusp = busdta(6, nb)
        pmax = busdta(7, nb)
        pgen = busdta(8, nb)
        if (ktype .ne. 5 .and. ktype .ne. 12) then
          qmax = busdta(9, nb)
          qmin = busdta(10, nb)
        else
          qmax = 0.0
          qmin = 0.0
        endif
        qgen = 0.0
        pvlv = 0.0
        qvlv = 0.0
        skcond = skcond*voltpu*voltpu
        sksusp = sksusp*voltpu*voltpu
        skcap = amax1(0.0, sksusp)
        skreak = amin1(0.0, sksusp)
        call typno(type, ktype)
        gequiv = 0.0
        bequiv = 0.0
        bfixed = 0.0
        iarea = 0

C       INITIALIZE LINE FLOW QUANTITIES

        ltype = 1
        komnt = '    '
        iform = 1
        icapsw = 1
        pltot = 0.0
        qltot = 0.0
        pintot = 0.0
        qintot = 0.0d0
        poutot = 0.0
        qoutot = 0.0

C       1   2   3   4   5   6   7   8    9   10  11  12  13  14
C       " " "E" "S" "C" "D" "V" "Q" "G"  "O" "T" "X" "M" "F" "J"
        if (ktype .eq. 5 .or. ktype .eq. 12) then

C         D-C bus -- determine converter values

          pgen = 0.0
          qgen = 0.0
          pgenx = 0.0
          qgenx = 0.0
          pload = 0.0
          qload = 0.0
          qmin = 0.0
          qmax = 0.0d0
          skcond = 0.0
          sksusp = 0.0
          skcap = 0.0
          skreak = 0.0
          cbname = '        '
          voltcb = 0.0
          if (ktype .eq. 12) then
            do idc = 1, mtdcbs
              if (dcmtbs(1, idc) .eq. nb) goto 180
            enddo
            call erexit()

  180       pdc = dcmtbs(25, idc)
            qdc = dcmtbs(26, idc)
            vdc = dcmtbs(20, idc)
            pvlv = abs(pdc-dcmtbs(19, idc))
            qvlv = qdc
            adc = 1000.0*dcmtbs(19, idc)/dcmtbs(20, idc)

          else
            do idc = 1, kdtot
              if (dc2t(1, idc) .eq. nb) goto 190
              if (dc2t(3, idc) .eq. nb) goto 200
            enddo
            call erexit()
            goto 210

  190       pdc = dc2t(42, idc)
            qdc = dc2t(44, idc)
            vdc = dc2t(40, idc)
            pvlv = abs(pdc-0.001*dc2t(39, idc)*dc2t(40, idc))
            qvlv = qdc
            adc = dc2t(39, idc)
            goto 210

  200       pdc = dc2t(43, idc)
            qdc = dc2t(45, idc)
            vdc = dc2t(41, idc)
            pvlv = abs(pdc+0.001*dc2t(39, idc)*dc2t(41, idc))
            qvlv = qdc
            adc =  - dc2t(39, idc)
  210       continue

          endif
        else 
          if (ktype .eq. 8 .or. ktype .eq. 11) then

C           TYPE "BG" OR "BX" BUS

            kb = kbsdta(13, nb)
            if (kb .ne. 0) then
              if (ktype .ne. 11 .or. kb .ne. nb) then
                cbname = bus(kb)
                pctq = busdta(14, nb)
                ikb = inp2opt(kb)
                voltcb = dsqrt(e(ikb)**2+f(ikb)**2)*base(kb)
              else
                kb = 0
              endif
            endif
            if (ktype .eq. 11) then
              jxdta = busxdtptr(nb)
              if (jxdta .eq. 0) call erexit()
              skreak = xdata(3, jxdta)
              skcap = xdata(4, jxdta)
              userek = xdata(5, jxdta)
              usecap = xdata(6, jxdta)
              busdta(6, nb) = userek + usecap
              skreak = skreak*voltpu**2
              skcap = skcap*voltpu**2
            endif
          endif
          if (ktype .eq. 1 .or. ktype .eq. 4 .or. ktype .eq. 6 .or.
     &        ktype .eq. 10) then
            if (qmin .lt. 0.0) qmax = 0.0d0
            qmin = qmax
          endif

          jtbx = ptrtbx(nb)
          if (ktype .eq. 6 .or. ktype .eq. 7 .or. ktype .eq. 8 .or.
     &        ktype .eq. 9 .or. ktype .eq. 11 .or. ktype .eq. 13) then
            if (jtbx .eq. 0) then
              write (errbuf(1), 10188) bus(nb), base(nb), ktype
10188         format (' Bus ', a8, f6.1, ' subtype "', i2, 
     &       '" is missing TBX entity')
              call prterx('W', 1)
            else
              ltype = tbx(1, jtbx)
              itype = tbx(7, jtbx)

              if (ltype .ne. 2 .and. ltype .ne. 4) then
                if (ltype .eq. 3) goto 240
                if (ltype .eq. 5) goto 260
                if (ltype .eq. 6) goto 300
                if (itype .eq. 1 .or. itype .eq. 2) goto 310
                if (itype .eq. 3) goto 250
                if (itype .eq. 4) goto 290
              endif
              if (itype .eq. 1 .or. itype .eq. 2) goto 300
              if (itype .eq. 3 .or. itype .eq. 5) goto 280
              if (itype .eq. 4 .or. itype .eq. 6) goto 270
  240         if (itype .eq. 1) goto 300
              if (itype .eq. 2) goto 280
              if (itype .eq. 3) goto 270
              if (itype .eq. 4) then

                komnt = 'QPCT'
                goto 300
              elseif (itype .ne. 5) then
                if (itype .eq. 6) goto 290
                goto 260
              endif

  250         komnt = 'VMIN'
              goto 300
  260         if (itype .eq. 1) goto 300
              if (itype .ne. 3) then
                if (itype .ne. 4) goto 280

                komnt = 'QDIS'
                icapsw = 2
                goto 310
              endif

  270         komnt = 'QMAX'
              goto 310

  280         komnt = 'QMIN'
              goto 310

  290         komnt = 'VMAX'
  300         icapsw = 2
              goto 320
            endif
  310       kpq = 2
            goto 330
          endif
        endif
        icapsw = 2
  320   kpq = 1
  330   continue
        ploadm = pload
        qloadm = qload
        pgenmw = pgen
        qgenmw = qgen
        qsusp = sksusp

C       Initialize composite load quantities

        tottyp(1) = pload
        tottyp(2) = qload
        do i = 3, 6
          tottyp(i) = 0.0
        enddo

        ncb = kbsdta(15, nb)

C       Process continuation bus data

        do while (ncb .gt. 0)
          if (ktype .eq. BSTYP_BD .or. ktype .eq. BSTYP_BM) then
          endif
          pload2 = bctbl(2, ncb)
          qload2 = bctbl(3, ncb)
          skcon2 = bctbl(4, ncb)
          sksus2 = bctbl(5, ncb)
          pgen2 = bctbl(6, ncb)
          qgen2 = 0.0
          qmax2 = bctbl(11, ncb)
          qmin2 = bctbl(12, ncb)
          call getchr(1, kode2, kbctbl(8, ncb))
          call getchr(2, kodeyr, kbctbl(9, ncb))
          call getchr(3, kownr2, kbctbl(10, ncb))
          skcon2 = skcon2*voltpu*voltpu
          sksus2 = sksus2*voltpu*voltpu

C         Convert constant current and constant admittance loads back to
C         constant MVA.

          if (kode2 .eq. 'A') then
            if (kodeyr .eq. '01' .or. kodeyr .eq. '*I') then
              pload2 = pload2*voltpu
              qload2 = qload2*voltpu
              if (kownr2 .eq. '***') then
                gequiv = gequiv + skcon2
                bequiv = bequiv + sksus2
                skcon2 = 0.0
                sksus2 = 0.0
              else
                pload2 = pload2 + skcon2
                qload2 = qload2 - sksus2
                skcon2 = 0.0
                sksus2 = 0.0
              endif
            elseif (kodeyr .ne. '02' .and. kodeyr .ne. '*P') then
              bfixed = bfixed + sksus2
              sksus2 = 0.0
            elseif (kownr2 .eq. '***') then
              gequiv = gequiv + skcon2
              bequiv = bequiv + sksus2
              skcon2 = 0.0
              sksus2 = 0.0
            else
              pload2 = pload2 + skcon2
              qload2 = qload2 - sksus2
              skcon2 = 0.0
              sksus2 = 0.0
            endif
          elseif (kodeyr .eq. '*I') then
            pload2 = pload2*voltpu
            qload2 = qload2*voltpu
            if (kownr2 .eq. '***') then
              gequiv = gequiv + skcon2
              bequiv = bequiv + sksus2
              skcon2 = 0.0
              sksus2 = 0.0
            else
              pload2 = pload2 + skcon2
              qload2 = qload2 - sksus2
              skcon2 = 0.0
              sksus2 = 0.0
            endif
          elseif (kodeyr .eq. '*P') then
            if (kownr2 .eq. '***') then
              gequiv = gequiv + skcon2
              bequiv = bequiv + sksus2
              skcon2 = 0.0
              sksus2 = 0.0
            else
              pload2 = pload2 + skcon2
              qload2 = qload2 - sksus2
              skcon2 = 0.0
              sksus2 = 0.0
            endif
          endif
          skcap2 = amax1(0.0, sksus2)
          skrek2 = amin1(0.0, sksus2)
          qsusp = qsusp + skrek2 + skcap2
          pgenmw = pgenmw + pgen2
          qmax = qmax + qmax2
          qmin = qmin + qmin2
          ploadm = ploadm + pload2
          qloadm = qloadm + qload2
          skcond = skcond + skcon2
          skreak = skreak + skrek2
          skcap = skcap + skcap2
          sksusp = sksusp + sksus2
          ncb = bctbl_nxt(ncb)
        enddo

C       REMOVE "FICTICIOUS GENERATION" WHICH SIMULATES CONTINUOUS SHUNT
C       REACTIVE

        if (qsusp .ne. 0.0) then
          if (icapsw .ne. 1) then
            if (qgenx*qsusp .le. 0.0) then
              if (abs(qgenx) .le. abs(qsusp)) then
                qgenx = 0.0
              else
                qgenx = qgenx + qsusp
              endif
            endif
          endif
        endif
        qgenx=qgenx-ddim(dble(qgenx),qmax)+ddim(qmin,dble(qgenx))

C       QGEN is calculated from solution injection and SUM(+ bus)

        qgen = qgenx - qgenmw
        qgenmw = qgenx

C       ADJUST SLACK BUS

        if (koptsw .eq. 0) then
          iarea = 0
        else
          iarea = 0
          do i = 1, ntotc
            if (karea(1, i) .eq. nb) goto 340
          enddo
          goto 350
  340     iarea = i
        endif
  350   if (iarea .gt. 0) then
          padj = pgenx - pgenmw
          pgenmw = pgenmw + padj
          pgen = pgen + padj
          busdta(8, nb) = pgen
          area(8, iarea) = pgenmw
        elseif (ktype .eq. BSTYP_BS) then
          padj = pgenx - pgenmw
          pgenmw = pgenmw + padj
          pgen = pgen + padj
          busdta(8, nb) = pgen
        endif

C       ENCODE BUS QUANTITIES

        if (ktype .ne. BSTYP_BD .and. ktype .ne. BSTYP_BM) then
          write (outbuf, 10200) bsname, bsvolt, voltkv, ang, zname, 
     &     kowner, voltpu, type
10200     format (1x, a8, 2f7.1, 'KV/', a5, ' ZONE ', a2, 52x, a3, 9x, 
     &     f6.3, 'PU KV      BUS TYPE ', a1)
          iform = 2
          if (abs(pgen) .gt. 0.04) then
            write (outbuf(41:64), 10210) pgen, qgen
10210       format (f7.1, 'PGEN', f8.1, 'QGEN ')
          endif
          if (abs(qgen) .gt. 0.04) then
            write (outbuf(51:64), 10220) qgen
10220       format (f9.1, 'QCOND')
          endif
          write (outbuf(65:89), 10230) pload, qload
10230     format (f7.1, 'PLOAD', f8.1, 'QLOAD')

        elseif (ktype .eq. BSTYP_BM .and. dcmtbs(3, idc) .eq. 0) then
          write (outbuf, 10240) bsname, bsvolt, zname, vdc, kowner, 
     &     type
10240     format (1x, a8, f7.1, 15x, ' ZONE ', a2, f8.1, 'DKV', 40x, 
     &     a3, 16x, 'PASSIVE DC BUS TYPE ', a1)
        else
          write (outbuf, 10250) bsname, bsvolt, voltkv, ang, zname, 
     &     pdc, adc, pvlv, qvlv, kowner, voltpu, type
10250     format (1x, a8, 2f7.1, 'KV/', a5, ' ZONE ', a2, f8.1, 'PVLV', 
     &     f7.1, 'AMPS', f9.1, 'PLOSS', f8.1, 'QLOSS', 2x, a3, 9x, 
     &     f6.3, 'PU KV      BUS TYPE ', a1)
          call prtout(1)
          if (ktype .eq. BSTYP_BM) then
            namtyp = 'RECTIFIER'
            if (dcmtbs(19, idc) .lt. 9) namtyp = 'INVERTER'
            angle = 57.2957795*dcmtbs(13, idc)
            da = 0.0
            if (dcmtbs(9, idc) .gt. 0) da = 
     &         100.0 * ddim(dble(adc), dcmtbs(9, idc))/dcmtbs(9, idc)
            vdc = dcmtbs(20, idc)
          else
            da = 0.0
            if (dc2t(1, idc) .eq. nb) then
              angle = 57.2957795*dc2t(22, idc)
              namtyp = 'RECTIFIER'
              if (dc2t(14, idc) .gt. 0) da = 
     &          100.0 * ddim(dble(adc), dc2t(14,idc))/dc2t(14, idc)
              vdc = dc2t(40, idc)
            else
              angle = 57.2957795*dc2t(26, idc)
              namtyp = 'INVERTER'
              if (dc2t(17, idc) .gt. 0) da = 
     &           100.0 * ddim(dble(adc), dc2t(17,idc))/dc2t(17, idc)
              vdc = dc2t(41, idc)
            endif
          endif
          write (outbuf, 10260) vdc, angle, namtyp
10260     format (40x, f7.1, 'DKV', f8.1, 'DEG', 59x, a10)
          if (da .gt. 0.0) then
            write (outbuf(81:98), 10270) da
10270       format ('N', f9.1, ' % OVLD ')
          endif
        endif
        call prtout(1)
        if (newfrm(iframe)) then
          write (outbuf, 10280) bsname, bsvolt, zname
10280     format ('$mfie', 5x, a8, f6.1, ' KV ', a2)
          call pfomf(1)
        endif

        if ((ktype .eq. BSTYP_BX .and. kb .gt. 0) .or. 
     &      (ktype .eq. BSTYP_BG .and. kb .gt. 0)) then
          write (outbuf, 10290) cbname, base(kb), voltcb
10290     format (8x, a8, f7.1, '   CONTROLLED TO ', f7.1, 'KV')
          call prtout(1)
          iform = 1
        endif
        call bsanal()
        ncb = kbsdta(15, nb)
        do while (ncb .gt. 0)

C         Encode continuation bus data

          outbuf = ' '
          call getchr(1, kode, kbctbl(8, ncb))
          call getchr(2, kodeyr, kbctbl(9, ncb))
          call getchr(3, kowner, kbctbl(10, ncb))
          pload2 = bctbl(2, ncb)
          qload2 = bctbl(3, ncb)
          skcon2 = bctbl(4, ncb)
          sksus2 = bctbl(5, ncb)
          pgen2 = bctbl(6, ncb)
          qgen2 = 0.0
          qmax2 = bctbl(11, ncb)
          qmin2 = bctbl(12, ncb)
          skcon2 = skcon2*voltpu**2
          sksus2 = sksus2*voltpu**2

C         Convert constant current and constant admittance loads back to
C         constant MVA.

          do i = 1, 6
            lodtyp(i) = 0.0
          enddo
          if (kode .eq. 'A') then
            if (kodeyr .eq. '01' .or. kodeyr .eq. '*I') then
              pload2 = pload2*voltpu
              qload2 = qload2*voltpu
              lodtyp(3) = pload2
              lodtyp(4) = qload2
              if (kowner .eq. '***') then
                skcon2 = 0.0
                sksus2 = 0.0
              else
                lodtyp(5) = skcon2
                lodtyp(6) =  - sksus2
                pload2 = pload2 + skcon2
                qload2 = qload2 - sksus2
                skcon2 = 0.0
                sksus2 = 0.0
              endif
            elseif (kodeyr .eq. '02' .or. kodeyr .eq. '*P') then
              lodtyp(1) = pload2
              lodtyp(2) = qload2
              if (kowner .eq. '***') then
                skcon2 = 0.0
                sksus2 = 0.0
              else
                lodtyp(5) = skcon2
                lodtyp(6) =  - sksus2
                pload2 = pload2 + skcon2
                qload2 = qload2 - sksus2
                skcon2 = 0.0
                sksus2 = 0.0
              endif
            else
              lodtyp(1) = pload2
              lodtyp(2) = qload2
              sksus2 = 0.0
            endif
          elseif (kodeyr .eq. '*I') then
            pload2 = pload2*voltpu
            qload2 = qload2*voltpu
            lodtyp(3) = pload2
            lodtyp(4) = qload2
            if (kowner .eq. '***') then
              skcon2 = 0.0
              sksus2 = 0.0
            else
              lodtyp(5) = skcon2
              lodtyp(6) =  - sksus2
              pload2 = pload2 + skcon2
              qload2 = qload2 - sksus2
              skcon2 = 0.0
              sksus2 = 0.0
            endif
          elseif (kodeyr .eq. '*P') then
            lodtyp(1) = pload2
            lodtyp(2) = qload2
            if (kowner .eq. '***') then
              skcon2 = 0.0
              sksus2 = 0.0
            else
              lodtyp(5) = skcon2
              lodtyp(6) =  - sksus2
              pload2 = pload2 + skcon2
              qload2 = qload2 - sksus2
              skcon2 = 0.0
              sksus2 = 0.0
            endif
          else
            lodtyp(1) = pload2
            lodtyp(2) = qload2
          endif
          do i = 1, 6
            tottyp(i) = tottyp(i) + lodtyp(i)
          enddo

          skrek2 = amin1(0.0, sksus2)
          skcap2 = amax1(0.0, sksus2)

          if (iprint .ne. 1) then

            write (outbuf, 10300) kowner, kode, kodeyr
10300       format (t92, a3, t120, a3, 1x, a2)

            if (abs(pgen2)+abs(qgen2)+abs(pload2)+abs(qload2) .le. 
     &       0.04) then
              call prtout(1)
              outbuf = ' '
            else
              if (abs(pgen2)+abs(qgen2) .gt. 0.04) then
                if (iform .eq. 1) then
                  iform = 2
                  write (outbuf(41:63), 10310) pgen2, qgen2
10310             format (f7.1, 'PGEN', f8.1, 'QGEN')
                else
                  write (outbuf(41:59), 10320) pgen2, qgen2
10320             format (f7.1, 4x, f8.1)
                endif
              endif

              if (kodeyr .eq. '*I' .or. (kode .eq. 'A' .and. kodeyr 
     &         .eq. '01')) then
                if (abs(lodtyp(3))+abs(lodtyp(4)) .gt. 0.04) then
                  if (iform .eq. 1) then
                    iform = 2
                    write (outbuf(65:89), 10330) lodtyp(3), lodtyp(4)
10330               format (f7.1, 'PLOAD', f8.1, 'QLOAD')
                    outbuf(127:129) = '(I)'
                    call prtout(1)
                    outbuf = ' '
                  else
                    write (outbuf(61:84), 10340) lodtyp(3), lodtyp(4)
10340               format (4x, f7.1, 5x, f8.1)
                    outbuf(127:129) = '(I)'
                    call prtout(1)
                    outbuf = ' '
                  endif
                endif
                if (abs(lodtyp(5))+abs(lodtyp(6)) .gt. 0.04) then
                  if (outbuf .eq. ' ') write (outbuf, 10300) kowner, 
     &             kode, kodeyr
                  if (iform .eq. 1) then
                    iform = 2
                    write (outbuf(65:89), 10330) lodtyp(5), lodtyp(6)
                    outbuf(127:129) = '(Z)'
                    call prtout(1)
                    outbuf = ' '
                  else
                    write (outbuf(61:84), 10340) lodtyp(5), lodtyp(6)
                    outbuf(127:129) = '(Z)'
                    call prtout(1)
                    outbuf = ' '
                  endif
                endif
              elseif (kodeyr .eq. '*P' .or. (kode .eq. 'A' .and. kodeyr 
     &         .eq. '02')) then
                if (abs(lodtyp(1))+abs(lodtyp(1)) .gt. 0.04) then
                  if (iform .eq. 1) then
                    iform = 2
                    write (outbuf(65:89), 10330) lodtyp(1), lodtyp(2)
                    outbuf(127:129) = '(P)'
                    call prtout(1)
                    outbuf = ' '
                  else
                    write (outbuf(61:84), 10340) lodtyp(1), lodtyp(2)
                    outbuf(127:129) = '(P)'
                    call prtout(1)
                    outbuf = ' '
                  endif
                endif
                if (abs(lodtyp(5))+abs(lodtyp(6)) .gt. 0.04) then
                  if (outbuf .eq. ' ') write (outbuf, 10300) kowner, 
     &             kode, kodeyr
                  if (iform .eq. 1) then
                    iform = 2
                    write (outbuf(65:89), 10330) lodtyp(5), lodtyp(6)
                    outbuf(127:129) = '(Z)'
                    call prtout(1)
                    outbuf = ' '
                  else
                    write (outbuf(61:84), 10340) lodtyp(5), lodtyp(6)
                    outbuf(127:129) = '(Z)'
                    call prtout(1)
                    outbuf = ' '
                  endif
                endif
              elseif (abs(lodtyp(1))+abs(lodtyp(1)) .gt. 0.04) then
                if (iform .eq. 1) then
                  iform = 2
                  write (outbuf(65:89), 10330) lodtyp(1), lodtyp(2)
                  call prtout(1)
                  outbuf = ' '
                else
                  write (outbuf(61:84), 10340) lodtyp(1), lodtyp(2)
                  call prtout(1)
                  outbuf = ' '
                endif
              endif

              if (outbuf .ne. ' ') call prtout(1)
            endif
          endif

          call cbanal()

          ncb = bctbl_nxt(ncb)
        enddo

        if (nztot .gt. 0) call znbsum(zname)
        systot(1) = systot(1) + pgenmw
        systot(2) = systot(2) + qgenmw
        systot(3) = systot(3) - ploadm
        systot(4) = systot(4) - qloadm
        systot(13) = systot(13) - pvlv
        systot(14) = systot(14) - qvlv

C       BEGIN BRANCHES for this bus



C       ********************************************************
C       1st bus rcd printed, begin printing connected branches *
C       ( Transpose data as needed when qptr is negitive )   *
C       v
        nxtybr = 1
        iform = 1
        ktrot = 1
        pintot = 0.0
        qintot = 0.0d0
        poutot = 0.0
        qoutot = 0.0
        pltot = 0.0
        qltot = 0.0

        ptr = kbsdta(16, nb)

        do while (ptr .gt. 0)
          qptr = brnch_ptr(ptr)
          nbr = iabs(qptr)
          jbr = nbr	! save index to branch data table (anal)
          mbptr = ptr	! save pointer to main branch (anal)
          k1 = kx(ptr)
          k2 = ky(ptr)
          id = brid(ptr)
          kt = inp2opt(k1)
          mt = inp2opt(k2)
          call getchr(3, kowner, kbrnch(3, nbr))
          isect = brsect(ptr)
          ltype = brtype(ptr)
          oldptr = ptr
          if (ltype .eq. BRTYP_LM .or. ltype .eq. BRTYP_R .or. ltype 
     &     .eq. BRTYP_LD) then
            isect = 0
            id = ' '
          endif
          if ((ltype .eq. BRTYP_R .and. kauto .eq. 0) .or. ltype .eq. 
     &     BRTYP_RZ) then

C           Process next record

            ptr = brnch_nxt(ptr)
            do while (ptr .gt. 0 .and. ((brtype(ptr) .eq. BRTYP_R .and. 
     &       kauto .eq. 2) .or. brtype(ptr) .eq. BRTYP_RZ))
              ptr = brnch_nxt(ptr)
            enddo
          else

            ktybr = nxtybr
            if (k1 .ne. nb) call erexit()

C           1    2    3    4    5    6    7    8
C           SECT   LM    L    R    T   TP   LD    E
            if (ltype .ne. 1) then
              if (ltype .eq. 3) then


                amiles = brnch(9, nbr)
              elseif (ltype .eq. 4) then

                ktrot = 2
                kcb = kbrnch(4, nbr)
                tmax = brnch(6, nbr) - 0.05
                tmin = brnch(7, nbr) + 0.05
                taps = brnch(8, nbr)
                skmax = brnch(9, nbr)
                skmin = brnch(10, nbr)
                call getchr(1, rtype, kbrnch(3, nbr))

C               Determine if tap limits hit.

                nptr = brnch_nxt(ptr)
                loc = orienttx(ptr, nptr, k1x, k2x, tap1, tap2)
                nqptr = brnch_ptr(nptr)
                nxnbr = iabs(nqptr)
                if (brtype(nptr) .eq. 6) then
                  if ((loc .eq. 1 .and. nqptr .lt. 0) .or. (loc .eq. 2 
     &             .and. nqptr .gt. 0)) then
                    temp = tmax
                    tmax =  - tmin
                    tmin =  - temp
                  endif
                  trx = tap1
                else
                  trx = tap2
                endif

                dtap = dim(trx, tmax) - dim(tmin, trx)
                if (dtap .lt. 0.0) then
                  atrx2 = 'MIN'
                elseif (dtap .gt. 0.0) then
                  atrx2 = 'MAX'
                else
                  atrx2 = '   '
                endif
                atrx1 = 'LTC'

C               Process next record

                ptr = brnch_nxt(ptr)
                do while (ptr .gt. 0 .and. ((brtype(ptr) .eq. BRTYP_R 
     &           .and. kauto .eq. 2) .or. brtype(ptr) .eq. BRTYP_RZ))
                  ptr = brnch_nxt(ptr)
                enddo
                goto 430
              else
                if (ltype .ne. 5) then
                  if (ltype .ne. 6) then
                    if (ltype .eq. 8) then

                      amiles = 0
                      goto 370
                    else

C                     FIND DC INDEX

                      if (ktybr .ne. 1) then
                        write (errbuf(1), 10350)
10350                   format ('0 DC LINE HAS ILLEGAL PARALLELS')
                        call bcdbrn(nbr, xbuf)
                        write (errbuf(2), 10400) xbuf(1:80)
                        call prterx('W', 2)
                      endif

                      isect = 0
                      k1x = min0(k1, k2)
                      k2x = max0(k1, k2)
                      if (ltype .eq. BRTYP_LM) then
                        do jdc = 1, mtdcln
                          l1x = dmin1 (dcmtln(1, jdc), dcmtln(2, jdc))
                          l2x = dmax1 (dcmtln(1, jdc), dcmtln(2, jdc))
                          if (k1x .eq. l1x .and. k2x .eq. l2x) goto 360
                        enddo
                        call erexit()
                      else
                        jdc = idc
                      endif

  360                 amiles = brnch(16, nbr)
                      parl = 1.0
                      rating = brnch(4, nbr)
                      rattag = 'N'
                      intor = gtmetpnt(ptr)
                      iparl = 1
                      goto 390
                    endif
                  endif
                endif
                if (ktrot .eq. 1) then
                  atrx1 = '   '
                  atrx2 = '   '
                endif
                if (brtype(ptr) .eq. BRTYP_T) then
                  if (qptr .gt. 0) then
                    trx1 = brnch(9, nbr)
                    trx2 = brnch(10, nbr)
                  else
                    trx2 = brnch(9, nbr)
                    trx1 = brnch(10, nbr)
                  endif
                elseif (qptr .gt. 0) then
                  trx1 = brnch(9, nbr)
                  trx2 = brnch(10, nbr)
                else
                  trx1 =  - brnch(9, nbr)
                  trx2 = brnch(10, nbr)
                endif
                goto 380
              endif

  370         atrx1 = '   '
              atrx2 = '   '
            endif
  380       call getchr(3, kowner, kbrnch(3, nbr))
            intor = gtmetpnt(ptr)

C           Get Nominal/Extended ratings

            call getrat(ptr, rating, rattag, ratnom, ratthr, ratllf, 
     &       ratbtl)
            parl = brnch(16, nbr)
            iparl = amax1(parl, 1.0)

            ktrpos = 0
            if (qptr .lt. 0) ktrpos = 1       ! transpose it..
            if (ltype .eq. BRTYP_PEQ) then
              if (qptr .gt. 0) then
C               inp direction
                y(1, 1) = cmplx(brnch(4, nbr), brnch(5, nbr))
                y(1, 2) = cmplx(brnch(6, nbr), brnch(7, nbr))
                y(2, 1) = cmplx(brnch(8, nbr), brnch(9, nbr))
                y(2, 2) = cmplx(brnch(10, nbr), brnch(11, nbr))
              else
C               transposed direction
                y(2, 2) = cmplx(brnch(4, nbr), brnch(5, nbr))
                y(2, 1) = cmplx(brnch(6, nbr), brnch(7, nbr))
                y(1, 2) = cmplx(brnch(8, nbr), brnch(9, nbr))
                y(1, 1) = cmplx(brnch(10, nbr), brnch(11, nbr))
              endif
              rating = 0.0
              x = 0
              iparl = 1
              call chkmet(k1, k2, id)
            else
              call pieqiv(ptr, y, kerr)
              x = brnch(6, nbr)
            endif

C           Look ahead (at next branch)

  390       ptr = brnch_nxt(ptr)
            do while (ptr .gt. 0 .and. ((brtype(ptr) .eq. BRTYP_R .and. 
     &       kauto .eq. 0) .or. brtype(ptr) .eq. BRTYP_RZ))
              ptr = brnch_nxt(ptr)
            enddo
            if (ptr .eq. 0) then
              nxk1 = 0
              nxk2 = 0
            else
              qptr = brnch_ptr(ptr)
              nbr = iabs(qptr)
              nxk1 = kx(ptr)
              nxk2 = ky(ptr)
              nxid = brid(ptr)
              nxsect = brsect(ptr)
              if (nxk1 .ne. k1) nxk2 = 0
              if (brtype(ptr) .eq. BRTYP_LM .or. brtype(ptr) .eq. 
     &         BRTYP_LD) nxsect = 0

            endif
            if (ktybr .eq. 1) then
              nbpar = 1
              nbsec = 0
              nbline = iparl
            elseif (ktybr .eq. 2) then
              nbpar = nbpar + 1
              nbsec = 0
              nbline = nbline + iparl
            elseif (ktybr .eq. 3) then

C             Processing sections

              nbline = iparl

              if (ltype .eq. 2 .or. ltype .eq. 4 .or. ltype .eq. 7) 
     &         then

                write (errbuf(1), 10360)
10360           format ('0 ILLEGAL BRANCH TYPE WITH A SECTION.')
                call bcdbrn(nbr, xbuf)
                write (errbuf(2), 10400) xbuf(1:80)
                call prterx('W', 2)
              elseif (ltype .eq. 3 .or. ltype .eq. 5 .or. ltype .eq. 6 
     &         .or. ltype .eq. 8) then

                nbsec = nbsec + 1
                base1 = base2
                if (ltype .eq. BRTYP_T) base2 = base(k2)
                if ((ltype .eq. BRTYP_TP) .and. (brnch(10, nbr) .ne. 
     &           0.0)) base2 = base(k2)

C               Check for last section

                if (k1 .ne. nxk1 .or. k2 .ne. nxk2) then
                  nxtybr = 1
                elseif (id .eq. nxid) then
                  nxtybr = 3
                else
                  nxtybr = 2
                endif

                call pqsect(oldptr, nxtybr)
                if (nbline .eq. 1) nbline = 0

                if (x .lt. 0.0) then
                  if (xtotal .gt. 0.0) then
                    pctcp =  - 100.0*x/xtotal
                  else
                    pctcp = 100.0
                  endif
                  if (ktrpos .eq. 0) call cpanal(pctcp)
                endif

                if (iprint .ne. 1) then
                  if (iform .eq. 1) then
                    write (outbuf, 10410) nbline, bus(k2), base(k2), 
     &               id, isect, zone(k2), pin, qin, ploss, qloss, 
     &               kowner
                    iform = 2
                  else
                    write (outbuf, 10440) nbline, bus(k2), base(k2), 
     &               id, isect, zone(k2), pin, qin, ploss, qloss, 
     &               kowner
                  endif
                  if (ltype .eq. BRTYP_T) then
                    if (trx1 .lt. 1000.0) then
                      write (tmwrd1, 10490) trx1
                    else
                      write (tmwrd1, 10500) trx1
                    endif

                    if (trx2 .lt. 1000.0) then
                      write (tmwrd2, 10490) trx2
                    else
                      write (tmwrd2, 10500) trx2
                    endif
                    write (outbuf(98:117), 10510) atrx1, tmwrd1, 
     &               tmwrd2, atrx2

                  elseif (ltype .eq. BRTYP_TP) then
                    if (trx1 .le. -99.9) then
                      write (ang, 10460) trx1
                    else
                      write (ang, 10470) trx1
                    endif
                    write (outbuf(98:117), 10480) atrx1, ang, atrx2
                  endif


C                 PRINT OUT COMPENSATION


                  if (x .lt. 0.0) then
                    ipctcp = pctcp + 0.5
                    write (outbuf(104:113), 10370) ipctcp
10370               format (i4, '% COMP')
                  endif

C                 PRINT OUT OVERLOAD

                  if (pctol .ne. 0.0) then
                    ipctol = pctol + 0.5
                    if (ipctol .gt. 9999) ipctol = 9999
                    write (outbuf(119:133), 10450) rattag, ovld(2:), 
     &               ipctol
                  endif

                  call prtout(1)

C                 PRINT OUT PASSIVE NODES


                  if (nxtybr .eq. 3) then
                    em = dreal(v(2))
                    fm = dimag(v(2))
                    emag = cdabs(v(2))*base2
                    degree = 57.2957795*atan2(fm, em) + sang
                    write (outbuf, 10380) emag, degree
10380               format (53x, f6.1, 'KV', f10.1, 'DEGREE')
                    call prtout(1)
                  endif
                endif
              else

                write (errbuf(1), 10390)
10390           format ('0 DUPLICATE "L*" RECORDS')
                call bcdbrn(nbr-1, xbuf)
                write (errbuf(2), 10400) xbuf(1:80)
10400           format (2x, '(', a80, ')')
                call bcdbrn(nbr, xbuf)
                write (errbuf(3), 10400) xbuf(1:80)
                call prterx('W', 3)
              endif
              goto 410
            endif
            if (nbline .eq. 1) nbline = 0
            if (ltype .eq. BRTYP_PEQ) then
              base1 = base(k1)
              base2 = base1
            else
              base1 = base(k1)
              base2 = base(k2)
            endif
            call pqparl(oldptr, 0)
            if (idebug .ne. 0) call chkbrn()
            if (ltype .eq. BRTYP_PEQ) then

              nxtybr = 3
            elseif (ltype .eq. BRTYP_LD .or. ltype .eq. BRTYP_LM) then
              goto 400
            elseif (k2 .ne. nxk2 .and. ktybr .eq. 1) then
              goto 400
            else
              nxtybr = 2
            endif

            if (ltype .eq. BRTYP_PEQ) then

C             Look ahead to determine XTOTAL

              nsect = 0
              xtotal = 0.0
              nbsec = 0
              jptr = ptr
              do while (jptr .gt. 0 .and. (ky(jptr) .eq. k2 .and. brid
     &         (jptr) .eq. id))
                if (brtype(jptr) .ne. BRTYP_RZ) then
                  nsect = nsect + 1
                  qptr = brnch_ptr(jptr)
                  j = iabs(qptr)
                  xtotal = xtotal + amax1(0.0, brnch(6, j))
                endif
                jptr = brnch_nxt(jptr)
              enddo
              goto 410
            else
              nbline = iparl
              if (nbline .eq. 1) nbline = 0
            endif
  400       if (iprint .ne. 1) then
              if (iform .eq. 1) then
                if (ltype .ne. BRTYP_LM .and. ltype .ne. BRTYP_LD) then


C                 ENCODE AC LINE/TRANSFORMER QUANTITIES

                  write (outbuf, 10410) nbline, bus(k2), base(k2), id, 
     &             isect, zone(k2), pin, qin, ploss, qloss, kowner
10410             format (5x, i2.0, 1x, a8, f7.1, 3x, a1, i2.0, 8x, a2, 
     &             f8.1, 'PIN', f9.1, 'QIN', f9.1, 'PLOSS', f8.1, 
     &             'QLOSS', 2x, a3)
                  iform = 2
                else

C                 ENCODE DC LINE QUANTITIES

                  write (outbuf, 10420) nbline, bus(k2), base(k2), id, 
     &             isect, zone(k2), pin, ploss, kowner
10420             format (5x, i2.0, 1x, a8, f7.1, 3x, a1, i2.0, 8x, a2, 
     &             f8.1, 'PIN', 12x, f9.1, 'PLOSS', 15x, a3)
                  iform = 2
                endif
              elseif (ltype .eq. BRTYP_LM .or. ltype .eq. BRTYP_LD) 
     &         then

C               ENCODE AC LINE/TRANSFORMER QUANTITIES(ALTERNATE FORMAT)

                write (outbuf, 10430) nbline, bus(k2), base(k2), id, 
     &           isect, zone(k2), pin, ploss, kowner
10430           format (5x, i2.0, 1x, a8, f7.1, 3x, a1, i2.0, 8x, a2, 
     &           f8.1, 12x, f12.1, 20x, a3)
              else

C               ENCODE DC LINE QUANTITIES (ALTERNATE FORMAT)

                write (outbuf, 10440) nbline, bus(k2), base(k2), id, 
     &           isect, zone(k2), pin, qin, ploss, qloss, kowner
10440           format (5x, i2.0, 1x, a8, f7.1, 3x, a1, i2.0, 8x, a2, 
     &           f8.1, f12.1, f12.1, f13.1, 7x, a3)
              endif
              if (pctol .gt. 0.0) then
                ipctol = pctol + 0.5
                if (ipctol .gt. 9999) ipctol = 9999
                write (outbuf(119:133), 10450) rattag, ovld(2:), ipctol
10450           format (a1, a7, i4.0, '%')
              endif
              if (ltype .eq. BRTYP_TP) then
                if (trx1 .gt. -99.9) then
                  write (ang, 10460) trx1
10460             format (f5.1)
                else
                  write (ang, 10470) trx1
10470             format (f5.0)
                endif
                write (outbuf(98:117), 10480) atrx1, ang, atrx2
10480           format (a5, a5, 1x, a3, ' DEGPH')
              elseif (ltype .eq. BRTYP_T) then
                if (trx1 .lt. 1000.0) then
                  write (tmwrd1, 10490) trx1
10490             format (f5.1)
                else
                  write (tmwrd1, 10500) trx1
10500             format (f5.0)
                endif
                if (trx2 .lt. 1000.0) then
                  write (tmwrd2, 10490) trx2
                else
                  write (tmwrd2, 10500) trx2
                endif

                write (outbuf(98:117), 10510) atrx1, tmwrd1, tmwrd2, 
     &           atrx2
10510           format (a5, a5, '/', a5, 1x, a3)
              endif
              call prtout(1)
            endif

C           Determine type of next branch

  410       do while (ptr .ne. 0)
              if (k1 .ne. kx(ptr)) goto 430
              if (k2 .eq. ky(ptr)) then
                nxid = brid(ptr)
                if (id .eq. nxid) then
                  nxtybr = 3
                else

                  nxtybr = 2
                endif
              else

                nxtybr = 1
                ktrot = 1
              endif
              if ((brtype(ptr) .ne. BRTYP_R .or. kauto .ne. 1) .and.
     &             brtype(ptr) .ne. BRTYP_RZ) goto 420
              ptr = brnch_nxt(ptr)
              qptr = brnch_ptr(ptr)
              nbr = iabs(qptr)
            enddo
            goto 430
  420       if (ktrot .eq. 2 .and. nxtybr .eq. 1) ktrot = 1
          endif

  430     continue
        enddo

C       Branch loop completed - determine reactive allocation

        qlim(3) = bequiv + bfixed - qloadm - qvlv
        qlim(2) = qlim(3) + skcap
        qlim(1) = qlim(2) + qmax
        qlim(4) = qlim(3) + skreak
        qlim(5) = qlim(4) + qmin
        qcap = amax1(0.0, bfixed)
        qreak = amin1(0.0, bfixed)
        qt = skcap + qcap + skreak + qreak

        do j = 1, 3
          dq = ddim(qintot, dble(qlim(j)))
          if (dq .gt. 0.0) goto 440
        enddo

        do j = 5, 3, -1
          dq =  - ddim(-qintot, -dble(qlim(j)))
          if (dq .lt. 0) goto 440
        enddo
        j = 3

  440   if (j .eq. 2) then

C         QGEN > 0

          qunsk = 0
          qcap = qcap + skcap
          qgenmw = dq
          dq = skreak
        elseif (j .eq. 3) then

C         QCAP or QREAK

          qgenmw = 0
          qunsk = 0
          if (qintot .gt. qlim(3)) then

C           QCAP

            qcap = qcap + dq
            dq = skcap + skreak - dq
          else

C           QREAK

            qreak = qreak + dq
            dq = skcap + skreak - dq
          endif
        elseif (j .eq. 4) then

C         QGEN < 0

          qunsk = 0
          qreak = qreak + skreak
          qgenmw = dq
          dq = skcap
        elseif (j .eq. 5) then

C         QUNSK < 0

          qunsk = dq
          dq =  - dq
          qt = qt - dq
          qreak = qreak + skreak
          qgenmw = qmin
        else

C         QUNSK > 0

          qunsk = dq
          dq =  - dq
          qt = qt - dq
          qcap = qcap + skcap
          qgenmw = qmax
        endif

        skcap = skcap + amax1(0.0, bfixed)
        skreak = skreak + amin1(0.0, bfixed)
        if (ktype .eq. BSTYP_BX) dq = 0.0
        if (icapsw .eq. 1) dq = 0.0
        systot(8) = systot(8) + qunsk
        dq = dq/(voltpu*voltpu*bmva)
        qt = qt/(voltpu**2*bmva)
        capcor(1, kt) = dq
        capcor(2, kt) = qt
        outbuf = ' '
        if (iprint .eq. 2) then
          if (abs(skcond) .ge. .05) write (outbuf(42:52), 10520) skcond
10520     format (f6.1, 'CNDK ')
          if (abs(skcap) .ge. 0.05) then
            if (abs(qcap) .ge. 0.05) write (outbuf(53:64), 10530) qcap
10530       format (f7.1, 'QUSED')
            if (abs(skcap) .ge. 0.05) write (outbuf(77:99), 10540) 
     &       skcap
10540       format (f8.1, 'QCAP SCHEDULED')
            if (outbuf .ne. '  ') then
              call prtout(1)
              outbuf = ' '
            endif
          endif
          if (abs(skreak) .gt. 0.05) then
            if (abs(qreak) .ge. 0.05) write (outbuf(53:64), 10530) 
     &       qreak
            if (abs(skreak) .ge. 0.05) write (outbuf(77:99), 10550) 
     &       skreak
10550       format (f8.1, 'QIND SCHEDULED')
            if (outbuf .ne. '  ') then
              call prtout(1)
              outbuf = ' '
            endif
          endif
        endif
        if (bequiv .ne. 0.0 .or. gequiv .ne. 0.0) then
          if (iprint .eq. 2) then
            write (outbuf, 10560) gequiv, bequiv
10560       format (8x, 'EQUIVALENT SHUNT', 15x, f8.1, 'CNDK', f8.1, 
     &       'SUSP')
            call prtout(1)
          endif
        endif

        pnetmw = pintot + pvlv
        qnetmw = qintot + qvlv
        pinsum = pinsum + pintot
        qinsum = qinsum + qintot
        poutsm = poutsm + poutot
        qoutsm = qoutsm + qoutot
        plssum = plssum + pltot
        qlssum = qlssum + qltot
        systot(5) = systot(5) - skcond
        systot(6) = systot(6) + qcap + qreak
        systot(9) = systot(9) - gequiv
        systot(10) = systot(10) + bequiv
        systot(11) = systot(11) - 0.5*pltot
        systot(12) = systot(12) - 0.5*qltot

        if (iprint .ne. 1) then
          write (outbuf, 10570) pnetmw, qnetmw, komnt
10570     format (40x, f7.1, 'PNET', f8.1, 'QNET', 57x, a4)
          if (komnt .ne. '    ') iprnet = 1
        endif

        if (kpq .ne. 2) then
          if (iprint .eq. 1) goto 450
          if (abs(qunsk) .gt. cutoff) then
            write (outbuf(101:119), 10580) qunsk
10580       format (f7.1, ' MVAR UNSKED')
          endif
        endif

        if (ktype .eq. BSTYP_BS) then
          if (outbuf(101:119) .ne. ' ') then
            call prtout(1)
            outbuf = ' '
          endif
          write (outbuf(101:120), 10590) padj
10590     format (f7.1, ' SLACK ADJ ')
        elseif (iarea .ne. 0) then
          if (outbuf(101:119) .ne. ' ') then
            call prtout(1)
            outbuf = ' '
          endif
          write (outbuf(101:120), 10600) padj
10600     format (f7.1, ' MW AREA ADJ')
          iprnet = 1
        endif

  450   if (ktype .eq. BSTYP_BE .or. ktype .eq. BSTYP_BS .or. ktype 
     &   .eq. BSTYP_BV) then
C         *** removed bus types JKL ***
C         &    ktype .eq. BSTYP_BV .or. ktype .eq. BSTYP_BK) then
          qadj = qunsk
        else
          qadj = 0.0
        endif
        perror = pnetmw - pgenmw + ploadm + skcond + gequiv
        qerror = qnetmw - qgenmw + qloadm - qcap - qreak - bequiv - 
     &   qadj
        if (abs(perror) .gt. cutoff .or. abs(qerror) .gt. cutoff) then

          if (iprint .eq. 1) then
            write (outbuf, 10610) bsname, bsvolt, voltkv, ang, zname, 
     &       perror, qerror
10610       format ('0', a8, 2f7.1, 'KV/', a5, ' ZONE ', a2, 25x, f7.1, 
     &       'PERR', f9.1, 'QERR')
            call prtout(1)
          else
            write (outbuf(65:88), 10620) perror, qerror
10620       format (f7.1, 'PERR', f9.1, 'QERR')
            iprnet = 1

          endif
        endif

        if (iprint .ne. 1 .and. iprnet .ne. 0) call prtout(1)
        if (lskp .eq. 1 .or. lskp .eq. 2) then
          if (abs(perror) .gt. cutoff .or. 
     &        abs(qerror) .gt. 5.0 * cutoff) then

            write (errbuf(1), 10630) bus(nb), base(nb), zone(nb), 
     &       perror, qerror
10630       format ('0 BUS ', a8, f6.1, ' ZONE ', a2, ' OUTPUT ERROR:', 
     &       2f8.1)
            call prterx('W', 1)
          endif
        endif
        call fianal()
        if (nztot .gt. 0) call znbsuf()
      enddo

  460 continue
      plssum = 0.5*plssum
      qlssum = 0.5*qlssum
      lprtsw = lprtsx
      fichsw = fichsx
      write (outbuf, 10640)
      call prtout(1)
      write (outbuf, 10650) pinsum, qinsum, poutsm, qoutsm, plssum, 
     & qlssum
      call prtout(1)
10640 format ('0 OUTPUT CHECK  ---  LINE EXPORT SUMS     ', 
     & 'LINE IMPORT SUMS         LINE LOSSES ')
10650 format ('0', 17x, 2f9.1, 3x, 2f9.1, 3x, 2f9.1)
      pltot = 0
      qltot = 0
      do i = 1, 13, 2
        pltot = pltot + systot(i)
        qltot = qltot + systot(i+1)
      enddo
      write (outbuf, 10660)
10660 format ('0', 40x, 'SUMMARY OF SYSTEM TOTALS', 
     & '             (MW)    (MVAR) ')
      call prtout(1)
      call space(1)
      pinsum = 0
      qinsum = 0
      do i = 1, 7, 2
        pinsum = pinsum + systot(i)
        qinsum = qinsum + systot(i+1)
      enddo
      write (outbuf, 10670)
10670 format (41x, 'SYSTEM INJECTION:              ')
      call prtout(1)
      write (outbuf, 10680) systot(1), systot(2)
10680 format (41x, '   GENERATION                  ', 2f10.1)
      call prtout(1)
      write (outbuf, 10690) systot(3), systot(4)
10690 format (41x, '   LOAD                        ', 2f10.1)
      call prtout(1)
      write (outbuf, 10700) systot(5), systot(6)
10700 format (41x, '   BUS SHUNT ADMITTANCE        ', 2f10.1)
      call prtout(1)
      write (outbuf, 10710) systot(7), systot(8)
10710 format (41x, '   UNSCHEDULED SOURCES         ', 2f10.1)
      call prtout(1)
      write (outbuf, 10720) pinsum, qinsum
10720 format (41x, 'SUBTOTAL (INJECTION)           ', 2f10.1)
      call prtout(1)
      write (outbuf, 10730)
10730 format (41x, 'SYSTEM LOSSES:                 ')
      call prtout(1)
      write (outbuf, 10740) systot(9), systot(10)
10740 format (41x, '    EQUIVALENT SHUNT ADMITTANCE', 2f10.1)
      call prtout(1)
      write (outbuf, 10750) systot(11), systot(12)
10750 format (41x, '    LINE AND TRANSFORMER LOSSES', 2f10.1)
      call prtout(1)
      write (outbuf, 10760) systot(13), systot(14)
10760 format (41x, '    DC CONVERTER LOSSES        ', 2f10.1)
      call prtout(1)
      plssum = systot(9) + systot(11) + systot(13)
      qlssum = systot(10) + systot(12) + systot(14)
      write (outbuf, 10770) plssum, qlssum
10770 format (41x, 'SUBTOTAL (LOSSES)              ', 2f10.1)
      call prtout(1)
      call space(1)
      write (outbuf, 10780) pltot, qltot
10780 format (41x, 'NET SYSTEM EXPORT:             ', 2f10.1)
      call prtout(1)
      if (idebug .ne. 0) call fibrck()
      end
