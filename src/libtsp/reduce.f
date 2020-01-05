C    %W% %G%
      subroutine reduce

C     THIS SUBROUTINE USES GAUSSIAN ELIMINATION AND SPARSITY
C     TECHNIQUES TO OBTAIN THE TABLE OF FACTORS FOR THE
C     NETWORK ADMITTANCE MATRIX. IT IS CALLED BY CNTRL.

      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/matrow.inc'
      include 'tspinc/ecstbg.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/toler.inc'
      include 'tspinc/param.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/search.inc'
      include 'tspinc/lnk12.inc'
      include 'tspinc/lnk12a.inc'
      include 'tspinc/link2.inc'
      include 'tspinc/lnk1a.inc'
      include 'tspinc/lnk1c.inc'
      include 'tspinc/lnk2c.inc'
      include 'tspinc/lnkcd.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/rk.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/param1.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/equiv.inc'
      include 'tspinc/brake1.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/ecstbc.inc'
      include 'tspinc/ecstbd.inc'
      include 'tspinc/ecstbh.inc'
      include 'tspinc/fltopt.inc'
      include 'tspinc/newton.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/yfactr.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/ecion.inc'

      common /mytr/mytr

      dimension gec(150), bec(150), locs(50), iused(200), loc(400), 
     &          grow(400), brow(400), gdum(50), bdum(50), 
     &          tstr2(MAXBUS), tstr3(MAXBUS), cgen(1)
      equivalence (kptbl1, igecse), (kgtri, kmtecg), (kbtri, kmtecb), 
     &            (locs, locl), (nsize, nmx), (lens, idell), (loclen, 
     &            idelh), (tstr2, rdiar), (tstr3, rdiax)

      double precision gdum, bdum, grow, brow, gphse, bphse, sqrcp, 
     &                 drecpg, drecpb, gec, bec, pmsq, gaid1, baid1, 
     &                 gpmr, bpmr, gpm, bpm

C     Begin     Begin     Begin     Begin     Begin     Begin

      call mpost('REDUCE')
      iseq = 3
      indxm5 = indxm*5
      nmxm44 = nmxm4*4
      if (nchck .eq. 3) then
        kdelcr = igecse
        kmtecg = 1
        kmtecb = kmtecg + 5*nmx
        kend = kmtecb + 5*nmx
        kend1 = keyri + kecsym + 1000
        if (nmx .le. 50) then
          kmtecg = 1
          kmtecb = kmtecg + (24*nmx)
          kend = kmtecb + (24*nmx)
        endif
      endif
      if (.not. (matsw .eq. 1 .or. matsw .eq. 2)) then
        if (matsw .ne. 3) goto 130

        call matmod()
        if (keybrd(0) .ne. 0) then
          write (outbuf, 10000)
10000     format ('0', 5x, 'MATMOD-LFRST =', i5)
          call prtout(1)
        endif
        icntl2 = 1

C       CHECK FOR NO NETWORK REDUCTION FLAG

        if (inewts .eq. 1 .and. nordk .eq. 0) goto 380
      endif

C     YI solution, Newton solution option

C     GET LOWEST BUS NO. FOR START OF REDUCTION

      if (lcompf .ne. 0) lfrst = min0(lfrst, lcompf)

C     K IF PRESENT OR PREVIOUS FACTORIZING WITH SHORT CKT.

      if (ifltsw .ne. 1) then
        if (iesc .eq. 3) lfrst = min0(lfrst, ifltb)
      endif
      if (keybrd(0) .ne. 0) then
        write (outbuf, 10010) tsim, iesc
10010   format (1x, f10.1, ' cycle ', i5, ' iesc')
        call prtout(1)
      endif
      goto 370

  100 iseq = 2
  110 iseqsw = 1

C     INITIALIZE TEMP STORAGE
      do i = 1, nmx
        tstr2(i) = 0.0
        tstr3(i) = 0.0
      enddo
      if (jmx .eq. nmx) then
        if (lt .ne. lfix) then
          read (lt) i
          jmx = i - 1
          backspace lt
        endif
      endif
      rewind lfix

  120 if (ipwr .ne. 1) then
        read (lfix) dummy
        read (lfix) dummy
        if (ivtyp3 .ne. 1) read (lfix) dummy
      endif
      if (iseqsw .ne. 1) then
        if (iseqsw .ne. 2) then
          if (iseqsw .eq. 3) goto 130
          goto 250
        endif
      endif

C     INITIALIZE SOLN COLMN
      do i = 1, nmx
        eyr(i) = 0.0
        eyi(i) = 0.0
      enddo
      if (iseq .ne. 1) then
        if (iseq .eq. 2) goto 270
        goto 360
      endif

      entry reduc1

  130 if (lfrst .ge. nmx+1) goto 380
      irow = lfrst
      if (inewts .eq. 2) irow = 1
      if (keybrd(0) .ne. 0) then
        write (outbuf, 10020) lfrst
10020   format ('0', 5x, 'REDUCE-LFRST =', i5)
        call prtout(1)
      endif

C     Retrieve Y- matrix row

      call getmat(irow, ii)

C     FOR NEWTONS METHOD, ADD YIO TERMS TO YII

      if (inewts .eq. 2) then
        atrow(ii-1) = atrow(ii-1) + gadmit(irow)
        atrow(ii) = atrow(ii) + badmit(irow)
        if (keybrd(30) .ne. 0) then
          write (l6, 10030) irow, atrow(ii-1), atrow(ii), gadmit(irow), 
     &     badmit(irow)
10030     format ('0', 2x, ' IROW= ', i4, 4(1x, e12.5))
        endif
      endif
      lens = matrow(2)
      if (keybrd(30) .ne. 0) then
        write (outbuf, 10040) irow, lfrst
10040   format ('0 IROW, LFRST = ', 2i10)
        call prtout(1)
      endif
      loclen = matrow(3)
      indx = 4
      if (lens .eq. 0) then
        bdum(1) = 0.0
        gdum(1) = 0.0
        locs(1) = 0
      else
        do ind = 1, lens
          locs(ind) = matrow(indx)
          gdum(ind) = atrow(indx+1)
          bdum(ind) = atrow(indx+2)
          indx = indx + 3
        enddo
      endif
      do ind = 1, loclen
        loc(ind) = matrow(indx)
        grow(ind) = atrow(indx+1)
        brow(ind) = atrow(indx+2)
        indx = indx + 3
      enddo

C     SETUP PROPER INDEX FOR REDUCTION

      iindxz = 1
      iilocf = 1
      ijspot = 1

C     READ REDUCED MATRIX TABLES

      indxm5 = indxm*5
      nmxm44 = nmxm4*4
      irowm = irow - 1
      index(1) = 1
      ibus = 0
      iphase = 1
      do while (iphase .le. lphase .and. ibus .lt. irow)

C       READ PHASE SHIFT TX DATA FROM ECS

        ibus = itbphn(1, iphase)
        iecsn(2) = itbphn(2, iphase)
        gphse = gijph(iphase)
        bphse = bijph(iphase)
        if (ibus .lt. irow) iphase = iphase + 1
      enddo
      if (irow .eq. 1) then
        nanti = 0
        janti = nmx + 1
        mytr = index(irow) - 1
      else

C       BUILD JSPOT TABLE FOR PARTIAL REDUCTION OF MATRIX
        do i = 1, irow
          j = index(i+1)
          jmin = index(i)
          do while (j .gt. jmin .and. (iloc(j-1) .ge. irow))
            j = j - 1
          enddo
          jspot(i) = j
        enddo
        mytr = index(irow) - 1
        if (lphase .ne. 0) then
          last = index(nmx) - 1
          do i = mytr, last
            if (iloc(i) .eq. janti) nanti = nanti - 1
          enddo
        endif
      endif
      do while (.true.)
        if (iseq .eq. 2) then
          grow(loclen) = grow(loclen) + tstr2(irow)
          brow(loclen) = brow(loclen) + tstr3(irow)
        elseif (iseq .ne. 3) then
          if (irow .eq. nmx) then
            if (iesc .eq. 2 .and. imat1 .ne. 2) then
              grow(loclen) = grow(loclen) - gfr
              brow(loclen) = brow(loclen) - bfr
            endif
          endif
        endif

        if (keybrd(0) .ne. 0) then
          lll = min0(lens, 4)
          mmm = max0(4-lll, 0)
          mmm = min0(mmm, loclen)
          write (outbuf, 10050) irow, lens, loclen, (locs(i), gdum(i), 
     &     bdum(i), i = 1, lll), (loc(i), grow(i), brow(i), i = 1, mmm)
          call prtout(1)
          do jjj = lll + 1, lens, 4
            kkk = min0(jjj+3, lens)
            write (outbuf, 10060) (locs(i), gdum(i), bdum(i), i = jjj, 
     &       kkk)
10050       format (' IROW=', i4, ' LENS=', i3, ' LOCLEN=', i3, 4(i5, 
     &     2e12.4))
            call prtout(1)
          enddo
          do jjj = mmm + 1, loclen, 4
            kkk = min0(jjj+3, loclen)
            write (outbuf, 10060) (loc(i), grow(i), brow(i), i = jjj, 
     &       kkk)
10060       format (30x, 4(i5, 2e12.4))
            call prtout(1)
          enddo
        endif
        if (lens .gt. MATROWSIZE) then
          i = lens
          j = irow
          k = loclen
          write (errbuf(1), 10070) lens, MATROWSIZE
10070     format ('0 REDUCE error, overflow of LOCS, GDUM, BDUM ', 2i6)
          call prterr('E', 1)
          call erexit()
        endif
        iuse = 0
        jloc = 0
        iflop = 1
        i = loclen

C       ENTER SKEWSYMMETRIC TERMS DUE TO PHASE SHIFTERS
        do while (irow .eq. ibus)
          jbus = iecsn(2)
          ifound = 1
          if (iflop .eq. 2) then
            iflop = 1
            l = 201
            i = 1
            lmax = loclen + 200
          else
            iflop = 2
            l = 1
            i = 201
            lmax = loclen
          endif
          do while (.true.)
            kbus = loc(l)
            loc(i) = kbus
            if (kbus .eq. jbus) then
              grow(i) = gphse
              brow(i) = bphse
              ifound = 2
              i = i + 1

C             Skewsymmetric component is identified by flag JANTI
C             (=NMX+1) and is located next to the corresponding
C             symmetric element such that it will be picked up
C             immediately after that element when a row is modified
C             or modifying (s500+)

              loc(i) = janti
              grow(i) = grow(l) - gphse
              brow(i) = brow(l) - bphse
            else
              grow(i) = grow(l)
              brow(i) = brow(l)
            endif
            if (l .eq. lmax) goto 140
            i = i + 1
            l = l + 1
          enddo
  140     loclen = loclen + 1
          if (ifound .eq. 1) then
            write (outbuf, 10080) iphase
10080       format (i10, ' Phase shifter not found in reduction')
            call prtout(1)
          endif
          iphase = iphase + 1
          if (iphase .gt. lphase) goto 150

C         READ PHASE SHIFT DATA FROM ECS
          ibus = itbphn(1, iphase)
          iecsn(2) = itbphn(2, iphase)
          gphse = gijph(iphase)
          bphse = bijph(iphase)
        enddo
  150   do while (.true.)
          jloc = jloc + 1
          if (jloc .gt. lens) goto 220
          jmod = locs(jloc)
          do while (.true.)
            ndo = 2
            do while (ndo .le. iuse)
              if (jmod .eq. iused(ndo)) goto 210
              ndo = ndo + 1
            enddo

C           Modify row IROW with row JMOD
C           Y'(IROW,II) = Y(IROW,II)
C           - Y(IROW,JMOD)*Y(JMOD,II)/Y(JMOD,JMOD)

            k = jspot(jmod)
            if (iloc(k) .eq. janti) then
              k = k + 1
              kout = 3
            else
              kout = 4
            endif

C           OBTAIN 1/Y(JMOD,JMOD)
            gaid1 = rdiar(jmod)
            baid1 = rdiax(jmod)
            pmsq = gaid1*gaid1 + baid1*baid1

C           CHECK FOR ZERO DIAGONAL AND SET ADMITTANCE TO ZERO

            if (abs(pmsq) .lt. 1e-10) then
              gpmr = 0.0
              bpmr = 0.0
            else
              gpmr = gaid1/pmsq
              bpmr =  - baid1/pmsq
            endif
            lsw = 2
            jsw = 2
            k1 = k + 1
            jspot(jmod) = k1
            jdiff = 1
            do while (.true.)
              j = index(jmod+1)
              m = index(jmod)
              nsiz = j - m
              do i = 1, nsiz
                gec(i) = gtri(i+m)
                bec(i) = btri(i+m)
              enddo

C             Obtain Y(IROW,JMOD)/Y(JMOD,JMOD)
C             = Y(JMOD,IROW)/Y(JMOD,JMOD) for symmetric component
C             = Y(JMOD,IROW)/-Y(JMOD,JMOD) for skewsymmetric component
              istr = (k-m) + 1
              gpm = gec(istr)*gpmr - bec(istr)*bpmr
              bpm = gec(istr)*bpmr + bec(istr)*gpmr
              if (iflop .eq. 2) then
                iflop = 1
                l = 200
                i = 0
                imax = 200
              else

C               Row is flopped back and forth in same array so that
C               only one set of equations and instructions are required
                iflop = 2
                l = 0
                i = 200
                imax = 400
              endif
              do while (.true.)
                l = l + 1

C               LL IS COLUMN WITH NEXT NON-ZERO ELEMENT OF Y(IROW,II)
                ll = loc(l)
                do while (.true.)
                  j = j - jdiff

C                 JJ IS COLUMN WITH NEXT NON-ZERO ELEMENT OF Y(JMOD,II)
                  if (j .eq. 0) j = 1
                  jj = iloc(j)
                  if (jsw .ne. 2) then
                    if (jsw .eq. 3) goto 190
                    if (jsw .eq. 4) goto 200

C                   if Y(IROW,JMOD) is skewsymmetric, we need special
C                   logic since

C                   SKEWSYMMETRIC*SYMMETRIC = SKEWSYMMETRIC and
C                   SKEWSYMMETRIC*SKEWSYMMETRIC = SYMMETRIC
                    if (jdiff .le. 0) then
                      jj = janti
                      jdiff = 2
                    elseif (iloc(j-1) .eq. janti) then
                      jdiff =  - 1
                      j = j - 1
                    else
                      jdiff = 1
                      lsw = 1
                    endif
                  endif
                  do while (.true.)
                    i = i + 1
                    if (iabs(i) .gt. imax) then
                      write (errbuf(1), 10090) iabs(i)
10090                 format (' Overflow in REDUCE, imax ', i6)
                      call prterr('E', 1)
                      call erexit()
                    endif
                    if (jj .ge. ll) then
                      if (jj .gt. ll) goto 170
                      if (ll .eq. irow) then
                        jsw = kout
                        ilast = i
                      endif
                      if (lsw .ne. 1) goto 160
                      lsw = 2
                      jj = janti
                    endif
                    loc(i) = ll
                    grow(i) = grow(l)
                    brow(i) = brow(l)
                    l = l + 1
                    ll = loc(l)
                  enddo
  160             if (lsw .eq. 2) goto 180
  170             loc(i) = jj
                  istr = (j-m) + 1
                  grow(i) =  - gec(istr)*gpm + bec(istr)*bpm
                  brow(i) =  - gec(istr)*bpm - bec(istr)*gpm
                enddo
  180           loc(i) = ll
                istr = (j-m) + 1
                brow(i) =  - gec(istr)*bpm - bec(istr)*gpm + brow(l)
                grow(i) =  - gec(istr)*gpm + bec(istr)*bpm + grow(l)
              enddo
  190         lsw = 2
              jsw = 1
              kout = 4
              bpmr =  - bpmr
              gpmr =  - gpmr
              k = k - 1
            enddo
  200       i = ilast

C           Check if too many terms in row right of diagonal
            if (i .gt. imax) then
              j = iflop
              k = irow
              write (errbuf(1), 10100) i, imax
10100         format ('0 REDUCE error, overflow of LOC, GROW, BRWO ', 
     &         2i6)
              call prterr('E', 1)
              call erexit()
            endif

            iuse = iuse + 1
            iused(iuse) = jmod
            j = index(jmod)
            jmod = iloc(j)
            if (jmod .eq. janti) jmod = iloc(j+1)
            if (jmod .ge. irow) goto 210
          enddo
  210     continue
        enddo
  220   jmin = 0
        if (iflop .eq. 2) jmin = 200
        if ((grow(i) .ne. 0.0) .or. (brow(i) .ne. 0.0)) then
          sqrcp = 1.0/(grow(i)*grow(i)+brow(i)*brow(i))
          drecpg = grow(i)*sqrcp
          drecpb =  - brow(i)*sqrcp
        else
          drecpb = 0.0
          drecpg = 0.0
        endif
        j = i - 1
        matind = 0
        do while (j .ne. jmin)
          if (.not. (loc(j) .eq. janti .and. grow(j) .eq. 0.0 .and. 
     &     brow(j) .eq. 0.0)) then
            mytr = mytr + 1
            matind = matind + 1
            gec(matind) = grow(j)*drecpg - brow(j)*drecpb
            bec(matind) = grow(j)*drecpb + brow(j)*drecpg
            il = loc(j)
            indxz(mytr) = il
            if (loc(j) .eq. janti) nanti = nanti + 1
          endif
          j = j - 1
        enddo
        idat = index(irow)
        jspot(irow) = idat
        rdiar(irow) = drecpg
        rdiax(irow) = drecpb
        irow = irow + 1
        mytr1 = mytr + 1
        index(irow) = mytr1

c       For new option test for faulted bus row and zero out factorized
c       matrix for this row
        if (iesc .eq. 3) then
          if (ifltb .eq. irow-1) then
            if (ifltsw .ne. 1) then
C             NEW OPTION
              do i = 1, matind
                gtri(i+idat) = 0.0
                btri(i+idat) = 0.0
              enddo
              goto 230
            endif
          endif
        endif
        do i = 1, matind
          gtri(i+idat) = gec(i)
          btri(i+idat) = bec(i)
        enddo
  230   if (irow .eq. jmx+1) then
          if (irow .eq. nmx+1) goto 240
        endif

C       Retrieve Y-matrix row

        call getmat(irow, ii)

C       YI, Newton solution option
        if (inewts .eq. 2) then

C         Add YIO to YII
          atrow(ii-1) = atrow(ii-1) + gadmit(irow)
          atrow(ii) = atrow(ii) + badmit(irow)
        endif

C       TEMP DEBUG.
        if (keybrd(30) .ne. 0) write (l6, 10030) irow, atrow(ii-1), 
     &     atrow(ii), gadmit(irow), badmit(irow)
        lens = matrow(2)
        loclen = matrow(3)
        indx = 4
        if (lens .eq. 0) then
          bdum(1) = 0.0
          gdum(1) = 0.0
          locs(1) = 0
        else
          do ind = 1, lens
            locs(ind) = matrow(indx)
            gdum(ind) = atrow(indx+1)
            bdum(ind) = atrow(indx+2)
            indx = indx + 3
          enddo
        endif
        do ind = 1, loclen
          loc(ind) = matrow(indx)
          grow(ind) = atrow(indx+1)
          brow(ind) = atrow(indx+2)
          indx = indx + 3
        enddo
        irowm = irow - 1
      enddo

C     STORE REDUCED MATRIX DATA INTO ECS
  240 indxf = index(nmx)
      indxm = (indxf+4)/5
      indxm5 = indxm*5
      nmxm44 = nmxm4*4
      goto 260

  250 if (ndmc .ne. 0) then
        if (lt .ne. lfix) rewind lt
        rewind lfix
        if (iseq .eq. 1) goto 360
        if (iseq .ne. 3) goto 290
      endif

  260 nsymm = mytr - nanti
      if (keybrd(12) .ne. 0) then
        write (outbuf, 10110) nmx, nsymm, nanti
10110   format (1x, i4, ' buses required ', i4, 
     &   ' off-diagonals, of which ', i4, 
     &   ' had skewsymmetric components.')
        call prtout(1)
        do jjj = 1, nmxm44, 8
          kkk = min0(jjj+7, nmxm44)
          write (outbuf, 10120) (ilocf(i), i = jjj, kkk)
10120     format ('0 ilocf table', (2x, 8i10))
          call prtout(1)
        enddo
        write (outbuf, 10130)
10130   format ('0 indxz table')
        call prtout(1)
        do jjj = 1, indxm5, 10
          kkk = min0(jjj+9, indxm5)
          write (outbuf, 10140) (indxz(i), i = jjj, kkk)
10140     format (2x, 10i8)
          call prtout(1)
        enddo
      endif
      do i = 2, nsize
        if (jspot(i-1) .ne. index(i)) then
          j = index(i)
          k = jspot(i-1)
          write (errbuf(1), 10150) j, k
10150     format ('0 REDUCE error, inconsistent data in JSPOT', 2i6)
          call prterr('E', 1)
          call erexit()
        endif
      enddo
      if (keybrd(12) .ne. 0) then

C       OUTPUT DIAGONALS OF REDUCED MATRIX
        write (outbuf, 10160)
10160   format ('0', 3x, 'rdiar,rdiax ')
        call prtout(1)
        do jjj = 1, nmx, 4
          kkk = min0(jjj+3, nmx)
          write (outbuf, 10170) (rdiar(i), rdiax(i), i = jjj, kkk)
10170     format (4(2x, 2e12.5))
          call prtout(1)
        enddo
      endif

C     Check if too many off-diagonals.
      if (mytr .gt. MAXBRN) then
        i = mytr
        j = nsize
        k = 0
        write (errbuf(1), 10180) mytr, MAXBRN
10180   format ('0 REDUCE error, overflow of ILOC, GTRI, BTRI ', 2i6)
        call prterr('E', 1)
        call erexit()

C     Check if too many buses
      elseif (nsize .gt. MAXBUS) then
        i = nsize
        j = mytr
        k = 0
        write (errbuf(1), 10190) nsize, MAXBUS
10190   format ('0 REDUCE error, overflow of INDEX, JSPOT ', 2i6)
        call prterr('E', 1)
        call erexit()
      else
        goto 380
      endif

C     STORE NEG SEQ Y
  270 ispf = 0
      do while (.true.)
        ispf = ispf + 1
        if (ispf .gt. isg) goto 280
        iecsn(1) = igentn(1, ispf)
        iecsn(2) = igentn(2, ispf)
        igbn = iecsn(1)
        if (iecs .ne. 0) then
          call redecs(tstr1, iecs, 100)

C         code below won't execute since prior neg seq code causes abort
          if (idp .ne. 0) then
            if (idp .ne. 1) then
              iecsn(1) = igentn(1, ispf)
              iecsn(2) = igentn(2, ispf)
              igr = iecsn(2)
              x = 0.5*(cgen(igr+2)+cgen(igr+4))
              r = cgen(igr+7)
              rsqrx = 1.0/(x*x+r*r)
              r2 = rneg + cgen(igr+1)
              x2 = xneg + cgen(igr)
              rzsq = 1.0/(r2*r2+x2*x2)
              tstr2(igbn) = r2*rzsq - r*rsqrx
              tstr3(igbn) =  - x2*rzsq + x*rsqrx
            endif
          endif
        endif
      enddo

C     REMOVE FAULT IMPED
  280 if (imat1 .eq. 2) then
        tstr2(nmx-1) = tstr2(nmx-1) - gopl
        tstr3(nmx-1) = tstr3(nmx-1) - bopl
      else
        tstr2(nmx) = tstr2(nmx) - gfr
        tstr3(nmx) = tstr3(nmx) - bfr
      endif
      goto 130

C     STORE EY CONS, E=1.0
  290 continue
      do i = 1, nmx
        eyr(i) = 0.0
        eyi(i) = 0.0
      enddo
      if (iseq .eq. 2 .and. imat1 .eq. 2) then
        eyr(nmx-1) = 1.0
      else
        eyr(nmx) = 1.0
      endif
      idn = nmx - 1
      ngn = 1

  300 if (idn .le. nmx-1) then
        if (.not. (eyr(idn) .eq. 0.0 .and. eyi(idn) .eq. 0.0)) then
          ilo = ilocf(idn)
          ihi = ilocf(idn+1) - 1
          slnr = eyr(idn)
          slni = eyi(idn)
          ipvot = idn
          i1 = 0
          ksz = ihi - ilo + 1
          kgt = kmtecg + ilo
          kbt = kmtecb + ilo
          do i = 1, ksz
            gec(i) = gtri(ilo+i-1)
            bec(i) = btri(ilo+i-1)
          enddo
          do i = ilo, ihi
            i2 = indxz(i)

C           CHECK FOR SKEWSYMMETRIC TERM
            if (i2 .eq. janti) then

C             SKEWSYMMETRIC TERM IS NEGATIVE TO LEFT OF DIAGONAL
              i2 = istort(i1+1)
              eyr(i2) = eyr(i2) + slnr*gec(i) - slni*bec(i)
              eyi(i2) = eyr(i2) + slni*gec(i) + slnr*bec(i)
            else
              eyr(i2) = eyr(i2) - slnr*gec(i) + slni*gec(i)
              eyi(i2) = eyi(i2) - slni*gec(i) - slnr*gec(i)
            endif
          enddo
        endif
        idn = idn + 1
        goto 300
      else

C       PROD OF SOLN COL AND DIAG
        do i = 1, nmx
          eyrt = rdiar(i)*eyr(i) - rdiax(i)*eyi(i)
          eyit = rdiax(i)*eyr(i) + rdiar(i)*eyi(i)
          eyr(i) = eyrt
          eyi(i) = eyit
        enddo

C       BACK SUBSTITUTION SETUP
        if (iesc .ne. 1) then
          if (iseq .ne. 2) then
            if (imat1 .ne. 2) then
              if (ngn .eq. nmx) then
                iecsn(1) = igentn(1, isrch)
                iecsn(2) = igentn(2, isrch)
                i = iecsn(2)
                tstr3(ngn) = cgen(i)
                goto 320
              else
                eyr(nmx) = 0.0
                eyi(nmx) = 0.0
              endif
            endif
          endif
        endif
        i = nmx - 1
        ihi = ilocf(nmx) - 1
        do while (.true.)
          ilo = ilocf(i)
          ipvot = i
          i1 = 0
          do ii = ilo, ihi
            i2 = indxz(ii)

C           CHECK FOR SKEWSYMMETRIC TERM
            if (i2 .eq. janti) i2 = istort(i1+1)
            eyr(i) = eyr(i) - eyr(i2)*gec(ii) + eyi(i2)*bec(ii)
            eyi(i) = eyi(i) - eyi(i2)*gec(ii) - eyr(i2)*bec(ii)
          enddo
          i = i - 1
          if (i .lt. ngn) goto 310
          ihi = ilocf(i+1) - 1
        enddo
  310   if (iseq .eq. 2) goto 340

C       OBTAIN RE+JXE
        rdrv = eyr(ngn)
        xdrv = eyi(ngn)

C       This negative seq code is full of uninitialized variables
C       Force program to abort here.
        write (*, '(2a)') ' REDUCE [f] - exercising untested negative', 
     &   ' sequence code.'
        write (*, '(a)') ' REDUCE [i] - program will stop here. '
        call erexit()

        rsqrx = 1.0/(rdrv**2+xdrv**2)
        gdrv = rdrv*rsqrx
        bdrv =  - xdrv*rsqrx
        iecsn(1) = igentn(1, isrch)
        iecsn(2) = igentn(2, isrch)
        igr = iecsn(2)

C       IGT=MDLGN(ISRCH)
        igt = 0
        if (mod(igt, 2) .eq. 0) igr = igr + 2
        x = 0.5*(cgen(igr)+cgen(igr+2))
        r = cgen(igr+5)
        rsqrx = 1.0/(r*r+cgen(igr)*cgen(igr+2))
        ge = gdrv - r*rsqrx
        be = bdrv + x*rsqrx
        gbsq = ge*ge + be*be
        tstr3(ngn) =  - be/gbsq + cgen(igr-2)
        if (keybrd(3) .ne. 0) then
          write (outbuf, 10200) ngn, gdrv, bdrv, x, r, cgen(igr-2)
          call prtout(1)
10200     format (1x, i3, 5e12.5, 'ngn,gdrv,bdrv,x,r,xt')
        endif
      endif
  320 do while (.true.)
        isrch = isrch + 1
        if (isrch .gt. isg) goto 350
        if (i .ne. 0) then
          if (i .ne. 2) goto 330
        endif
      enddo
  330 continue
      do i = 1, nmx
        eyr(i) = 0.0
        eyi(i) = 0.0
      enddo
      iecsn(1) = igentn(1, isrch)
      iecsn(2) = igentn(2, isrch)
      ngn = iecsn(1)
      idn = ngn
      eyr(idn) = 1.0
      goto 300

C     CALC NEG SEQ CURR DISTRIB FACTOR
  340 continue
      do ispf = 1, isg
        if (iecs .ne. 0) then
          call redecs(tstr1, iecs, 100)
          iecsn(1) = igentn(1, ispf)
          iecsn(2) = igentn(2, ispf)
          igbn = iecsn(1)
          igr = iecsn(2)

C         Put in error code here with ungraceful abort
          write (*, '(2a)') 
     &     ' REDUCE [f] - in incomplete negative sequence', 
     &     ' code. Program will stop here.'
          call erexit()
C         -
          r2 = rneg + cgen(igr+1)
          x2 = xneg + cgen(igr)
          rzsq = 1.0/(r2*r2+x2*x2)
          g2 = r2*rzsq
          b2 = x2*rzsq
          cr = eyr(igbn)*g2 - eyi(igbn)*b2
          ci = eyi(igbn)*g2 + eyr(igbn)*b2
          tstr2(igbn) = sqrt(cr*cr+ci*ci)
        endif
        tstr3(igbn) = 0.0
      enddo
      if (keybrd(3) .ne. 0) then
        i = 1732
        write (outbuf, 10210) i
10210   format ('REDUCE/10210', i5)
        call prtout(1)
        do jjj = 1, nmx, 3
          kkk = min0(jjj+2, nmx)
          write (outbuf, 10220) (k, eyr(k), eyi(k), k = jjj, kkk)
10220     format (1x, 3(i5, 2e13.7))
          call prtout(1)
        enddo
      endif
      if (ndmc .ne. 1) then
        iseqsw = 2
        iseq = 1
        goto 120
      endif

C     UPDATE TAPE WITH XE, NEG, DIST FAC
C     code below won't execute since prior neg seq code causes abort
  350 read (lspare) eyr, eyi
      rewind lspare
      do ispf = 1, isg
        if (iecs .ne. 0) then
          call redecs(tstr1, iecs, 100)
          iecsn(1) = igentn(1, ispf)
          iecsn(2) = igentn(2, ispf)
          igbn = iecsn(1)
          xe = tstr3(igbn)
          disf2 = tstr2(igbn)
          call ritecs(tstr1, iecs, 100)
          if (keybrd(3) .ne. 0) then
            write (outbuf, 10230) igbn, xe, disf2
10230       format (1x, i4, 2e12.5, ' bus,xe,disf2')
            call prtout(1)
          endif
        endif
      enddo
      iseqsw = 3
      iseq = 3
      goto 120

  360 isrch = 0
      goto 320

  370 ndmc = 0
      goto 130
  380 return
      end
