C    %W% %G%
      subroutine genout
C     
C     THIS SUBROUTINE CALCULATES AND OUTPUTS THE
C     GENERATOR QUANTITIES WHICH HAVE BEEN REQUESTED
 
C     Revs:
C     Sep/16/92 - DEM: Renamed signal exciter supp. to PSS output
 
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/comn56.inc'
      include 'tspinc/link56.inc'
      include 'tspinc/dc.inc'
      include 'tspinc/out512.inc'
      include 'tspinc/kbusno.inc'
      include 'tspinc/nwgntn.inc'
      include 'tspinc/mgen.inc'
      include 'tspinc/geno.inc'
      include 'tspinc/worst.inc'
      include 'tspinc/kcnstx.inc'
      include 'tspinc/igend1.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/mvn.inc'
      include 'tspinc/gdif.inc'
      include 'tspinc/room.inc'
      include 'tspinc/prtmax.inc'

      common /kngenn/kngenn(2, MAXGEN)
      character*1 kngenc

      common /kngenc/kngenc(MAXGEN)
      common /outop/igensw(15)

      character*8 ibusc, id
      dimension cur(2*MAXSTP), regsup(2*MAXSTP), vflcfd(2*MAXSTP),
     &          accgrp(1000, 50), govpepq(2*MAXSTP)
      dimension fminmx(28), jgen(15), kcount(15)
      equivalence (mbus, mbust)
      dimension cst(11)
      dimension msub10(2), msub12(2), msub13(2)
      equivalence (noepq, nogen(4)), (nofsat, nogen(5)), 
     &            (noxsat, nogen(8)), (nopacc, nogen(10)), 
     &            (nogenp, nogen(7)), (nogenq, nogen(11)), 
     &            (notqdp, nogen(13)), (nofcur,nogen(14))
 
      equivalence (nobus, nobust), (nosup, nogen(12))
      equivalence (jgen(1), jangl), (jgen(2), jangp), (jgen(3), jvfld),
     &            (jgen(4), jepq), (jgen(5), jfsat), (jgen(6), jgovp),
     &            (jgen(7), jgenp), (jgen(8), jxsat), (jgen(9), jreg),
     &            (jgen(10), jpacc), (jgen(11), jgenq), 
     &            (jgen(12), jxsup), (jgen(13), jtqdmp), 
     &            (jgen(14), jfcur), (jgen(15), jmpa)
 
      equivalence (kcount(1), ktang), (kcount(2), ktanp), 
     &            (kcount(3), ktvfl), (kcount(4), ktepq), 
     &            (kcount(5), ktfsat), (kcount(6), ktgov), 
     &            (kcount(7), ktgenp), (kcount(8), ktxsat), 
     &            (kcount(9), ktreg), (kcount(10), ktpacc),
     &            (kcount(11), ktgenq), (kcount(12), ktxsup), 
     &            (kcount(13), ktqdmp), (kcount(14), ktfcur)
 
      equivalence (ksub(4), ksub4b), (ksub(5), ksub1a), 
     &            (ksub(7), ksub7a), (ksub(8), ksub1b), 
     &            (ksub(9), ksb12b), (ksub(10), ksub7b), 
     &            (ksub(11), ksub2b), (ksub(12), ksub12),
     &            (ksub(13), ksub2a), (ksub(14), ksub4a)
 
      equivalence (msub(19), msub10), (msub(23), msub12), 
     &            (msub(25), msub13)
 
      equivalence (iagsw, igensw(1)), (iapsw, igensw(2)), 
     &            (iepqsw, igensw(4)), (ifssw, igensw(5)), 
     &            (igovsw, igensw(6)), (igpsw, igensw(7)), 
     &            (iexsw, igensw(8)), (irgsw, igensw(9)), 
     &            (ipasw, igensw(10)), (igqsw, igensw(11)),
     &            (ixspsw, igensw(12)), (itqsw, igensw(13)), 
     &            (ifcsw, igensw(14)), (impasw, igensw(15))

C     -  Meanings of IGENSW[] - write out this parm:
C     [1] IAGSW - angle;  
C     [2] IAPSW - speed dev;  
C     [3] field voltage;
C     [4] IEPQSW - E'q;  
C     [5] IFSSW - fld sat;   
C     [6] IGOVSW - P_mech;
C     [7] IGENSW - P_elec;  
C     [8] IEXSW - exc sat; 
C     [9] IRGSW - V_reg;
C     [10] IPASW - P_acc;  
C     [11] IGQSW - Q_gen; 
C     [12] IXSPSW - PSS out;
C     [13] ITQSW - Dmp trq; 
C     [14] IFCSW - I_fd;
C     [15] IMPASW - Area P_acc;
 
      logical opplot, opprnt, opaux
C     -  Gen name to repeat for each curve in new aux format
      character gname*40
C     -    Begin    Begin    Begin    Begin    Begin    Begin    Begin
C     !
      call mpost('GENOUT')
      ktang = 0
      ktanp = 0
      ktvfl = 0
      ktgov = 0
      ktcur = 0
      ktsup = 0
      ktgenp = 0
      ktgenq = 0
      ktepq = 0
      ktfcur = 0
      ktfsat = 0
      ktqdmp = 0
      ktpacc = 0
      ktxsup = 0
      ktreg = 0
      ktxsat = 0
      jsup = 0
      ipaa = 0
      icoun1 = icount + 3
      icoun2 = icount + 2
      ksub1a = kwork
      ksub1b = ksub1a + nofsat
      ksub2a = ksub1b + noxsat
      ksub2b = ksub2a + notqdp
      ksub4a = ksub2b + nogenq
      ksub4b = ksub4a + nofcur
      ksub7a = ksub4b + noepq
      ksub7b = ksub7a + nogenp
      ksb12b = ksub7b + nopacc
      if (nogrp .ne. 0) then
        kwork = ksb12b + nosup
C       
C       PROCESSING GENERATORS ONE AT A TIME
C       
        do i = 1, nogrp
          do j = 1, icoun2
            accgrp(j, i) = 0
          enddo
        enddo
      endif
      icnt2 = 2*icount
      do ind = 1, 15
        jgen(ind) = 0
        jcount(ind) = 0
      enddo
      do i = 1, isg
        iecs = (i-1)*11
        do l = 1, 11
          cst(l) = constx(iecs+l)
        enddo
        xdp = cst(1)
        xdpp = cst(11)
        ib = kngenn(1, i)
        ibus = 0
        if (nobus .ne. 0) ibus = kbusno(ib)
        izero = 0
        do ind = 1, 15
          ind1 = (ind-1)*MAXGEN
          jadr = ind1 + i
          igensw(ind) = mgen(jadr)
          izero = izero + igensw(ind)
        enddo
        if (izero .ne. 0) then
          isupsw = msup(i)
          icursw = mcur(i)
          ibusw = mbus(ib)
          jadr = 2701
          if (ibusw .ne. 0) then
            jbus = jadr
            call redecp(msub13(2), ksub13+ibus-1, 1)
            call readmp (13, worksp(jbus), icnt2, 1)
            jadr = jadr + icnt2 + 1
          endif
          if (icursw .ne. 0) then
            ktcur = ktcur + 1
            ktcr = 2*ktcur - 1
            jcur = jadr
            call redecp(msub10(2), ksub10+ktcr-1, 1)
            call readmp (10, cur, icount*2, 1)
            jadr = jadr + icoun1
          endif
          if (isupsw .ne. 0) then
            ktsup = ktsup + 1
            ktsp = 2*ktsup - 1
            jsup = jadr
            call redecp(msub12(2), ksub12+ktsp-1, 1)
            call readmp (12, regsup, icount*2, 1)
            jadr = jadr + icoun1
          endif
          do jind = 1, 15
            if (igensw(jind) .ne. 0) then
              jgen(jind) = jadr
              if (jind .le. 3 .or. jind .eq. 6) then
                kcount(jind) = kcount(jind) + 1
                kcnt = kcount(jind)
                if (jind .eq. 3 .or. jind .eq. 6) kcnt = 2*kcnt - 1
                mind = msindx(jind)
                call redecp(msub(2*mind), kcnt+ksub(jind)-1, 1)
                if (jind .eq. 3) then
                  call readmp(mind, vflcfd, icount*2, 1)
                else if (jind .eq. 6) then
                  call readmp(mind, govpepq, icount*2, 1)
                else
                  call readmp(mind, worksp(jadr+1), icount, 1)
                endif
              endif
              jadr = jadr + icoun1
            endif
          enddo
          jfirst = 0
          ifirst = 0
          if (jadr .ne. 2701) then
            do j = 1, 28
              fminmx(j) = 0.0
            enddo
C           
C           INITIALIZE ARRAYS TO HOLD MAX AND MIN VALUES FOR OUTPU
C           
            do itrr = 1, 18
              prtmax(itrr) =  - 10000.
              prtmin(itrr) = 10000.
            enddo
            do ind = 1, icount
C             
C             STORE FIELD VOLTAGE
C             
              if (igensw(3) .ne. 0) worksp(jvfld+ind) = vflcfd(ind)
              if (igensw(4) .ne. 0) 
     &          worksp(jepq+ind) = govpepq(ind+icount)
              if (igensw(6) .ne. 0) worksp(jgovp+ind) = govpepq(ind)
              if (icursw .ne. 0) then
                curr = cur(ind)
                curi = cur(ind+icount)
              endif
              if (ibusw .ne. 0) then
                iadr = jbus + (ind-1)*2
                er = worksp(iadr)
                ei = worksp(iadr+1)
              endif
              if (igpsw .ne. 0) 
     &          worksp(jgenp+ind) = (curr*er+curi*ei 
     &                            + (curr*curr+curi*curi)*cst(3))*bmva
              if (igqsw .ne. 0) 
     &          worksp(jgenq+ind) = (curr*ei-curi*er)*bmva
              if (ifcsw .ne. 0) then
                if (cst(10) .le. -10000.) then
                  cosa = cos(cst(2))
                  sina = sin(cst(2))
                  vd = er*sina - ei*cosa
                else
                  cosa = cos(worksp(jangl+ind))
                  sina = sin(worksp(jangl+ind))
                endif
                xid = curr*sina - curi*cosa
                xiq = curr*cosa + curi*sina
                vq = er*cosa + ei*sina
                if (xdpp .gt. 0.) then
C                   
C                 STORE FIELD CURRENT FOR GENERATORS WITH DAMPER WINDI
C                   
                  vfl = vflcfd(ind)
                  cfd = vflcfd(ind+icount)
                  worksp(jfcur+ind) = cfd
                else
                  worksp(jfcur+ind) = vq + cst(3)*xiq + cst(4)*xid
                endif
              endif
              if (iagsw .ne. 0) worksp(jangl+ind) = worksp(jangl+ind)
     &         *degrad
              if (ifssw .ne. 0) then
                worksp(jfsat+ind) = 0.0
                if (iepqsw .ne. 0) then
                  sd = 0.0
                  if (cst(10) .ge. -9999.) then
                    if (cst(7) .ne. 0.0) then
                      vpr = er + cst(3)*curr - cst(6)*curi
                      vpi = ei + cst(3)*curi + cst(6)*curr
                      esat1 = sqrt(vpr*vpr+vpi*vpi)
                      if (cst(7) .le. esat1) 
     &                 sd = (cst(8)*(esat1-cst(7))*(esat1-cst(7)))/esat1
                    endif
                  endif
                  worksp(jfsat+ind) = sd
                elseif (cst(7) .eq. 0.0) then
                  worksp(jfsat+ind) = 0.0
                else
                  if (cst(6) .eq. 0.0) then
                    esat1 = worksp(jepq+ind)
                  else
                    vpr = er + cst(3)*curr - cst(6)*curi
                    vpi = ei + cst(3)*curi + cst(6)*curr
                    esat1 = sqrt(vpr*vpr+vpi*vpi)
                  endif
                  if (cst(7) .le. esat1) worksp(jfsat+ind) = (cst(8)*
     &             (esat1-cst(7))*(esat1-cst(7)))/esat1
                endif
              endif
              if (itqsw .ne. 0) worksp(jtqdmp+ind) = (cst(2)*worksp
     &         (jangp+ind))*bmva
              if (iapsw .ne. 0) worksp(jangp+ind) = worksp(jangp+ind)
     &         *hzconv
              if (ipasw .ne. 0 .or. impasw .ne. 0) then
                if (ipasw .ne. 0) worksp(jpacc+ind) = worksp(jgovp+ind)
     &           *bmva - worksp(jtqdmp+ind) - worksp(jgenp+ind)
                if (impasw .ne. 0) then
                  worksp(jmpa+ind) = worksp(jgovp+ind)*bmva - worksp
     &             (jtqdmp+ind) - worksp(jgenp+ind)
                  ipaa = 1
                endif
              endif
              if (igovsw .ne. 0) worksp(jgovp+ind) = worksp(jgovp+ind)
     &         *bmva
              if (iexsw .ne. 0) then
                worksp(jxsat+ind) = 0.0
                if (cst(10) .ne. 0.0) then
                  vfld = vflcfd(ind)
C                 
C                 RETRIEVE AND STORE FIELD VOLTS FOR DAMPER WINDING GENE
C                 
                  worksp(jvfld+ind) = vfld
                  sat = 0.0
                  sat = cst(10)*exp(cst(9)*abs(vfld))
                  worksp(jxsat+ind) = sat
                endif
              endif
              if (irgsw .ne. 0 .or. ixspsw .ne. 0) then
                reg = regsup(ind)
                sup = regsup(ind+icount)
                if (irgsw .ne. 0) worksp(jreg+ind) = reg
                if (ixspsw .ne. 0) worksp(jxsup+ind) = sup
              endif
C             
C             OBTAIN MAX AND MIN VALUES FOR PRINTING AND PLOTTING
C             
              do j = 1, 14
                jindx = jgen(j) + ind
                if (t(ind) .ge. wtim1 .and. t(ind) .le. wtim2 .or.
     &           wtim2 .eq. -1.0) then
                  if (worksp(jindx) .gt. prtmax(j)) prtmax(j) = worksp
     &             (jindx)
                  if (worksp(jindx) .lt. prtmin(j)) prtmin(j) = worksp
     &             (jindx)
                endif
                if (worksp(jindx) .gt. fminmx(2*j-1)) fminmx(2*j-1) =
     &           worksp(jindx)
                if (worksp(jindx) .lt. fminmx(2*j)) fminmx(2*j) =
     &           worksp(jindx)
              enddo
            enddo
            jfirst = 0
            ifirst = 0
            do ind = 1, 14
              imvsw = 0
              if (igensw(ind) .ge. 10) then
                igensw(ind) = igensw(ind) - 10
                igend1(ind+16, i) = igensw(ind)
                imvsw = 1
              endif
              jadr = jgen(ind)
C             
C             IF THIS IS RELATIVE ANGLE, SUBTRACT THE REFERENCE AN
C             
              if ((igensw(1) .ne. 0) .and. (ind .eq. 1) .and. (iref
     &         .gt. 0)) then
                bias = 0.0
                if ((worksp(jadr+1)-refag(1)) .lt. -180.) bias = 360.
                if ((worksp(jadr+1)-refag(1)) .gt. 180.) bias =  - 360.
                prtmax(1) =  - 10000.
                prtmin(1) = 10000.
                fminmx(1) = 0.0
                fminmx(2) = 0.0
                do jj = 1, icount
                  if (worksp(jadr+jj) .ge. -9999.) then
                    worksp(jadr+jj) = worksp(jadr+jj) - refag(jj) +
     &               bias
                    if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or.
     &               wtim2 .eq. -1.0) then
                      if (worksp(jadr+jj) .gt. prtmax(1)) prtmax(1) =
     &                 worksp(jadr+jj)
                      if (worksp(jadr+jj) .lt. prtmin(1)) prtmin(1) =
     &                 worksp(jadr+jj)
                    endif
                    if (worksp(jadr+jj) .gt. fminmx(1)) fminmx(1) =
     &               worksp(jadr+jj)
                    if (worksp(jadr+jj) .lt. fminmx(2)) fminmx(2) =
     &               worksp(jadr+jj)
                  endif
                enddo
              endif
              if (opplot(ind)) then
                jcount(ind) = jcount(ind) + 1
                mind = msindx(ind)
                worksp(jadr+icount+1) = fminmx(2*ind-1)
                worksp(jadr+icount+2) = fminmx(2*ind)
                if (icoun2 .gt. 0) call writmp(mind, worksp(jadr+1),
     &           icoun2, 1)
                call ritecp(msub(2*mind), ksub(ind)+jcount(ind)-1, 1)
              endif
              igensw(ind) = igend1(ind+16, i)
              if (opprnt(ind) .or. (opaux(ind))) then
                if (ifirst .ne. 1 .and. jfirst .ne. 1) then
                  ibusc = nwgntc(1, i)
                  ibase = nwgntn(i)
                  id = nwgntc(2, i)
                  base1 = basekv(ibase)
                endif
                if (jfirst .ne. 1) then
                  jfirst = 1
                  nqty = 0
C                 
C                 COUNT HOW MANY QTYS GO TO AUXILIARY FILE
C                 
                  do ji = 1, 15
                    if (opaux(ji)) nqty = nqty + 1
                  enddo
C                 WRITE TO AUX FILE IF ANY
                  if (nqty .ne. 0) then
C                   -  List reference gen with current gen
                    if (iref .eq. 0) then
C                     WRITE (L11,712) IBUSC,BASE1,ID,NQTY
                      write (gname, 10000) ibusc, base1, id, nqty
10000                 format (a8, f5.1, 1x, a1, 1x, 16x, i2)
C                     712           FORMAT ('G  ',A8,F5.1,1X,A1,1X,16X,I
                    else
                      baserf = basekv(nwgntn(iref))
C                     WRITE (L11,710) IBUSC,BASE1,ID,
                      write (gname, 10010) ibusc, base1, id, nwgntc(1,
     &                 iref), baserf, nwgntc(2, iref), nqty
10010                 format (2(a8, f5.1, 1x, a1, 1x), i2)
C                     710           FORMAT ('G  ',2(A8,F5.1,1X,A1,1X),I2
                    endif
                    if (auxfmt .eq. 'STD') write (l11, '(2A)') 'G  ',
     &               gname
                  endif
                endif
                if (ifirst .ne. 1) then
                  if (opprnt(ind)) then
                    write (outbuf, 10020) ibusc, base1, id
                    call prtout(1)
                  endif
10020             format ('0', 5x, 'OUTPUT LISTING FOR GENERATOR', 2x,
     &             a8, 2x, f5.1, 2x, a1)
                  ifirst = 1
                endif
C               
C               IF DATA IS NEEDED FOR A TWO OPTION OUTPUT REQUEST
C               THEN STORE IT IN VSPCE
C               
                if (imvsw .ne. 0) then
                  do n = 1, 2*mvkt
                    if (mvgen(n) .eq. i) then
                      if (mvcde(n) .eq. 'G') then
                        if (mvopt(n) .eq. ind) then
                          mvadr(n) = madr1
                          do l = 1, icoun2
                            vspce(madr1+l) = worksp(jadr+l)
                          enddo
                          madr1 = madr1 + icoun2
                        endif
                      endif
                    endif
                  enddo
                endif
                if (ind .eq. 2) then
                  if (opprnt(ind)) then
                    write (outbuf, 10030) prtmax(2), prtmin(2)
10030               format ('0', 5x, 'FREQUENCY DEVIATION (HERTZ)', 5x,
     &               ' MAX = ', f8.4, ' MIN = ', f8.4)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10040) (t(jj), worksp(jangp+jj),
     &                 jj = jjj, kkk)
10040                 format (5(f8.2, ' CYCLES', f8.4))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jangp)
                elseif (ind .eq. 3) then
                  if (opprnt(ind)) then
                    write (outbuf, 10050) prtmax(3), prtmin(3)
10050               format ('0', 5x, 'FIELD VOLTS PU', 5x, ' MAX = ',
     &               f8.4, ' MIN = ', f8.4)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10060) (t(jj), worksp(jj+jvfld),
     &                 jj = jjj, kkk)
10060                 format (5(f8.2, ' CYCLES', f8.4))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jvfld)
                elseif (ind .eq. 4) then
                  if (opprnt(ind)) then
                    write (outbuf, 10070) prtmax(4), prtmin(4)
10070               format ('0', 5x, 'FLUX LINKAGE--EPQ (PU)', 5x,
     &               ' MAX = ', f8.4, ' MIN = ', f8.4)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10080) (t(jj), worksp(jepq+jj), jj
     &                 = jjj, kkk)
10080                 format (5(f8.2, ' CYCLES ', f8.4))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jepq)
                elseif (ind .eq. 5) then
                  if (opprnt(ind)) then
                    write (outbuf, 10090) prtmax(5), prtmin(5)
10090               format ('0', 5x, 'MAIN FIELD SAT. (PU)', 5x,
     &               ' MAX = ', f8.4, ' MIN = ', f8.4)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10100) (t(jj), worksp(jfsat+jj),
     &                 jj = jjj, kkk)
10100                 format (5(f8.2, ' CYCLES ', f8.4))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jfsat)
                elseif (ind .eq. 6) then
                  if (opprnt(ind)) then
                    write (outbuf, 10110) prtmax(6), prtmin(6)
10110               format ('0', 5x, 'TURBINE POWER (MW)', 5x,
     &               ' MAX = ', f9.2, ' MIN = ', f9.2)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10120) (t(jj), worksp(jgovp+jj),
     &                 jj = jjj, kkk)
10120                 format (5(f8.2, ' CYCLES ', f9.2))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jgovp)
                elseif (ind .eq. 7) then
                  if (opprnt(ind)) then
                    write (outbuf, 10130) prtmax(7), prtmin(7)
10130               format ('0', 5x, 'GENERATOR POWER  (MW)', 5x,
     &               ' MAX = ', f9.2, ' MIN = ', f9.2)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10140) (t(jj), worksp(jgenp+jj),
     &                 jj = jjj, kkk)
10140                 format (5(f8.2, ' CYCLES ', f9.2))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jgenp)
                elseif (ind .eq. 8) then
                  if (opprnt(ind)) then
                    write (outbuf, 10150) prtmax(8), prtmin(8)
10150               format ('0', 5x, 'EXCITATION SAT. (PU)', 5x,
     &               ' MAX = ', f8.4, ' MIN = ', f8.4)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10160) (t(jj), worksp(jj+jxsat),
     &                 jj = jjj, kkk)
10160                 format (5(f8.2, ' CYCLES ', f8.4))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jxsat)
                elseif (ind .eq. 9) then
                  if (opprnt(ind)) then
                    write (outbuf, 10170) prtmax(9), prtmin(9)
10170               format ('0', 5x, 'REG. OUTPUT VOLTS (PU)', 5x,
     &               ' MAX = ', f8.4, ' MIN = ', f8.4)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10180) (t(jj), worksp(jj+jreg), jj
     &                 = jjj, kkk)
10180                 format (5(f8.2, ' CYCLES ', f8.4))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jreg)
                elseif (ind .eq. 10) then
                  if (opprnt(ind)) then
                    write (outbuf, 10190) prtmax(10), prtmin(10)
10190               format ('0', 5x, 'ACCELERATING POWER (MW)', 5x,
     &               ' MAX = ', f8.2, ' MIN = ', f8.2)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10200) (t(jj), worksp(jj+jpacc),
     &                 jj = jjj, kkk)
10200                 format (5(f8.2, ' CYCLES ', f8.2))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jpacc)
                elseif (ind .eq. 11) then
                  if (opprnt(ind)) then
                    write (outbuf, 10210) prtmax(11), prtmin(11)
10210               format ('0', 5x, 'GENERATOR MVAR', 5x, ' MAX = ',
     &               f8.2, ' MIN = ', f8.2)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10220) (t(jj), worksp(jgenq+jj),
     &                 jj = jjj, kkk)
10220                 format (5(f8.2, ' CYCLES ', f8.2))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jgenq)
                elseif (ind .eq. 12) then
                  if (opprnt(ind)) then
                    write (outbuf, 10230) prtmax(12), prtmin(12)
10230               format ('0', 5x, 'PSS OUTPUT (PU)', 5x, ' MAX = ',
     &               f8.4, ' MIN = ', f8.4)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10240) (t(jj), worksp(jxsup+jj),
     &                 jj = jjj, kkk)
10240                 format (5(f8.2, ' CYCLES ', f8.4))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jxsup)
                elseif (ind .eq. 13) then
                  if (opprnt(ind)) then
                    write (outbuf, 10250) prtmax(13), prtmin(13)
10250               format ('0', 5x, 'TORQUE DAMPING (MW)', 5x,
     &               ' MAX = ', f8.4, ' MIN = ', f8.4)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10260) (t(jj), worksp(jtqdmp+jj),
     &                 jj = jjj, kkk)
10260                 format (5(f8.2, ' CYCLES ', f8.4))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jtqdmp)
                elseif (ind .eq. 14) then
                  if (opprnt(ind)) then
                    write (outbuf, 10270) prtmax(14), prtmin(14)
10270               format ('0', 5x, 'FIELD CURRENT (PU)', 5x,
     &               ' MAX = ', f8.4, ' MIN = ', f8.4)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10280) (t(jj), worksp(jfcur+jj),
     &                 jj = jjj, kkk)
10280                 format (5(f8.2, ' CYCLES ', f8.4))
                      call prtout(1)
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jfcur)
                else
                  if (opprnt(ind)) then
                    write (outbuf, 10290) prtmax(1), prtmin(1)
10290               format ('0', 5x, 'GENERATOR ANGLE (DEGREES)', 5x,
     &               ' MAX = ', f9.3, ' MIN = ', f9.3)
                    call prtout(1)
                    do jjj = 1, icount, 5
                      kkk = min0(jjj+4, icount)
                      write (outbuf, 10300) (t(jj), worksp(jangl+jj),
     &                 jj = jjj, kkk)
10300                 format (5(f8.2, ' CYCLES ', f9.3))
                      call prtout(1)
 
                    enddo
                  endif
                  if (opaux(ind)) call auxout(gname, ind, jangl)
                endif
              endif
            enddo
            impasw = igend1(31, i)
            if (impasw .ne. 0) then
              do ix = 1, nogrp
                igrp1 = igrup(ix)
                if (igrp1 .ne. 0) then
                  igrp = igend1(1, i)
                  if (igrp1 .eq. igrp) then
                    do iz = 1, icount
                      accgrp(iz, ix) = accgrp(iz, ix) + work(jmpa+iz)
                    enddo
                  endif
                endif
              enddo
            endif
C           
C           FORM TABLES FOR GENERATOR DIFFERENCE QUANITITES
C           
            if (idifkt .ne. 0) then
              do l = 1, idifkt
                if (i .eq. k1gdif(l)) then
                  do m = 1, icoun2
                    gdifan(m, l) = worksp(jangl+m) + gdifan(m, l)
                    gdifsp(m, l) = worksp(jangp+m) + gdifsp(m, l)
                    gdifpw(m, l) = worksp(jgenp+m) + gdifpw(m, l)
                  enddo
                endif
                if (i .eq. k2gdif(l)) then
                  do m = 1, icoun2
                    gdifan(m, l) =  - worksp(jangl+m) + gdifan(m, l)
                    gdifsp(m, l) =  - worksp(jangp+m) + gdifsp(m, l)
                    gdifpw(m, l) =  - worksp(jgenp+m) + gdifpw(m, l)
                  enddo
                endif
              enddo
            endif
          endif
        endif
      enddo
      max = icount + 1
      min = icount + 2

      do i = 1, nogrp
        do j = 1, icount
          if (accgrp(j, i) .gt. accgrp(max, i)) accgrp(max, i) =
     &       accgrp(j, i)
          if (accgrp(j, i) .lt. accgrp(min, i)) accgrp(min, i) =
     &       accgrp(j, i)
        enddo
      enddo
      if (ipaa .ne. 0) then
        do ix = 1, nogrp
          if (igrup(ix) .ne. 0) then
            write (outbuf, 10310) igrup(ix)
10310       format ('0', 5x, ' ACCELERATING POWER (MW) FOR GROUP ', i3)
            call prtout(1)
            do jjj = 1, icount, 5
              kkk = min0(jjj+4, icount)
              write (outbuf, 10320) (t(jj), worksp(2700+jj), jj = jjj,
     &         kkk)
10320         format (5(2x, f7.2, ' CYCLES ', f8.2))
              call prtout(1)
            enddo
          endif
        enddo
      endif
      return
      end
