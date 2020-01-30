C    %W% %G%
      subroutine soln
C     
C     This subroutine is called twice by subroutine SWINGM.
C     The first pass it initializes variables, writes header data
C     to the FOR008 (.SOL) file and returns. After the table
C     of factors for the admittance matrix has been obtained, it
C     calls NETWRK to obtain the initial (time equals zero plus)
C     network solution and writes this data to the FOR008 file.
C     
      include 'tspinc/params.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/blkcom2.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/prt.inc'
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
      include 'tspinc/rk.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/param1.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/equiv.inc'
      include 'tspinc/brake1.inc'
      include 'tspinc/ectba.inc'
      include 'tspinc/mdctbl.inc'
      include 'tspinc/gamma.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/ecstbc.inc'
      include 'tspinc/ecstbd.inc'
      include 'tspinc/ecstbh.inc'
      include 'tspinc/ecstbj.inc'
      include 'tspinc/linksw.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/brnch.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/citer.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/vym.inc'
      include 'tspinc/vymn.inc'
      include 'tspinc/znox.inc'

      dimension istem(MAXLS)
      equivalence (istem, isorti)
      dimension itemps(65), temps1(390), itemp2(450), temps3(900), 
     &          itempm(65)
      character*8 itmpsc(65), itmp2c(450)
      character*8 inamec, jnamec
c dlc remove destructive equiv
      real const
      dimension const(11*MAXGEN)
c dlc      equivalence (const(1), brnch(1, 1))

C     Functions

      logical dbghere

C     Local variables

      logical debug

C     begin     begin     begin     begin     begin     begin

      debug = dbghere('SOLN    ')
      indxf = nmx*5
      if (nchck .eq. 2) then
C       
C       Y MATRIX HAS BEEN REDUCED AND SUCCESSFUL INITIAL SOLUTION
C       HAS BEEN OBTAINED.  INITIALIZE A FEW MORE VARIABLES AND
C       RETURN TO SWINGM TO START SOLUTION
C       
        lpsol = 1
        idvrg = 1
        ntry = 0
        imxmn1 = 1
        imxmn2 = 1
        maxg = 2
        ming = 2
        icntl1 = 1
        icntl2 = 1
        al = 0.
        lnksw1 = 2
        link = 1
      elseif (nchck .eq. 3) then
C       
C       INITIAL Y MATRIX REDUCTION HAS BEEN COMPLETED
C       
        nchck = 1
C       
C       OBTAIN INITIAL NETWORK SOLUTION IN SUBROUTINE DERIV
C       
        call deriv()
C       
C       CHECK FOR DIVERGENT INITIAL NETWORK SOLUTION
C       
        if (link .gt. 10) then
          link = link - 10
        else
          if (.not. (ldc .eq. 0 .and. ldc1 .eq. 0)) then
            ndc1 = 0
            ndc2 = 0
            idch1 = 0
            ldcm = 0
            iterm = 0
            i1 = 1
            if (ldc .ne. 0) then
C             
C             PREPARE 2 TERM DC TABLES FOR OUTPUT TO .SOL FILE
C             THERE ARE SIX ENTRIES FOR EACH TERMINAL LAST TWO ARE
C             
              i1 = 0
              do i = 1, ldc
                i2 = (i-1)*idcl + kdc
                call redecs(ptab(1), i2, 252)
C               
C               ITMPSC(I) = RECTIFIER BUS NAME CHAR*8
C               ITMPSC(I+1) = INVERTER BUS NAME CHAR*8
C               IPTAB(44) = RECTIFIER BASE KV CODE
C               IPTAB(45) = INVERTER BASE KV CODE
C               
                itmpsc(2*i-1) = bname(itab(35))
                itmpsc(2*i) = bname(itab(37))
                itemps(2*i-1) = iptab(44)
                itemps(2*i) = iptab(45)
                itempm(2*i-1) = 0
                itempm(2*i) = 0
C               
C               STORE MODULATION OUTPUT
C               GAMMA MODULATION STORED WITH INVERTER BUS
C               
                if (itab(114) .eq. 5) then
                  igam = itab(118)
                  itempm(2*i) = itab(114)
                endif

C               LOW LEVEL MODULATION

                if (itab(114) .eq. 1 .or. itab(114) .eq. 2) 
     &            itempm(2*i-1) = itab(114)

C               HIGH LEVEL MODULATION

                if (itab(114) .eq. 3 .or. itab(114) .eq. 4) 
     &            itempm(2*i-1) = itab(114)

C               DUAL FREQUENCY MODULATION

                if (itab(114) .eq. 6) itempm(2*i-1) = itab(114)
              enddo
C             
C             NDC1 = 2 TIMES THE NUMBER OF 2 TERM DC LINES
C             IT COUNTS THE NUMBER OF TERMINALS
C             
              ndc1 = ldc*2
              idch1 = ldc
            endif
C           
C           PREPARE TABLES FOR MULTI TERM DC OUTPUT TO .SOL FILE
C           
            if (ldc1 .ne. 0) then
              ldcc = 1
              jecs1 = kdc1
              do while (.true.)
                call redecs(dcb, jecs1, msizeb)
                call redecs(mdcen, necse, nbus)
C               
C               PROCESS TERMINAL DATA FIRST
C               
                do i = 1, nterm
                  i1 = lbt + i
                  call redecs(dca(1), idcb(i1), msizea)
                  call redecs(dcd, necsv, 2*nbus)
                  ndc1 = ndc1 + 1
                  idch(i) = ibus
                  itmpsc(ndc1) = dcec(ibus, ldcc)
                  itemps(ndc1) = mdcen(ibus)
                  itempm(ndc1) = idca(78)
                enddo
                idch1 = nterm
                ind = lbt + nterm
                do while (.true.)
                  ind = ind + 1
                  do while (ind .le. ndimc)
                    nb = idcb(ind)
                    if (nb .le. 0) goto 100
C                   
C                   ADJACENT BUS J
C                   TEST IF BUS J IS A TERMINAL
C                   
                    if (nb .gt. 10) goto 110
                    jbus = nb
C                   
C                   CHECK IF PROPER  ORDER   OF LOW TO HIGH
C                   
                    if (jbus .ge. mbus) then
                      idch(idch1+1) = mbus
                      idch(idch1+2) = jbus
                      idch1 = idch1 + 2
                      ndc2 = ndc2 + 1
                      itmp2c(2*ndc2-1) = dcec(mbus, ldcc)
                      itemp2(2*ndc2-1) = mdcen(mbus)
                      itmp2c(2*ndc2) = dcec(jbus, ldcc)
                      itemp2(2*ndc2) = mdcen(jbus)
                    endif
                    ind = ind + 8
                  enddo
                  goto 120
C                 
C                 NEW BUS
C                 
  100             mbus =  - nb
                  if (mbus .gt. 10) mbus = mbus - 10
  110             continue
                enddo
C               
C               WRITE INDIV. IDCH TABLE
C               
  120           call ritecs(idch, necsh, idch1)
                if (idch1 .ne. ndimh) then
                  write (errbuf(1), 10000) ldcc, idch1, ndimh
10000             format (
     &             '0 MULTITERM DISCREP IN IDCH DIMENSION IN SUBRT SWING
     &--LDCC,IDCH1,NDIMH', 3i10)
                  call prterr('E', 1)
                  call erexit()
                endif
C               
C               TALLY AND TEST END OF MULTDC CKT
C               
                ldcc = ldcc + 1
                if (ldcc .gt. ldc1) goto 130
                jecs1 = kdcm(ldcc)
              enddo
            endif
C           
C           WRITE HEADER RECORD FOR 2 TERM DC TO .SOL FILE
C           
  130       ndc3 = ndc2*2
            nn3 = ndc1*9
            nn2 = ndc2*3
            irectp = 97
            idesc = 97
            irecln = 2
            if (debug) then
              call dbgeko2('SOLN - writing DC counters to ',
     &         'history file.')
              call dbgwri('  IRECTP /record type/ = ', irectp)
              call dbgwri('  IDESC /rec descrip/  = ', idesc)
              call dbgwri('  IRECLN /rec length/  = ', irecln)
            endif
            call puthisi(irectp, 1)
            call puthisi(idesc, 1)
            call puthisi(irecln, 1)
            call puthisi(ndc1, 1)
            call puthisi(ndc2, 1)
            irectp = 103
            idesc = 256*ndc1 + 8
            irecln = 2*ndc1 + 1
            if (debug) then
              call dbgeko2('SOLN - writing 2-t DC bus names to ',
     &         'history file.')
              call dbgwri('  IRECTP /record type/ = ', irectp)
              call dbgwri('  IDESC /rec descrip/  = ', idesc)
              call dbgwri('  IRECLN /rec length/  = ', irecln)
            endif
            call puthisrc(irectp, idesc, irecln, itmpsc)
            irectp = 105
            idesc = 105
            irecln = ndc1
            if (debug) then
              call dbgeko2('SOLN - writing 2-t DC base KV codes to ',
     &         'history file.')
              call dbgwri('  IRECTP /record type/ = ', irectp)
              call dbgwri('  IDESC /rec descrip/  = ', idesc)
              call dbgwri('  IRECLN /rec length/  = ', irecln)
            endif
            call puthisri(irectp, idesc, irecln, itemps)
            irectp = 109
            idesc = 109
            irecln = ndc1
            if (debug) then
              call dbgeko2(
     &         'SOLN - writing 2-t DC term modultn codes to ',
     &         'history file.')
              call dbgwri('  IRECTP /record type/ = ', irectp)
              call dbgwri('  IDESC /rec descrip/  = ', idesc)
              call dbgwri('  IRECLN /rec length/  = ', irecln)
            endif
            call puthisri(irectp, idesc, irecln, itempm)
C           WRITE(L8) NDC1,NDC2,(ITMPSC(I), I=1, NDC1), (ITEMPS(I),
C           1              I = 1, NDC1),(ITEMPM(I),I= 1,NDC1)
            if (ndc2 .ne. 0) then
C             
C             WRITE HEADER RECORD FOR MULTI TERM DC TO .SOL FILE
C             
              irectp = 115
              idesc = 256*ndc3 + 8
              irecln = 2*ndc3 + 1
              if (debug) then
                call dbgeko2('SOLN - writing m-t DC bus names to ',
     &           'history file.')
                call dbgwri('  IRECTP /record type/ = ', irectp)
                call dbgwri('  IDESC /rec descrip/  = ', idesc)
                call dbgwri('  IRECLN /rec length/  = ', irecln)
              endif
              call puthisrc(irectp, idesc, irecln, itmp2c)
              irectp = 117
              idesc = 117
              irecln = ndc3
              if (debug) then
                call dbgeko2('SOLN - writing m-t DC base KV codes to ',
     &           'history file.')
                call dbgwri('  IRECTP /record type/ = ', irectp)
                call dbgwri('  IDESC /rec descrip/  = ', idesc)
                call dbgwri('  IRECLN /rec length/  = ', irecln)
              endif
              call puthisri(irectp, idesc, irecln, itemp2)
            endif
          endif
C             
C         WRITE HEADER RECORD FOR MULTI TERM DC TO .SOL FILE
C             
          irectp = 121
          idesc = 121
          irecln = 1
          if (debug) then
            call dbgeko2('SOLN - writing TCSC counters to ',
     &       'history file.')
            call dbgwri('  IRECTP /record type/ = ', irectp)
            call dbgwri('  IDESC /rec descrip/  = ', idesc)
            call dbgwri('  IRECLN /rec length/  = ', irecln)
          endif
          call puthisi(irectp, 1)
          call puthisi(idesc, 1)
          call puthisi(irecln, 1)
          call puthisi(iznmax, 1)
          if (iznmax .gt. 0) then
            irectp = 125
            idesc = 125
            irecln = 5*iznmax
            if (debug) then
              call dbgeko2('SOLN - writing TCSC codes to ',
     &         'history file.')
              call dbgwri('  IRECTP /record type/ = ', irectp)
              call dbgwri('  IDESC /rec descrip/  = ', idesc)
              call dbgwri('  IRECLN /rec length/  = ', irecln)
            endif
            do i = 1, iznmax
              if (ivymsw(i) .ne. 0) then
                itemp2(5*i-4) = 0
                itemp2(5*i-3) = 0
                itemp2(5*i-2) = 0
                itemp2(5*i-1) = 0
                itemp2(5*i) = 0
              else
                itemp2(5*i-4) = iznbus(i)
                itemp2(5*i-3) = jznbus(i)
                itemp2(5*i-2) = ichar(iznpar(i))
                itemp2(5*i-1) = iznsec(i)
                itemp2(5*i) = iranityp(i)
              endif
            enddo
            call puthisri(irectp, idesc, irecln, itemp2)
          endif

C         WRITE (L8) (ITMP2C(I), I=1, NDC3),(ITEMP2(I), I=1, NDC3)
C         READ TABLES FOR INITIAL OUTPUT...IGENT,EPQ,GOVPWR,GENP,
C         ANGL,ANGP1

          to = tsim
C         
C         CALL WRTHIS TO WRITE SOLUTION DATA TO HISTORY FILE FOR00
C         
          call wrthis()
C         
C         PREPARE INITIAL COND
C         
          imat4 = 1
          iswt = 1
          ist = 0
          icntl2 = 2
          lfrst = nmx + 1
          iijj1 = 1
          ibao = nmx + 1
          itpos1 = 1
          nchck = 2
          dnx1 = tsim
          tnx = tsim + dt
          icntl3 = 1
          al = 0.0
          isg3 = 0
C         
C         READ FAULT CARDS
C         
          if (cyc(1) .eq. tsim) then
C           
C          PREPARE ISTEM TABLE FOR INITIAL SWITCHING
C          TEST AND SETUP FOR DC PWR CHANGE
C           
            do while (.true.)
              if (mflt(iswt) .ne. 5 .or. ipcd .ne. 7) then
                ist = ist + 1
                it = iabs(iftab(1))
                jt = iabs(jftab(1))
                if ((it .lt. lfrst) .and. (it .gt. 0)) lfrst = it
                if ((jt .lt. lfrst) .and. (jt .gt. 0)) lfrst = jt
                istem(ist) = iswt
              else
                ndc = kdc
                do i = 1, ldc
                  call redecs(ptab, ndc, idcl)
                  if (.not. (itab(35) .ne. iftab(iswt) .or. itab(37)
     &             .ne. jftab(iswt))) goto 140
                  if (.not. (itab(35) .ne. jftab(iswt) .or. itab(37)
     &             .ne. iftab(iswt))) goto 140
                  ndc = ndc + idcl
                enddo
                goto 150
  140           ptab(31) = fltr(iswt)
                call ritecs(ptab(31), ndc+30, 1)
              endif
              iswt = iswt + 1
              if (iswt .gt. ifcd) cyc(iswt) = endt + dt
              if (cyc(iswt) .gt. tsim) goto 160
            enddo
  150       i825 = iabs(iftab(iswt))
            j825 = iabs(jftab(iswt))
            inamec = exnamc(i825)
            jnamec = exnamc(j825)
            write (errbuf(1), 10010) inamec, jnamec
            call prterr('E', 1)
10010       format ('0 FAULTY DC PORDER CHANGE CD SPECIFIED BY ', 5x,
     &       2a8)
            goto 200
  160       dnx = cyc(iswt)
            matsw = 3
            link = 2
          else
            dnx = cyc(1)
            jmx = nmx
            matsw = 2
            if (ipwr .eq. 1) then
              link = 2
            else
              matsw = 2
              ichnge = 4
              link = 2
            endif
          endif
        endif
      else
C       
C       FIRST PASS THROUGH SOLN, INITIALIZE VARIABLES, WRITE HEADE
C       TO THE .SOL FILE AND PREPARE TO REDUCE Y MATRIX
C       
        ifirst = 4
        jfirst = nmx + 1
C       
C       WRITE BUS NAMES (EXNAMC) AND BASE KV CODES (IXNAMN) TO .SO
C       
        irectp = 75
        idesc = 256*nmx + 8
        irecln = 2*nmx + 1
        if (debug) then
          call dbgeko2('SOLN - writing revised bus names to ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call puthisrc(irectp, idesc, irecln, exnamc)
        irectp = 77
        idesc = 77
        irecln = nmx
        if (debug) then
          call dbgeko2('SOLN - writing revised bus kv codes to ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call puthisri(irectp, idesc, irecln, ixnamn)
C       WRITE (L8) IFIRST,JFIRST,NMX,(EXNAMC(I),IXNAMN(I),I=1,NMX)
C       
C       WRITE GENERATOR IDENTIFYING INFORMATION TO .SOL FILE
C       
        irectp = 81
        idesc = 81
        irecln = 2*isg
        if (debug) then
          call dbgeko2('SOLN - writing gen bus nums & ECS pointers to '
     &     , 'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call puthisri(irectp, idesc, irecln, igentn)
        irectp = 91
        idesc = 256*isg + 1
        irecln = (isg/4) + 1
        if (debug) then
          call dbgeko2('SOLN - writing revised gen IDs to ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call puthisrc(irectp, idesc, irecln, igentc)

C       WRITE (L8) ((IGENTN(J,I),J=1,2),IGENTC(I),I=1,ISG)

        do i = 1, isg
          iecs = igentn(2, i)
          ind = (i-1)*11
          call redecs(datat, iecs, 14)
          mgen = igndta(1, i)
          if (mgen .gt. 1) then
            do j = 1, 4
              const(ind+j) = datat(j+5)
            enddo
            do j = 5, 8
              const(ind+j) = datat(j+6)
            enddo
            if (mgen .ge. 6) then
              call redecs(xdpp, iecs+27, 1)
              const(ind+11) = xdpp
            endif
            mex = igndta(5, i)
 
C           GET SATURATION FACTORS ESATX AND CSATX TO CALCULATE EXCITER
C           IN OUTPUT ROUTINE
 
            if (mex .lt. 11) then
              mexecs = igndta(6, i)
              if (.not. (mex .eq. 4 .or. (mex .ge. 7 .and. mex .le. 9))
     &         ) then
                call redecs(const(ind+9), mexecs+3, 2)
                goto 170
              endif
            elseif (.not. (mex .eq. 14 .or. mex .eq. 17)) then
              if (mex .le. 18) then
                const(ind+9) = citer(4, i)
                const(ind+10) = citer(5, i)
                goto 170
              endif
            endif
            const(ind+9) = 0.0
            const(ind+10) = 0.0
            if (mgen .eq. 2) const(ind+10) =  - 10000.
          else
            const(ind+1) = datat(6)
            const(ind+2) = datat(7)
            do j = 3, 11
              const(ind+j) = 0.0
            enddo
          endif
  170     continue
        enddo
        ind = 11*isg
        ifirst = 7
        irectp = 94
        idesc = 94
        irecln = ind
        if (debug) then
          call dbgeko2('SOLN - writing revised gen constants to ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call puthisrf(irectp, idesc, irecln, const)
C       WRITE (L8) IFIRST,IND,(CONST(I),I=1,IND)
C       
C       PREPARE INIT COND
C       
        dnxrly = endt
        idiskt = 0
        ispf = 0
        nacc = 2
        pwrtol = 0.005
        angtol = 0.00175
        lpqit = ilim
        ilim = 50
        lppwr = 0
        asg = isg
        asumt = 0.0005*asg
        fcons = 1.0/6.2831853
        idsw = 7
        ddt2 = dt
        ddt1 = dt
        tc1 = 0.0
        tc2 = 0.0
        tc3 = tsim
        ipass = 1
        lfrst = 1
        jexit = 2
        edt = dt
        do i = 1, ifcd
          dcyc = cyc(i) - tsim
          if (dcyc .gt. 0.001) goto 180
        enddo
        goto 190
  180   if (dcyc .lt. dt-0.001) edt = dcyc
  190   nchck = 3
C       
C       OBTAIN POWERFLOW SOLUTION VOLTS FOR COMPARSION TO SWING RE
C       CHANGED SOLN VOLTAGE CHECK TO BUS 2
 
        esoln = eyr(2)
        fsoln = eyi(2)
        bva = bmva*1000000.0
        jmx = nmx
        matsw = 2
        ichnge = 4
        link = 2
      endif
  200 return
      end
