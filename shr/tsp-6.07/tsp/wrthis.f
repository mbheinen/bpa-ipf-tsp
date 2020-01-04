C    %W% %G%
      subroutine wrthis
C     
C     This subroutine writes solution data to the history file FOR
C     It is called by SOLN and CNTRL.
C     
      include 'tspinc/params.inc'
      include 'tspinc/newton.inc'
      include 'tspinc/buslod.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/param.inc'
      include 'tspinc/ectba.inc'
      include 'tspinc/mdctbl.inc'
      include 'tspinc/gamma.inc'
      include 'tspinc/dcmodd.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'
      include 'tspinc/outaux.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/znox.inc'
      include 'tspinc/znox2.inc'
      include 'tspinc/vy1.inc'
      include 'tspinc/vym.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
      include 'tspinc/bcur.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/spare1.inc'
      include 'tspinc/vymn.inc'

C     Functions

      logical dbghere

C     Local variables

      dimension temps1(390), temps3(900)
      dimension tempn(2, 13)
      character*8 name, name1, name2
      character*1 id
      logical debug, first

      save 

      data radeg/57.29578/
      data debug/.true./
      data to_old, e_old, f_old / 3 * 0.0 /

      data first / .true./

C     
C     Begin   Begin   Begin   Begin   Begin   Begin   Begin   Begin
C     
      if (first) then

        write (98, 94) 'TimeBase=Cycles'
        write (98, 94) '8'
        write (98, 94) 'Time VNET_MAG VNET_ANG VMAC_MAG VMAC_ANG INET_MA
     &G INET_ANG IMAC_MAG'
   94   format (a)

        write (99, 94) 'TimeBase=Cycles'
        write (99, 94) '9'
        write (99, 94) 'Time IMAC_ANG FD FQ IND_FREQ SLIP PELECT PMECH F
     &REQ'
        first = .false.
      endif

      debug = dbghere('WRTHIS  ')
      if (debug) then
        call dbgeko('WRTHIS - at start')
        call dbgwrf('  TO /current time/ = ', to)
      endif
C     
C     CALCULATE BUS LOAD BUSP - REAL LOAD IN MW'S
C     BUSQ -  REACTIVE LOAD IN MVAR'S
C     
C     *** CHANGED BUS LOAD OPTION TEST FROM ISPSW TO IBLOD

      if (iblod .ne. 0) then

        do itrr = 1, nmx
          name = bname(itrr)
          curr =  - busp(itrr) + eyr(itrr)*yreal(itrr) - eyi(itrr)
     &     *yimag(itrr)
          curi =  - busq(itrr) + eyi(itrr)*yreal(itrr) + eyr(itrr)
     &     *yimag(itrr)
          busp(itrr) = (eyr(itrr)*curr+eyi(itrr)*curi)*bmva
          busq(itrr) = (curr*eyi(itrr)-eyr(itrr)*curi)*bmva
          if (keybrd(0) .ne. 0) then
            write (outbuf, 10000) name, busp(itrr), busq(itrr)
10000       format (5x, a8, 2(2x, f9.2))
            call prtout(1)
          endif
        enddo
      endif
      ifirst = 5
      jfirst = 1 + 6*isg
      if (iblod .ne. 0) jfirst = 1 + 7*isg
      if (ispsw .ne. 0) jfirst = 1 + 8*isg

C     ADDITIONAL CODE FOR BOTH IBLOD,ISPSW .NE. 0 

      if (iblod .ne. 0 .and. ispsw .ne. 0) jfirst = 1 + 9*isg
C     
C     Write generator data to solution file (using unformatted,
c     direct-access writes).

c     Record No. 130 = time step

      irectp = 130
      idesc = 130
      irecln = 1
      if (debug) then
        call dbgeko2('WRTHIS - saving simulation time to ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrf(irectp, idesc, irecln, to)

c     Record No. 134 = gen rotor angles

      irectp = 134
      idesc = 134
      irecln = isg
      if (debug) then
        call dbgeko2('WRTHIS - saving gen rotor angles to ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrf(irectp, idesc, irecln, angl)

c     Record No. 138 = gen frequency devs

      irectp = 138
      idesc = 138
      irecln = isg
      if (debug) then
        call dbgeko2('WRTHIS - saving gen rotor frequency devs to ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrf(irectp, idesc, irecln, angp2)

c     Record No. 142 = gen field voltages & currents

      irectp = 142
      idesc = 142
      irecln = isg*2
      if (debug) then
        call dbgeko2('WRTHIS - saving field volts & currents to ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrf(irectp, idesc, irecln, vfldtn)

c     Record No. 146 = gen mech power

      irectp = 146
      idesc = 146
      irecln = isg
      if (debug) then
        call dbgeko2('WRTHIS - saving gen P_mech to ', 'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrf(irectp, idesc, irecln, govpwr)

c     Record No. 150 = gen elec power 

      irectp = 150
      idesc = 150
      irecln = isg
      if (debug) then
        call dbgeko2('WRTHIS - saving gen P_elec to ', 'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrf(irectp, idesc, irecln, genp)

c     Record No. 10150 = gen flux

      irectp = 10150
      idesc = 10150
      irecln = isg
      if (debug) then
        call dbgeko2('WRTHIS - saving gen flux ', 'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrf(irectp, idesc, irecln, epq)

c     Record No. 154 = gen terminal current

      irectp = 154
      idesc = 154
      irecln = isg*2
      if (debug) then
        call dbgeko2('WRTHIS - saving gen term current (R & I) to ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrf(irectp, idesc, irecln, curn)

c     Record No. 158 = gen voltage reg output

      irectp = 158
      idesc = 158
      irecln = isg
      if (debug) then
        call dbgeko2('WRTHIS - saving gen voltage reg output to ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrf(irectp, idesc, irecln, regout)

c     Record No. 162 = gen PSS output

      irectp = 162
      idesc = 162
      irecln = isg
      if (debug) then
        call dbgeko2('WRTHIS - saving gen PSS output to ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrf(irectp, idesc, irecln, supout)
      if (iblod .ne. 0) then

c       Record No. 166 = bus loads (real)

        irectp = 166
        idesc = 166
        irecln = nmx
        if (debug) then
          call dbgeko2('WRTHIS - saving bus loads (real) to ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call puthisrf(irectp, idesc, irecln, busp)

c       Record No. 170 = bus loads (reactive)

        irectp = 170
        idesc = 170
        irecln = nmx
        if (debug) then
          call dbgeko2('WRTHIS - saving bus loads (imag) to ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call puthisrf(irectp, idesc, irecln, busq)
      endif
      if (ispsw .ne. 0) then

c       Record No. 258 = spare points

        irectp = 258
        idesc = 258
        irecln = MAXSP
        if (debug) then
          call dbgeko2('WRTHIS - saving spare points to ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call puthisrf(irectp, idesc, irecln, sparpt)
C       WRITE(L8) (SPARPT(I),I=1,MAXSP)
      endif

c     Record No. 174 = bus voltages (real)

      irectp = 174
      idesc = 174
      irecln = nmx
      if (debug) then
        call dbgeko2('WRTHIS - saving bus voltage (real) to ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrf(irectp, idesc, irecln, eyr)

c     Record No. 178 = bus voltages (imaginary)

      irectp = 178
      idesc = 178
      irecln = nmx
      if (debug) then
        call dbgeko2('WRTHIS - saving bus voltage (imag) to ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrf(irectp, idesc, irecln, eyi)
C     
C     Output variable admittance values (Printer only)
C     
      if (iznmax .gt. 0 .and. keybrd(34) .ne. 0) then
        do i = 1, iznmax
          name1 = bname(iznbus(i))
          bkv1 = buskv(iznbus(i))
          name2 = bname(jznbus(i))
          bkv2 = buskv(jznbus(i))
          id = iznpar(i)
          alfa = (vyalfi(i)+vyalfa(i))*radeg
          if (iznosw(i) .ne. 1) then
            write (outbuf, 10010) name1, bkv1, name2, bkv2, id, alfa,
     &       zngij(i), znbij(i), vymmd(i), to
10010       format ('0', 5x, 2(a8, 1x, f4.0, 1x), a1, ' ALPHA = ',
     &       f6.2, ' GIJ = ', f9.4, ' BIJ = ', f9.4, 2x, ' MOD SIG = ',
     &       f9.4, ' AT ', f7.2, ' CYCLES ')
            call prtout(1)
          endif
          if (iznosw(i) .ne. 3) then
            recp2 = 1./(zngc(i)*zngc(i)+znbc(i)*znbc(i))
            rtot = zngc(i)*recp2
            xtot =  - znbc(i)*recp2
            write (outbuf, 10020) name1, bkv1, name2, bkv2, id, rtot,
     &       xtot, to
10020       format ('0', 5x, 2(a8, 1x, f4.0, 1x), a1, 1x, ' ZNO R = ',
     &       f9.4, ' ZNO X = ', f9.4, 'AT ', f7.2, ' CYCLES ')
            call prtout(1)
          endif
        enddo
      endif
C     
C     Output dc quantities (Printer only)
C     
      if (.not. (ldc .eq. 0 .and. ldc1 .eq. 0)) then
        i784 = 1
        idc1 = 0
        idc2 = 0
        if (ldc .ne. 0) then
          i1 = 0
          do i = 1, ldc
            i2 = (i-1)*idcl + kdc
            call redecs(ptab(1), i2, 252)
            if (.not. (itab(114) .lt. 1 .or. itab(114) .eq. 5)) then
              imod = itab(118)
              if (keybrd(22) .ne. 0) then
                if (itab(114) .eq. 6) then
                  write (outbuf, 10030) to, x1nmod(imod), y1nmod(imod),
     &             sighim(imod)
                  call prtout(1)
10030             format ('0', 5x, 'TIME = ', f7.2, 5x, 'MOD1 INPUT ',
     &             e13.6, 5x, 'MOD2 INPUT ', e13.6, 5x, 'DUAL MOD OUT '
     &             , e13.6)
                else
                  write (outbuf, 10040) to, xoomod(imod), siglom(imod),
     &             sighim(imod)
                  call prtout(1)
                endif
10040           format ('0', 5x, 'TIME = ', f7.2, 5x, 'MOD INPUT ',
     &           e13.6, 5x, 'LOW LEVEL OUTPUT ', e13.6, 5x,
     &           'HIGH LEVEL OUTPUT ', e13.6)
              endif
            endif
C           
C           ITEMPSC(I) = RECTIFIER BUS NAME CHAR*8
C           ITEMPSC(I+1) = INVERTER BUS NAME CHAR*8
C           IPTAB(44) = RECTIFIER BASE KV CODE
C           IPTAB(45) = INVERTER BASE KV CODE
C           TAB(95) = COS ALPHA FOR RECTIFIER
C           TAB(87) = IDC FOR RECTIFIER
C           TAB(110) = VDC FOR RECTIFIER
C           TAB(157) = IDC FOR INVERTER
C           TAB(111) = VDC FOR INVERTER
C           ITAB(114) IS THE MODULATION CODE
C           TAB(121) IS THE EXTINCTION ANGLE FOR THE RECTIFIER
C           TAB(122) IS THE EXTINCTION ANGLE FOR THE INVERTER
C           TAB(115) IS V ALPHA FOR THE RECTIFIER
C           TAB(116) IS V ALPHA FOR THE INVERTER
C           TAB(78) IS V ALPHA PRIME FOR THE RECTIFIER
C           TAB(84) IS V ALPHA PRIME FOR THE INVERTER
C           
            temps1(18*i-17) = tab(95)
            temps1(18*i-16) = tab(87)
            temps1(18*i-15) = tab(110)
            temps1(18*i-14) = ptab(25)
            temps1(18*i-13) = tab(121)
            temps1(18*i-12) = tab(115)
            temps1(18*i-11) = tab(78)
            temps1(18*i-10) = 0.0
            temps1(18*i-9) = 0.0
            temps1(18*i-8) = tab(96)
            temps1(18*i-7) = tab(157)
            temps1(18*i-6) = tab(111)
            temps1(18*i-5) = ptab(26)
            temps1(18*i-4) = tab(122)
            temps1(18*i-3) = tab(116)
            temps1(18*i-2) = tab(84)
            temps1(18*i-1) = 0.0
            temps1(18*i) = 0.0
C           
C           PRINT WARNING IF ANGLE OF OVERLAP EXCEEDS 60 DEGREES
C           
            aover = (temps1(18*i-13)-acos(tab(95)))*57.29578
            if (aover .gt. 60.) then
              ibno = itab(35)
              name = bname(ibno)
              bkv = buskv(ibno)
              write (errbuf(1), 10050) name, bkv, aover
10050         format ('0', 2x, 'ANGLE OF OVERLAP AT ', a8, 1x, f6.2,
     &         ' EXCEEDS 60', ' DEGREES.  ANGLE =  ', f6.2)
              call prterr('W', 1)
            endif
            aover = (temps1(18*i-4)-acos(tab(96)))*57.29578
            if (aover .gt. 60.) then
              ibno = itab(37)
              name = bname(ibno)
              bkv = buskv(ibno)
              write (errbuf(1), 10050) name, bkv, aover
              call prterr('W', 1)
            endif
C           
C           STORE MODULATION OUTPUT
C           GAMMA MODULATION STORED WITH INVERTER BUS
C           
            if (itab(114) .eq. 5) then
              igam = itab(118)
              temps1(18*i-5) = gama(igam)
            endif

C           LOW LEVEL MODULATION

            if (itab(114) .eq. 1 .or. itab(114) .eq. 2) 
     &       temps1(18*i-14) = siglo

C           HIGH LEVEL MODULATION

            if (itab(114) .eq. 3 .or. itab(114) .eq. 4) 
     &       temps1(18*i-14) = sighi

C           DUAL FREQUENCY MODULATION

            if (itab(114) .eq. 6) temps1(18*i-14) = tab(177)
          enddo
          idc1 = ldc*18
        endif
        if (ldc1 .ne. 0) then
          ldcc = 1
          n999 =  - 999
          jecs1 = kdc1
          do while (.true.)
            call redecs(dcb, jecs1, lbt)
 
C           INITIALIZE NFLT FOR FLT  DC BKR TRIP CODE IF REQ
 
            if (nflt .eq. 999) then
              kflt = 0
              call ritecs(kflt, jecs1+7, 1)
              call ritecs(n999, jecs1+13, 1)
            endif
            ndimj = 3*(ndimh-nterm)/2 + 9*nterm
            call redecs(dcj, necsj, ndimj)

C           PROCESS TERMINAL DATA FIRST

            nterm3 = 9*nterm
            do i = 1, nterm3
              temps1(idc1+i) = dcj(i)
            enddo
            idc1 = idc1 + nterm3

C           PROCESS DC BRANCH DATA

            i1 = 0
            nterm3 = nterm3 + 1
            do i = nterm3, ndimj
              i1 = i1 + 1
              temps3(idc2+i1) = dcj(i)
            enddo
            idc2 = idc2 + ndimj - nterm3 + 1
C           
C           LOOP THROUGH TERMINALS AND OUTPUT ANY MODULATION SIGNA
C           
            if (keybrd(22) .ne. 0) then
              ind = 0
              do while (.true.)
                ind = ind + 1
                if (ind .gt. nterm) goto 100
                ind1 = lbt + ind
                call redecs(dca, idcb(ind1), msizea)
C               
C               PRINT WARNING IF ANGLE OF OVERLAP EXCEEDS 60 DEGRE
C               
                itrr = 9*(ind-1)
                aover = (dcj(itrr+5)-acos(dcj(itrr+1)))*57.29578
                if (aover .gt. 60.) then
                  ibno = idca(46)
                  name = bname(ibno)
                  bkv = buskv(ibno)
                  write (errbuf(1), 10050) name, bkv, aover
                  call prterr('W', 1)
                endif
                modcod = idca(78)
                if (.not. (modcod .lt. 1 .or. modcod .eq. 5)) then
                  imod = idca(79)
                  if (modcod .eq. 6) then
                    write (outbuf, 10030) to, x1nmod(imod), y1nmod
     &               (imod), sighim(imod)
                    call prtout(1)
                  else
                    write (outbuf, 10040) to, xoomod(imod), siglom
     &               (imod), sighim(imod)
                    call prtout(1)
                  endif
                endif
              enddo
            endif
C           
C           TALLY AND TEST END OF MULTDC CKT
C           
  100       ldcc = ldcc + 1
            if (ldcc .gt. ldc1) goto 110
            jecs1 = kdcm(ldcc)
          enddo
        endif
C       
C       WRITE VARIABLE ADMITTANCE FIRING ANGLE OVER DC DATA
C       
  110   if (iznmax .gt. 0) then
          temps1(1) = cos(vyalfa(1)+vyalfi(1))
          temps1(2) = vymmd(1)
        endif
 
C       Record No. 182 = D-C Bus data
 
        irectp = 182
        idesc = 182
        irecln = idc1
        if (debug) then
          call dbgeko2('WRTHIS - saving DC bus data to ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call puthisrf(irectp, idesc, irecln, temps1)
 
C       Record No. 186 = D-C Bus data
 
        if (idc2 .ne. 0) then
          irectp = 186
          idesc = 186
          irecln = idc2
          if (debug) then
            call dbgeko2('WRTHIS - saving DC branch data to ',
     &       'history file.')
            call dbgwri('  IRECTP /record type/ = ', irectp)
            call dbgwri('  IDESC /rec descrip/  = ', idesc)
            call dbgwri('  IRECLN /rec length/  = ', irecln)
          endif
          call puthisrf(irectp, idesc, irecln, temps3)
C         WRITE(L8)(TEMPS3(I),I=1,IDC2)
        endif
      endif
 
C     Record No. 190 = TCSC data points
     
      if (iznmax .ne. 0) then
        irectp = 190
        idesc = 190
        irecln = 2*iznmax
        if (debug) then
          call dbgeko2('WRTHIS - saving TCSC data to ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        do i = 1, iznmax
          if (vyalfi(i) .gt. 0.0 .and. iranityp(i) .eq. 3) then
            zbase = buskv(iznbus(i))**2 / bmva
            temps1(2*i-1) = -xtcscge(i) * zbase
            temps1(2*i) = dangl(i) * 57.2957795
          else
            temps1(2*i-1) = 0.0
            temps1(2*i) = 0.0
          endif
        enddo
        call puthisrf(irectp, idesc, irecln, temps1)
      endif

C     Write an end-of-file marker in case this is the last time ste

      call puthisw(-1)

      return
      end
