C    %W% %G%
      subroutine tapewk 
C    
C     This subroutine reads labels on the power flow solution
C     history file (FOR003) and the saved data file (FOR009)
C     to insure that they are the proper files for the case.
C     It also reads initial data from the power flow history
C     file.  Called by INPUT1.
C    
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/param.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/in1n.inc'
      include 'tspinc/ffcard.inc'
      include 'tspinc/areanz.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/titles.inc'
      include 'tspinc/amorts.inc'
      include 'tspinc/files.inc'

      common /lqiks/lqiks
      character*10 machin(8, 100), lsrcp(8, 100), lrelay(8, 100), 
     &             lrrod(8, 100), work(8, 100), jwork(8, 100), 
     &             lrepdt(8, 100), aname, lshed(8, 100), dccard(8, 150)

      external komp31, swap31

C     -  Functions
      logical dbghere

C     -  Local variables
      character*8 plotd
      character*10 auxout
      dimension nslkxx(4, 10), basesd(MAXKV), kount(100), temp(4)
      integer count(100)
      character*10 pfcas, clbel1, clbel2, pversn, usrnam
      character cha*12, chb*12, chc*12
      logical debug
C     -
C     -    Begin       Begin       Begin       Begin       Begin
C     -
      debug = dbghere('TAPEWK  ')
      call mpost('TAPEWK')
      blnk10 = '          '
C     -   Fetch default damper data from the "CASE" card.
      read (buffer, 10000) pfcase, itskp, noprnt, iwscc, savin, savout,
     & xfact, (temp(i), i = 1, 4)
10000 format (bz, 5x, a10, 1x, i1, 2x, i1, 2x, i1, 1x, 2a10, 5x, 5f5.5)
C     -  Force case names to upper case
      call upcase(pfcase)
      call upcase(savin)
      call upcase(savout)
      idampr = 0
      iskkt = 0
      if (iwscc .gt. 0) then
        write (errbuf(1), 10010)
10010   format ('0 PROGRAM WILL READ NEW WSCC FORMAT OR BPA FORMAT ',
     &   'FOR UNDERFREQUENCY LOAD SHED DATA.')
        call prterr('W', 1)
      endif
C     -    XFACT is mult of X'_d for X"_d & X"_q for deflt amort data
      if (xfact .gt. 0.) then
        if (xfact .ge. 1.) then
          idampr = 1
          write (errbuf(1), 10020)
          call prterr('E', 1)
10020     format ('0', ' GENERALIZED DAMPER WINDING DATA- XFACT IS ',
     &     'GREATER THAN OR EQUAL TO ONE.')
        endif
C       -   Check range of damper times & convert to inverse cycles.
C       -      data thusly:
C       -      1: T"_do_round    2: T"_qo_round
C       -      3: T"_do_salient  4: T"_qo_salient
        do i = 1, 4
          if (temp(i) .lt. .01) temp(i) = 0.0
          if (temp(i) .ne. 0.0) temp(i) = 1./(frqbse*temp(i))
C         IF (TEMP(I).NE.0.0) TEMP(I) = 1./(60.*TEMP(I))
        enddo
        tdodps = temp(1)
        tqodps = temp(2)
        tdodph = temp(3)
        tqodph = temp(4)
        if (tdodps .eq. 0.0 .or. tdodph .eq. 0.0) then
          write (errbuf(1), 10030)
          call prterr('E', 1)
10030     format ('0', ' GENERALIZED DAMPER WINDING DATA- ONE OR BOTH '
     &     , 'TDO" VALUES ARE ZERO.')
          idampr = 1
        else
          if (tqodps .le. 0.0 .or. tqodph .le. 0.0) then
            idampr = 1
            write (errbuf(1), 10040)
            call prterr('E', 1)
10040       format ('0',
     &       ' BOTH TQO"  IN THE GENERALIZED DAMPER WINDING ',
     &       'DATA MUST BE GREATER THAN ZERO.')
          endif
          if (tdodps .le. tqodps .or. tdodph .le. tqodph) then
            idampr = 1
            write (errbuf(1), 10050)
            call prterr('E', 1)
10050       format ('0',
     &       ' GENERALIZED DAMPER WINDING DATA- ONE OR BOTH ',
     &       'TDO" ARE GREATER THAN CORROSPONDING TQO". ')
          endif
        endif
        if (idampr .ne. 0) then
          write (errbuf(1), 10060)
          call prterr('W', 1)
10060     format ('0', ' GENERALIZED DAMPER WINDING DATA IS IGNORED')
          xfact =  - 1.
        endif
      endif
C    
C     IF IREUSE=0, THEN CASE IS A BASE CASE
C     IF IREUSE =1, THEN CASE IS A CHANGE CASE
      ireuse = 0
      if (savin .ne. blnk10) ireuse = 1
C     -   Fetch Pwrflo admin data.  Look for case ID match only on
C     first header record (no loop-back for 2nd or later cases).
c dlc      read (l3, end = 100) auxout, pfcas, pfdate, clbel1, clbel2,
c dlc    & pversn, usrnam, count
C     write (*,'(a)') ' TAPEWK - dbg - SN 120: display PF header '
C     write (*,'(3x,8i4)') (ichar (auxout(la:la)), la = 1,8)
c dlc      if (auxout .ne. 'PF DATA') then
c dlc        write (errbuf(1), 10070)
c dlc 10070   format (' TAPEWK - Powerflow base file has improper',
c dlc      &   ' header.')
c dlc        call prterr('E', 1)
C       180    FORMAT(5X,'THE FILE ASSIGNED AS THE POWER FLOW DATA HISTO
C       1      ' (.BSE) IS NOT FROM THE POWER FLOW.')
C       ERRBUF(1) = ' POWERFLOW NOT FOUND'
C       CALL PRTERR ('E',1)
c dlc        call erexit()
c dlc      endif
C     -   PFCAS is from PF base file.  Force to upper case.
c dlc      call upcase(pfcas)
C     IF (PFCAS.EQ.PFCASE) GO TO 250
C     IF (PFCAS.EQ.'END') THEN
c dlc      if (pfcas .ne. ' ') then
c dlc        if (pfcas .ne. pfcase) then
c dlc          write (errbuf(1), '(2A)') 'TAPEWK - Requested PF case not ',
c dlc     &     'found in PF base file.'
c dlc          call prterr('E', 1)
c dlc          write (errbuf(1), 10080) pfcase, pfcas
c dlc 10080     format ('  Stab requested ', a10, '.  Base file has ', a10,
c dlc     &     '.')
c dlc          call prterr('E', 1)
C         220    FORMAT ('  CASE ( ',A10,' ) IS NOT ON THE POWER FLOW DA
C         1           'HISTORY FILE. (',A10,')'  )
c dlc          errbuf(1) = ' POWERFLOW NOT FOUND'
c dlc          call prterr('E', 1)
c dlc          call erexit()
c dlc        endif
c dlc      endif
c dlc      goto 110
C     GO TO 120
c dlc  100 errbuf(1) = 'TAPEWK - premature end-of-file in PF base.'
C     240 ERRBUF(1) = ' POWERFLOW NOT FOUND'
c dlc      call prterr('E', 1)
c dlc      call erexit()
C     250 IF(COUNT(1).EQ.1) THEN
C     GO TO 300
c dlc  110 if (count(1) .ne. 1) then
c dlc        write (errbuf(1), 10090)
c dlc        call prterr('E', 1)
c dlc 10090   format (' THE POWER FLOW SOLUTION FAILED.')
c dlc        errbuf(1) = ' POWERFLOW FAILED '
c dlc        call prterr('E', 1)
c dlc        call erexit()
c dlc      endif
      plotd = 'VRSND001'
      target = 0.0
C     -  Write intro record to history file
      irecln = 8
      call puthisi(0, 1)
      call puthisc('STAB', 1)
      call puthisi(irecln, 1)
      if (debug) then
        call dbgeko('TAPEWK - writing case names to history file.')
        call dbgwri('  IRECTP /record type/ = ', 0)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
        call dbgwrc('  PLOTD /hist file version/ = ', plotd)
      endif
      call puthisc(plotd, 2)
      call puthisc(pfcase, 3)
      call puthisc(swcase, 3)
C     WRITE(L8) PLOTD,PFCASE,SWCASE
C     -
      write (outbuf, 10100) pfcase
      call prtout(1)
10100 format ('0', '  POWER FLOW CASE ( ', a10,
     & ' ) IS BEING PROCESSED.')
 
C     END OF LOGIC TO LOCATE THE PARTICULAR CASE
 
C     READ LIST OF VARIABLES FROM POWER FLOW TAPE
 
c dlc      read (l3) ntot, ntot2, ltot, ntota, ntotb, ntotc, kdtot, mtdcbs,
c dlc     & mtdcln, kxtot, bmva, jtie, kecsy, kbsknt, kbrknt, jphno, nztot,
c dlc     & natot, ntotcs, nbslck, nslkxx, kount
c dlc      kslloc = nslkxx(1, 1)
c dlc      kslack = nslkxx(2, 1)
c dlc      kecsym = kecsy
c dlc      kmdcbs = mtdcbs
c dlc      kmdcbr = mtdcln
C     *
C     * CHECK POWER FLOW VERSION NUMBER
C     *
c dlc      if (ntotcs .ne. 0 .and. kmdcbs .ne. 0) then
c dlc        write (errbuf(1), 10110)
c dlc        write (errbuf(2), 10120)
c dlc        call prterr('E', 2)
c dlc 10110   format ('0', 'DC DATA ERRORS IN POWER FLOW HISTORY FILE')
c dlc 10120   format (' POWER FLOW IS UNACCEPTABLE FOR TRANSIENT STABLILITY '
c dlc     &   , 'STUDIES.')
c dlc        write (errbuf(1), 10130)
c dlc        call prterr('E', 1)
c dlc 10130   format ('0', 'POWER FLOW HISTORY FILE CONTAINS  MULTI-TERMINAL'
c dlc     &   , ' DC DATA WHICH IS NOT ACCEPTABLE.')
c dlc        errbuf(1) = ' BAD DC DATA'
c dlc        call prterr('E', 1)
c dlc        call erexit()
c dlc      endif
c dlc      if (ntot .gt. maxbus) then
c dlc        write (errbuf(1), 10140) ntot, maxbus
c dlc        call prterr('E', 1)
c dlc 10140   format ('BUS LIMIT VIOLATION DETECTED IN TAPEWK.  THIS CASE ',
c dlc     &   'HAS ', i5, ' BUSES BUT PROGRAM PERMITS ONLY ', i5)
c dlc        call erexit()
c dlc      endif
c dlc      if (bmva .eq. 0.0) bmva = 100.0
c dlc      ntotd = ntot
c dlc      lphase = jphno
c dlc      kecsx = 80000
c dlc      kecst = kecsx
c dlc      kzrecs = 1
 
 
C     CONVERT BASE KV TO A CODE AND THEN COMBINE THE BUS NAME AND
C     BASE CODE.  BUS NAME IS LEFT 48 BITS,BASE CODE IS RIGHT 12 BITS
 
c dlc      read (l3) (exnamc(i), i = 1, ntotd)
c dlc       read (l3) (exbase(i), i = 1, ntotd)
c dlc      read (l3) (exzonc(i), i = 1, ntot)
c dlc      read (l3) junk
c dlc      ibxyz = 0
c dlc      jdelet = 0
c dlc      do i = 1, ntotd
c dlc        xbase = exbase(i)
c dlc        if (ibxyz .ne. 0) then
c dlc          do k = 1, ibxyz
c dlc            if (basekv(k) .eq. xbase) goto 120
c dlc          enddo
c dlc        endif
c dlc        ibxyz = ibxyz + 1
c dlc        basekv(ibxyz) = xbase
c dlc  120   continue
c dlc      enddo
      if (ireuse .ne. 0) then
        call opents(3)
C       -   Fetch intro data from saved data file
        do while (.true.)
          read (l9, end = 130) swdata, swcas, svdate
          call upcase(swdata)
          call upcase(swcas)
          if (swdata .ne. 'SWING DATA') then
            write (errbuf(1), 10150)
            call prterr('E', 1)
10150       format (' THE FILE ATTACHED AS THE SAVED DATA FILE IS NOT',
     &       ' FROM THE TRANSIENT STABILITY FILE.')
            errbuf(1) = ' BAD SAVED DATA'
            call prterr('E', 1)
            call erexit()
          endif
          target = 0.0
          if (swcas .eq. savin) goto 140
          if (swcas .eq. xend) goto 130
        enddo
  130   write (errbuf(1), 10160) savin
        call prterr('E', 1)
10160   format (' SWING CASE (', a10, ') IS NOT AMOUNG THE BASE CASES.'
     &   )
        errbuf(1) = ' SAVIN CASE NOT FOUND'
        call prterr('E', 1)
        call erexit()
  140   call skipln(1)
        write (outbuf, 10170) savin, svdate
        call prtout(1)
10170   format ('0', 2x, 'SWING SAVED DATA FILE(', a10, ') CREATED ON '
     &   , a10, ' IS BEING PROCESSED')
C       -  Fetch set of base kv's of machines & buses in sav_dat file
        read (l9) kntsd, (basesd(i), i = 1, kntsd)
        if (kntsd .gt. MAXKV) then
          iabort = 1
          write (errbuf(1), '(2A)') 'In TAPEWK - ',
     &     ' Too many unique base voltages in save data file.'
          write (errbuf(2), '(A,I4,A,I4)') '    Allowed: ', MAXKV,
     &     ';  Used: ', kntsd
          call prterr('E', 2)
        endif
C       -   Any new stability KV's (sorted set) are appended to set of
C       -     PF KV's (unsorted set)
        ixtra = 0
        do i = 1, kntsd
          base = basesd(i)
          do j = 1, ibxyz
            if (base .eq. basekv(j)) goto 150
          enddo
          ixtra = ixtra + 1
          basekv(ibxyz+ixtra) = base
  150     continue
        enddo
        ibxyz = ibxyz + ixtra
        if (ibxyz .gt. MAXKV) then
          iabort = 1
          write (errbuf(1), '(A)') 'Base KV table size exceeded.',
     &     '  Allowed: ', MAXKV, ';  Used: ', ibxyz
          call prterr('E', 1)
        endif
      endif
      lqiks = 1
      call qiksrt(1, ibxyz, komp31, swap31)
      do i = 1, ntotd
        kb = nambas(exbase(i))
        ixnamn(i) = kb
        call endo()
      enddo
      irectp = 19
      idesc = 256*ntot + 8
      irecln = 2*ntot + 1
      if (debug) then
        call dbgeko2('TAPEWK - writing bus names to ', 'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisi(irectp, 1)
      call puthisi(idesc, 1)
      call puthisi(irecln, 1)
      call puthisc(exnamc(1), irecln)
      irectp = 21
      idesc = 21
      irecln = ntot
      if (debug) then
        call dbgeko2('TAPEWK - writing bus KV codes to ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisi(irectp, 1)
      call puthisi(idesc, 1)
      call puthisi(irecln, 1)
      call puthisi(ixnamn(1), irecln)
C     WRITE (L8) NTOT,(EXNAMC(I),I=1,NTOT),
C     1          (IXNAMN(I),I=1,NTOT)
      irectp = 26
      idesc = 26
      irecln = ibxyz
      if (debug) then
        call dbgeko2('TAPEWK - writing KV code list to ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisi(irectp, 1)
      call puthisi(idesc, 1)
      call puthisi(irecln, 1)
      call puthisf(basekv(1), irecln)
C     WRITE (L8) IBXYZ,(BASEKV(I),I=1,IBXYZ)
c dlc      do i = 1, maxbus
c dlc        exbase(i) = 0.0
c dlc      enddo
c dlc      call ritecs(exbase, kzrecs, 2100)
c dlc      num = (ntot+99)/100 + (ntot2+99)/100
c dlc      do i = 1, num
c dlc        read (l3) junk
c dlc      enddo
c dlc      if (ntotc .ne. 0) then
c dlc       read (l3) (areanc(i), i = 1, ntotc)
c dlc        read (l3) junk
c dlc        read (l3) junk
c dlc       read (l3) junk
c dlc        read (l3) ((areazc(i, j), i = 1, 10), j = 1, ntotc)
c dlc        read (l3) junk
c dlc        read (l3) junk
c dlc        read (l3) junk
c dlc        read (l3) junk
c dlc      endif
      return
      end
