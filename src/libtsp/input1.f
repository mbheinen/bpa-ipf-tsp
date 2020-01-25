C    %W% %G%
      subroutine input1
C     
C     This subroutine reads all input data cards starting with the
C     first comment card and ending with the 'FF' card.  This makes
C     up the entire set of cards in the /SIMDAT section.  It performs
C     initial error checking, sorts the card images alphabeticaly,
C     and stores them in tables.  It calls TAPEWK, SAVFIL, and SDATA.
C     It is called by SWINGM.
C     _
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/toler.inc'
      include 'tspinc/param.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/ecstbd.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/brake1.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/ecio.inc'
      include 'tspinc/in1n.inc'
      include 'tspinc/in1c.inc'
      include 'tspinc/lnet.inc'
      include 'tspinc/vtchkc.inc'
      include 'tspinc/packtn.inc'
      include 'tspinc/sort.inc'
      include 'tspinc/znox.inc'
      include 'tspinc/znox2.inc'
      include 'tspinc/fltopt.inc'
      include 'tspinc/cf1.inc'
      include 'tspinc/ffcard.inc'
      include 'tspinc/amorts.inc'
      include 'tspinc/brakn.inc'
      include 'tspinc/titles.inc'
      include 'tspinc/buslod.inc'
      include 'tspinc/tzro.inc'
      include 'tspinc/deltfq.inc'
      include 'tspinc/spare1.inc'
      include 'tspinc/lscrd.inc'

      common /areac2/areac2(60)
      character*10 areac2

      common /satsw/satsw

      common /lqiks/lqiks

      common /start/iswt1, iswt2, iswt3, iswt4, kswt1, kswt2

C     -  Local variables

      integer TEMP_ECSADDR
      parameter (TEMP_ECSADDR = 2101)

      dimension msort(225), temp(45), ktemp(45)
      equivalence (temp, ktemp)

      equivalence (sater, dtc), (isg2, istp)

      character*1 minus, x, typex, subtp, subtpx, crlytp, kodec, type, 
     &            subtyp, kchgcd, id, id1, id2, m1c, m2c, pori, iparc, 
     &            idx, sstyp
      character*2 zone, fulltyp
      character*3 dmpmlc
      character*5 finsc, plotd
      character*7 c7
      character*8 i1c, j1c, name2x, name3x, name4x, colmn4, namec, 
     &            name1c, name2c, name1k, name2k, name3k, name4k, 
     &            bus1c, bus2c, bus3c, bus4c, bus5c, bus6c
      character*10 machin(8, 100), lsrcp(8, 100), lrelay(8, 100), 
     &             lrrod(8, 100), lrepdt(8, 100), aname, lshed(8, 100),
     &             dccard(8, 150), jwork(8, 100), work(8, 100),
     &             typeb, c10, relayf, junk, remotf, serief, capcpf, 
     &             loadf, reprf, shedf, machnf, drectf, curntf, lnetf, 
     &             dat, area, zones, busbas

      character*80 work80(100), jwrk80(100), casecrd, ch80
      character tempcc8(8)*10, tempcc*80
      equivalence (tempcc8, tempcc)

      equivalence (jwork, jwrk80)
      equivalence (work, work80)

      external komp31, swap31
      logical debug, skipread

C     -  Functions
      logical dbghere

C     -  Local variables
      data plotd/'PLOTT'/
      data minus/'-'/

C     Code to initialize /TITLE/ items moved to STABDAT
C     
      data typeb /'BBBBBBBBBB'/
 
C     Begin     Begin     Begin     Begin     Begin     Begin

      debug = dbghere('INPUT1  ')

C      At this point, we are processing either the "CASE" or the
C     "/CASE" card.
C     
C     SAVE 'CASE' CARD IMAGE IN CASECRD
C     
      casecrd = buffer

C     BUGGER = BUFFER
C     -  Maximum number of by-bus load rep cards allowed (LBUS array
C     -  in INPUT2)
      mxlrep = MAXBUS/5
C     -
      skipread = .false.
      satsw = 0.0
C     
C     Tapewk interprets what's on the CASE card & loads the base
C     powerflow solution.
c dlc move to swingm
c dlc      call tapewk()
C     
      call mpost('INPUT1')
      call xdate(swdate)
      write (ch80, 10000) swcase, swdate, ver
C     WRITE(BUFFER,140) SCASE,SWDATE,VER
10000 format (2x, 'SWING CASE ', a10, ' EXECUTED ON ', a9,
     & ' USING VERSION ', a13)
      iswdte = 1
      ifirst = 1
      jfirst = 8
      inew = 0
      write (outbuf, 10040) ch80
      call prtout(1)
      irectp = 35
      idesc = 256 + 80
      irecln = 20
      if (debug) then
        call dbgeko2('INPUT1 - writing stab case/date/stab_vrsn ',
     &   'to history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
        call dbgwrc('  Partial card written = ', ch80(1:40))
      endif
      call puthisrc(irectp, idesc, irecln, ch80)
      write (ch80, 10010) pfcase, pfdate
10010 format (2x, '6000 BUS POWER FLOW CASE(', a10, ') EXECUTED ON ',
     & a10)
      iswdte = 2
      ifirst = 1
      jfirst = 8
      inew = 0
      write (outbuf, 10040) ch80
      call prtout(1)
C     
C     WRITE COMMENT CARDS ON SOLUTION FILE TO BE USED IN OUTPUT
C     
      irectp = 39
      idesc = 256 + 80
      irecln = 20
      if (debug) then
        call dbgeko2('INPUT1 - writing PF case/date to ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
        call dbgwrc('  Partial card written = ', ch80(1:40))
      endif
      call puthisrc(irectp, idesc, irecln, ch80)

C     Section 8: Initialize data counters
      iline = MAXLS + 1
      ifcd = 0
      iswdte = 3
      igtmax = 0
      imon = 0
      irld = 0
      rcpmva = 1.0/bmva
      ipwr = 1
      iabort = 0
      nchck = 1
      istat = 1
      iesc = 1
      idmp1 = 0
      ldc = kdtot
      ispsw = 0
      ifqsw = 0
      isg = 0
      pi = 3.14159265
      krld = 0
      mxng = 600
      mxng = MAXGEN
      mxnb = MAXBUS
      idcl = 250
      ibr = 1
      nufreq = 0
      ngenf = 0
      ndfltd = 0
      nbypas = 0
      iufreq = 0
      igenf = 0
      idfltd = 0
      ibypas = 0
      ntripl = 0
      itripl = 0
      nrl = 0
      nrlv = 0
      nrld = 0
      k13 = 13
      k23 = 23
      kecdsk = 1
      iecdsk = 1
      jdctot = 0
      mac = 0
      ldz = 0
      ldar = 0
      jdc = 0
      kzero = 0
      ln = 0
      lrd = 0
      lrep = 0
      lrr = 0
      ls = 0
      lsc = 0
      iznkt = 0
      igrp1 = 0
      igrp2 = 0
      igrp3 = 0
      igrp4 = 0
      igrp5 = 0
      igrp6 = 0
      igrp7 = 0
      igrp8 = 0
      igrp14 = 0
      jgrp14 = 0
      jgrp1 = 0
      jgrp2 = 0
      jgrp3 = 0
      jgrp4 = 0
      jgrp5 = 0
      jgrp6 = 0
      jgrp7 = 0
      jgrp8 = 0
      igrp9 = 0
      jgrp9 = 0
      igrp10 = 0
      jgrp10 = 0
      igrp11 = 0
      jgrp11 = 0
      igrp12 = 0
      jgrp12 = 0
      igrp13 = 0
      jgrp13 = 0
      irb = 0
      irc = 0
      kntrv = 0
      nbrake = 0
      mbrk = 45
      istop3 = 0
      jreuse = 0
      sum = 0.0
      iblod = 0
      iamrts = 0
      dmpall = 0.0
c
c     Revised Apr 1994 - Version 5.094M 
c     Set default bus frequency and tzero to 0.00
c
      tbusf = 0.05
      tzero = 0.00
      call whrec1('MAC', ilo, ihi, isz)

C     if (debug) then
C       call dbgeko ('INPUT1 - storing machine counter')                !d
C       call dbgwri ('  Unit 1 counter record #1 = ',ilo)               !d
C       call dbgwri ('  Unit 1 counter record #2 = ',ihi)               !d
C     endif

      write (l1, rec = ilo) mac
      write (l1, rec = ihi) mac
      call whrec1('LDA', ilo, ihi, isz)

C     if (debug) then
C       call dbgeko ('INPUT1 - storing area load rep counter')          !d
C       call dbgwri ('  Unit 1 counter record #1 = ',ilo)               !d
C       call dbgwri ('  Unit 1 counter record #2 = ',ihi)               !d
C     endif

      write (l1, rec = ilo) ldar
      write (l1, rec = ihi) ldar
      call whrec1('LDZ', ilo, ihi, isz)

C     if (debug) then
C       call dbgeko ('INPUT1 - storing zone load rep counter')          !d
C       call dbgwri ('  Unit 1 counter record #1 = ',ilo)               !d
C       call dbgwri ('  Unit 1 counter record #2 = ',ihi)               !d
C     endif

      write (l1, rec = ilo) ldz
      write (l1, rec = ihi) ldz
      call whrec1('LDB', ilo, ihi, isz)

C     if (debug) then
C       call dbgeko ('INPUT1 - storing bus load rep counter')           !d
C       call dbgwri ('  Unit 1 counter record #1 = ',ilo)               !d
C       call dbgwri ('  Unit 1 counter record #2 = ',ihi)               !d
C     endif

      write (l1, rec = ilo) lrep
      write (l1, rec = ihi) lrep
      call whrec1('LN ', ilo, ihi, isz)
      write (l1, rec = ilo) ln
      write (l1, rec = ihi) ln
      call whrec1('LSH', ilo, ihi, isz)

C     if (debug) then
C       call dbgeko ('INPUT1 - storing load shed counter')              !d
C       call dbgwri ('  Unit 1 counter record #1 = ',ilo)               !d
C       call dbgwri ('  Unit 1 counter record #2 = ',ihi)               !d
C     endif

      write (l1, rec = ilo) ls
      write (l1, rec = ihi) ls
      call whrec1('RG ', ilo, ihi, isz)
      write (l1, rec = ilo) lsc
      write (l1, rec = ihi) lsc
      call whrec1('RD ', ilo, ihi, isz)

C     if (debug) then
C       call dbgeko ('INPUT1 - storing local relay counter')            !d
C       call dbgwri ('  Unit 1 counter record #1 = ',ilo)               !d
C       call dbgwri ('  Unit 1 counter record #2 = ',ihi)               !d
C     endif

      write (l1, rec = ilo) lrd
      write (l1, rec = ihi) lrd
      call whrec1('RR ', ilo, ihi, isz)

C     if (debug) then
C       call dbgeko ('INPUT1 - storing remote relay counter')           !d
C       call dbgwri ('  Unit 1 counter record #1 = ',ilo)               !d
C       call dbgwri ('  Unit 1 counter record #2 = ',ihi)               !d
C     endif

      write (l1, rec = ilo) lrr
      write (l1, rec = ihi) lrr
      call whrec1('DC ', ilo, ihi, isz)

C     if (debug) then
C       call dbgeko ('INPUT1 - storing DC convertor counter')           !d
C       call dbgwri ('  Unit 1 counter record #1 = ',ilo)               !d
C       call dbgwri ('  Unit 1 counter record #2 = ',ihi)               !d
C     endif

      write (l1, rec = ilo) jdctot, jdc
      write (l1, rec = ihi) jdctot, jdc
      call forbtm()
      call fortop()
      write (outbuf, 10020)
10020 format ('0INPUT DATA CARDS')
      call prtout(1)
      if (ireuse .ne. 0) then
        write (outbuf, 10030)
10030   format ('+', 25x, '*** reuse feature ***')

C       From initializing, bypass reading input card, since TAPEWK
C       caused first "LS" card to be in BUFFER already.
        call prtout(1)
        call skipln(1)
        skipread = .true.
      endif

C     Section 12:  Start of major loop for reading lines of .SWI file
 
C     Many jumps to this point.  Read next input card.  Starts a
C     loop, but has many loopback places.
      do while (.true.)
        if (.not. skipread) then
          call readin()
10040     format ('0', a)
        endif
 
C       CHECKING DATA CARD FOR TYPE,SUBTYPE,AND CHANGE CODE
 
        read (buffer, 10050) type, subtyp, kchgcd
10050   format (3a1)
        skipread = .false.
        read (buffer, '(A2)') fulltyp

C       Jump if a comment card
        if (fulltyp .ne. '  ' .and. type .ne. 'C') then

c             csw need to remove chgcd trap for CV card

C             Alternate start of SN340 loop; used if last card read
C             starts a new section of data, so card reading bypassed.

              do while (.true.)
                if (inew .eq. 0) then
                  inew = 1
C                 -        1   2   3   4   5   6   7   8   9  10  11  12
C                 13  14
                elseif (ilast .eq. 2) then
                  goto 100
                elseif (ilast .eq. 3) then
                  goto 120
                elseif (ilast .eq. 4) then
                  goto 130
                elseif (ilast .eq. 5) then
                  goto 140
                elseif (ilast .eq. 6) then
                  goto 160
                elseif (ilast .eq. 7) then
                  goto 170
                elseif (ilast .eq. 8) then
                  goto 180
                elseif (ilast .eq. 9) then
                  goto 190
                elseif (ilast .eq. 10) then
                  goto 200
                elseif (ilast .eq. 11) then
                  goto 210
                elseif (ilast .eq. 12) then
                  goto 220
                elseif (ilast .eq. 13) then
                  goto 230
                elseif (ilast .eq. 14) then
                  goto 150
                endif

C               Section 15 - branch out by card type
                if (index('MGTE', type) .ne. 0) goto 450
 
C               CHANGE TYPE = F TO E AND SUBTYPE = A TO M ETC
                if (type .eq. 'F' .and. subtyp .ne. 'F') goto 430
                if (fulltyp .eq. 'FF') then
                  do iwp = 4, 11
                    match = index('ABCDEFGHIJKLMNOPQRSTUVWXYZ', buffer
     &               (iwp:iwp))
                    if (match .ne. 0) goto 430
                  enddo
                endif
                if (index('SVW', type) .ne. 0) goto 450
 
                if (igrp1 .eq. 1) then

C                 Section 23 - group 1 sorting
 
C                 ALPHABETICAL SORTING THE BUS NAMES ASSOCIATED WITH THE
C                 MACHINE DATA CARDS
 
                  jgrp1 = 1
                  igrp1 = 0
                  inew = 0
                  if (mac .ne. 0) then
                    if (mac .le. MAXMAC) then
                      lqiks = 2
                      call qiksrt(1, mac, komp31, swap31)

C                     Section 24 - group 1 data checking
                      if (noprnt .ne. 0) then
                        write (outbuf, 10060)
10060                   format ('0', 24x, '*****************************
     &*********************')
                        call prtout(1)
                        write (outbuf, 10070)
10070                   format (25x, '**********************************
     &****************')
                        call prtout(1)
                        write (outbuf, 10080)
10080                   format ('0', 24x, 'MACHINE DATA CARDS')
                        call prtout(1)
                        call skipln(1)
                      endif
                      kecsav = kecs
                      call wecn(mach1n, 4, kecsav, mac)
                      call whrec1('MAC', msmacl, msmach, msmacsz)
                      macallow = msmacsz*100
                      if (mac .gt. macallow) then
                        write (errbuf(1), '(2a)')
     &                   ' INPUT1 - insufficient temp file ',
     &                   'storage for machine data.'
                        call prterr('E', 1)
                        iabort = 1
                      endif
                      imac = 1

C                     THIS READS INTO A BUNCH OF THINGS- NOT JUST MACHIN
C                     WHICH IS NOT DIMENSIONED 1000 ANYWAY IT IS UNCLEAR
C                     INTENDED AT THIS POINT   ALEX............
 
                      do i = 1, 100
                        do j = 1, 8
                          machin(j, i) = ' '
                        enddo
                      enddo
                      m100 = 100
                      kecsav = kecsav - 1
                      do k = 1, mac
                        call recn(ecsn, 4, kecsav+k, 1)
                        kecs = iecsn(1)
                        call redecc(machin(1, imac), kecs, 8)
                        imac = imac + 1
                        if (imac .gt. 100) then
                          msmacl = msmacl + 1
                          msmach = msmach + 1
                          write (l1, rec = msmacl) machin
                          write (l1, rec = msmach) machin
                          if (noprnt .ne. 0) then
                            do i = 1, m100
                              write (outbuf, 10090) (machin(j, i), j =
     &                         1, 8)
                              call prtout(1)
                            enddo
10090                       format (1x, 8a10)
                            call skipln(2)
                          endif
                          imac = 1
                          do i = 1, 100
                            do j = 1, 8
                              machin(j, i) = ' '
                            enddo
                          enddo
                        endif
                      enddo
                      m100 = mod(mac, 100)
                      if (m100 .ne. 0) then
                        msmacl = msmacl + 1
                        msmach = msmach + 1
                        write (l1, rec = msmacl) machin
                        write (l1, rec = msmach) machin
                        if (noprnt .ne. 0) then
                          do i = 1, m100
                            write (outbuf, 10090) (machin(j, i), j = 1,
     &                       8)
                            call prtout(1)
                          enddo
                          call skipln(2)
                        endif
                      endif
                      call whrec1('MAC', msmacl, msmach, msmacsz)
                      write (l1, rec = msmacl) mac
                      write (l1, rec = msmach) mac
                    else
                      write (errbuf(1), 10100) mac, MAXMAC
10100                 format ('Number of machine cards submittted ',
     &                 i4, ' exceeds limit of ', i4)
                      call prterr('E', 1)
                      istop3 = 1
                    endif
                  endif
                  goto 240
                endif
 
C               A2   ADDING TEMPORARY LOAD & TX SATURATION
 
  100           if (type .eq. 'L' .and. 
     &              index('ABCD', subtyp) .ne. 0) goto 250
                if (igrp2 .eq. 2) then

C                 Section 26 - group 2A sorting
C                 
C                 ALPHABETICAL LISTING OF LOAD REPRESENTATION CARDS
C                 
                  jgrp2 = 2
                  igrp2 = 0
                  inew = 0
                  lrepec = kecs
                  if (ldar .ne. 0) then
                    if (ldar .gt. 60) then
                      write (errbuf(1), 10110) ldar, 60
10110                 format ('Number of load representation cards by ar
     &ea submittted ', i4, ' exceeds limit of ', i4)
                      call prterr('E', 1)
                      istop3 = 1
                    elseif (ntotc .eq. 0) then
                      write (errbuf(1), 10120)
10120                 format (' No Area Interchange data in BASE case. L
     &oad Representation by area can not be assigned.')
                      call prterr('E', 1)
                      istop3 = 1
                    elseif (jtie .eq. 0 .and. ntotc .gt. 0) then
                      write (errbuf(1), 10130)
10130                 format (' Incomplete Area Interchange data in BASE
     & case. Load Representation by area can not be assigned.')
                      call prterr('E', 1)
                      istop3 = 1
                    else
                      lqiks = 11
                      call qiksrt(1, ldar, komp31, swap31)

C                     Section 27 - group 2A data checking
                      call whrec1('LDA', mslrpl, mslrph, mslrpsz)
                      if (noprnt .ne. 0) then
                        write (outbuf, 10060)
                        call prtout(1)
                        write (outbuf, 10140)
                        call prtout(1)
10140                   format ('0', 24x,
     &                   'LOAD REPRESENTATION CARDS BY AREA INTERCHANGE'
     &                   )
                      endif
                      do i = 1, ldar
                        kecs = irea1n(i)
                        call redecc(lrepdt(1, i), kecs, 8)
                      enddo
                      if (noprnt .ne. 0) then
                        do i = 1, ldar
                          write (outbuf, 10150) (lrepdt(j,i),j=1,8)
                          call prtout(1)
                        enddo
10150                   format (1x, 8a10)
                      endif
                      write (l1, rec = mslrpl) ldar
                      write (l1, rec = mslrph) ldar
                      mslrpl = mslrpl + 1
                      mslrph = mslrph + 1
                      write (l1, rec = mslrpl) lrepdt
                      write (l1, rec = mslrph) lrepdt
                    endif
                  endif

C                 Section 28 - group 2B sorting
                  if (ldz .ne. 0) then
                    if (ldz .gt. 150) then
                      write (errbuf(1), 10160) ldz, 150
10160                 format ('Number of Load Representation cards by zo
     &ne submittted ', i4, ' exceeds limit of ', i4)
                      call prterr('E', 1)
                      istop3 = 1
                    else
                      call ritecs(lrep1, lrepec, ntotd)
                      lqiks = 10
                      call qiksrt(1, ldz, komp31, swap31)

C                     Section 29 - group 2B data checking
                      call whrec1('LDZ', mslrpl, mslrph, mslrpsz)
                      if (noprnt .ne. 0) then
                        write (outbuf, 10060)
                        call prtout(1)
                        write (outbuf, 10170)
                        call prtout(1)
10170                   format ('0', 24x,
     &                   'LOAD REPRESENTATION BY ZONES')
                      endif
                      write (l1, rec = mslrpl) ldz
                      write (l1, rec = mslrph) ldz
                      ii = 0
                      do i = 1, ldz
                        ii = ii + 1
                        kecs = idznen(i)
                        call redecc(lrepdt(1, ii), kecs, 8)
                        if (ii .ge. 100) then
                          mslrpl = mslrpl + 1
                          mslrph = mslrph + 1
                          write (l1, rec = mslrpl) lrepdt
                          write (l1, rec = mslrph) lrepdt
                          if (noprnt .ne. 0) then
                            do k = 1, ldz
                              write (outbuf, 10150) (lrepdt(j,k),j=1,8)
                              call prtout(1)
                            enddo
                          endif
                          ii = 0
                        endif
                      enddo
                      if (ldz .ne. 100) then
                        mslrpl = mslrpl + 1
                        mslrph = mslrph + 1
                        write (l1, rec = mslrpl) lrepdt
                        write (l1, rec = mslrph) lrepdt
                        kk = mod(ldz, 100)
                        if (noprnt .ne. 0) then
                          do k = 1, kk
                            write (outbuf, 10150) (lrepdt(j,k),j=1,8)
                            call prtout(1)
                          enddo
                        endif
                      endif
                    endif
                  endif
                  if (lrep .ne. 0) then
                    if (lrep .gt. mxlrep) then
                      write (errbuf(1), 10180) lrep, mxlrep
10180                 format ('Number of load representation cards by bus
     & submittted ', i4, ' exceeds limit of ', i4)
                      call prterr('E', 1)
                      istop3 = 1
                    else

C                     Section 30 - group 2C sorting
                      if (ldz .gt. 0) call redecs(lrep1, lrepec, ntotd)
                      m100 = 100
                      ilrep = 1
                      call whrec1('LDB', mslrpl, mslrph, mslrpsz)
                      lrallow = mslrpsz*100
                      if (lrep .gt. lrallow) then
                        write (errbuf(1), '(2a)')
     &                   ' INPUT1 - insufficient temp file ',
     &                   'storage for load rep data.'
                        call prterr('E', 1)
                        iabort = 1
                      endif
                      lqiks = 3
                      call qiksrt(1, lrep, komp31, swap31)

C                     Section 31 - group 2C data checking
                      if (noprnt .ne. 0) then
                        write (outbuf, 10060)
                        call prtout(1)
                        write (outbuf, 10190)
                        call prtout(1)
10190                   format ('0', 24x,
     &                   'LOAD REPRESENTATION BY BUS - BASE')
                      endif
                      do i = 1, lrep
                        kecs = lrep1(i)
                        call redecc(lrepdt(1, ilrep), kecs, 8)
                        ilrep = ilrep + 1
                        if (ilrep .gt. 100) then
                          mslrpl = mslrpl + 1
                          mslrph = mslrph + 1
                          write (l1, rec = mslrpl) lrepdt
                          write (l1, rec = mslrph) lrepdt
                          if (noprnt .ne. 0) then
                            do k = 1, m100
                              write (outbuf, 10090) (lrepdt(j, k), j =
     &                         1, 8)
                              call prtout(1)
                            enddo
                            write (outbuf, 10060)
                            call prtout(1)
                          endif
                          ilrep = 1
                        endif
                      enddo
                      m100 = mod(lrep, 100)
                      if (m100 .ne. 0) then
                        mslrpl = mslrpl + 1
                        mslrph = mslrph + 1
                        write (l1, rec = mslrpl) lrepdt
                        write (l1, rec = mslrph) lrepdt
                        if (noprnt .ne. 0) then
                          do k = 1, m100
                            write (outbuf, 10090) (lrepdt(j,k),j=1,8)
                            call prtout(1)
                          enddo
                        endif
                      endif
                      call whrec1('LDB', mslrpl, mslrph, mslrpsz)
                      write (l1, rec = mslrpl) lrep
                      write (l1, rec = mslrph) lrep
                    endif
                  endif
                  goto 240
                endif
  120           if (fulltyp .eq. 'LN') goto 260
                if (igrp3 .eq. 3) then

C                 Section 33 - group 3 sorting
C                 ALPHABETICALLY SORTING THE BUS NAMES IN THE LNET TAB
C                 
                  jgrp3 = 3
                  igrp3 = 0
                  inew = 0
                  if (ln .ne. 0) then
                    if (ln .gt. MAXGEN) then
                      write (errbuf(1), 10200) ln, MAXGEN
10200               format ('Number of load netting cards submittted ',
     &                 i4, ' exceeds limit of ', i4)
                      call prterr('E', 1)
                      istop3 = 1
                    else
                      lqiks = 4
                      call qiksrt(1, ln, komp31, swap31)

C                     Section 34 - group 3 data checking
                      if (noprnt .ne. 0) then
                        write (outbuf, 10060)
                        call prtout(1)
                        write (outbuf, 10210)
                        call prtout(1)
                        call skipln(1)
10210                   format ('0', 24x, ' LOAD NETTING BUSSES')
                        do i = 1, ln
                          name1c = lnetc(i)
                          namen = lnetn(i)
                          kb = lnetn(i)
                          base = basekv(kb)
                          write (outbuf, 10220) lnetcc(i), name1c, base
                          call prtout(1)
10220                     format (1x, 'LN', a1, a8, 2x, f6.1)
                        enddo
                      endif
                    endif
                  endif
                  goto 240
                endif

  130           if (type .eq. 'U' .and. 
     &              index('VFD', subtyp) .ne. 0) goto 270
                if (igrp4 .eq. 4) then

C                 Section 36 - group 4 sorting
C                 
C                 ALPHABETICALLY SORTING THE BUS NAMES ASSOCIATED WITH
C                 SHEDDING CARDS AND REARRANGING THE ENTRIES IN THE LS
C                 
                  jgrp4 = 4
                  igrp4 = 0
                  inew = 0
                  if (ls .ne. 0) then
                    if (ls .le. MXLSHD) then
                      lqiks = 5
                      call qiksrt(1, ls, komp31, swap31)
C                     -  Section 37 - group 4 data checking
                      if (noprnt .ne. 0) then
                        write (outbuf, 10060)
                        call prtout(1)
                        write (outbuf, 10230)
                        call prtout(1)
                        call skipln(1)
10230                   format ('0', 24x, 'LOAD SHEDDING DATA CARDS')
                      endif
                      call whrec1('LSH', mslsl, mslsh, mslsz)
                      lsallow = mslsz*100
                      if (ls .gt. lsallow) then
                        write (errbuf(1), '(2a)')
     &                   ' INPUT1 - insufficient temp file ',
     &                   'storage for load shed data.'
                        call prterr('E', 1)
                      endif
                      ils = 0
                      m100 = 100
                      do i = 1, ls
                        kecs = lshed2(i)
                        ils = ils + 1
                        call redecc(lshed(1, ils), kecs, 8)
                        if (ils .eq. 100) then
                          mslsl = mslsl + 1
                          mslsh = mslsh + 1
                          write (l1, rec = mslsl) lshed
                          write (l1, rec = mslsh) lshed
C                         MSLS=MSLS+1
C                         write (l1,REC=MSLS+100) LSHED
C                         write (l1,REC=MSLS) LSHED
                          if (noprnt .ne. 0) then
                            do k = 1, m100
                              write (outbuf, 10240) (lshed(j, k), j =
     &                         1, 8)
                              call prtout(1)
                            enddo
10240                       format (1x, 8a10)
                          endif
                          ils = 0
                        endif
                      enddo
                      m100 = mod(ls, 100)
                      if (m100 .ne. 0) then
                        mslsl = mslsl + 1
                        mslsh = mslsh + 1
                        write (l1, rec = mslsl) lshed
                        write (l1, rec = mslsh) lshed
C                       MSLS=MSLS+1
C                       write (l1,REC=MSLS+100) LSHED
C                       write (l1,REC=MSLS) LSHED
                        if (noprnt .ne. 0) then
                          do i = 1, m100
                            write (outbuf, 10240) (lshed(j, i), j = 1,
     &                       8)
                            call prtout(1)
                          enddo
                        endif
                      endif
                      call whrec1('LSH', mslsl, mslsh, mslsz)
                      write (l1, rec = mslsl) ls
                      write (l1, rec = mslsh) ls
C                     2620   write (l1,REC=70+100) LS
C                     write (l1,REC=70) LS
                    else
C                     IF (LS.LE.750) GO TO 2500
                      write (errbuf(1), 10250) ls, MXLSHD
10250                 format (' You have submitted ', i4,
     &                 ' load shedding cards.', '  The limit is ', i4,
     &                 '.')
                      call prterr('E', 1)
                      istop3 = 1
                    endif
                  endif
                  goto 240
                endif
  140           if (fulltyp .eq. 'RG') goto 290
                if (igrp5 .eq. 5) then
C                 -  Section 39 - group 5 sorting
C                 
C                 ALPHABETICALLY SORTING THE BUS NAMES ASSOCIATED WITH
C                 CAPACITOR CARDS AND REARRANGING THE ENTRIES IN THE L
C                 
                  jgrp5 = 5
                  igrp5 = 0
                  inew = 0
                  if (lsc .ne. 0) then
                    if (lsc .le. 100) then
                      lqiks = 6
                      call qiksrt(1, lsc, komp31, swap31)
C                     -  Section 40 - group 5 data checking
                      if (noprnt .ne. 0) then
                        write (outbuf, 10060)
                        call prtout(1)
                        write (outbuf, 10260)
                        call prtout(1)
                        call skipln(1)
10260                   format ('0', 24x, 'SERIES CAPACITOR DATA CARDS'
     &                   )
                      endif
                      m100 = lsc
                      do i = 1, lsc
                        kecs = lsrc3n(2, i)
                        call redecc(lsrcp(1, i), kecs, 8)
                      enddo
                      call whrec1('RG ', msscl, mssch, msscsz)
                      write (l1, rec = msscl) lsc
                      write (l1, rec = mssch) lsc
                      msscl = msscl + 1
                      mssch = mssch + 1
                      write (l1, rec = msscl) lsrcp
                      write (l1, rec = mssch) lsrcp
                      if (noprnt .ne. 0) then
                        do i = 1, m100
                          write (outbuf, 10270) (lsrcp(j, i), j = 1, 8)
                          call prtout(1)
                        enddo
10270                   format (1x, 8a10)
                      endif
                    else
                      write (errbuf(1), 10280) lsc, 100
10280                 format ('Number of Series Capacitor cards submittt
     &ed ', i4, ' exceeds limit of ', i4)
                      call prterr('E', 1)
                      istop3 = 1
                    endif
                  endif
                  goto 240
                endif
  150           if (fulltyp .eq. 'RZ') goto 300
                if (igrp14 .eq. 14) then

C                 Section 64 - group 14 sorting
C                 
C                 ALPHABETICALLY SORTING THE BUS NAMES ASSOCIATED
C                 CAPACITOR CARDS AND REARRANGING THE ENTRIES IN T
C                 
                  jgrp14 = 14
                  igrp14 = 0
                  inew = 0
                  if (iznkt .ne. 0) then
                    if (iznkt .le. 25) then
                      lqiks = 21
                      call qiksrt(1, iznkt, komp31, swap31)

C                     Section 65 - group 14 data checking
                      if (noprnt .ne. 0) then
                        write (outbuf, 10060)
                        call prtout(1)
                        write (outbuf, 10290)
                        call prtout(1)
                        call skipln(1)
10290                   format ('0', 24x, 'ZNO CAPACITOR DATA CARDS')
                      endif
                      m100 = iznkt
                      do i = 1, iznkt
                        kecs = izn3n(2, i)
                        call redecc(izncp(1, i), kecs, 8)
                      enddo
                      if (noprnt .ne. 0) then
                        do i = 1, m100
                          write (outbuf, 10300) (izncp(j, i), j = 1, 8)
                          call prtout(1)
                        enddo
10300                   format (1x, 8a10)
                      endif
                    else
                      write (errbuf(1), 10310) iznkt, 25
10310                 format ('Number of ZNO Capacitor cards submittted 
     &', i4, ' exceeds limit of ', i4)
                      call prterr('E', 1)
                      istop3 = 1
                    endif
                  endif
                  goto 240
                endif
  160           if (type .eq. 'R' .and. 
     &              index('DO123U', subtyp) .ne. 0) goto 310
                if (igrp6 .eq. 6) then

C                 Section 42 - group 6 sorting
C                 
C                 ALPHABETICALLY SORTING THE BUS NAMES ASSOCIATED WITH
C                 CARD AND REARRANGING THE ENTRIES IN THE LREDTA TABLE
C                 
                  jgrp6 = 6
                  igrp6 = 0
                  inew = 0
                  if (lrd .ne. 0) then
                    if (lrd .le. 200) then
                      lqiks = 7
                      call qiksrt(1, lrd, komp31, swap31)

C                     Section 43 - group 6 data checking
                      if (noprnt .ne. 0) then
                        write (outbuf, 10060)
                        call prtout(1)
                        write (outbuf, 10320)
                        call prtout(1)
                        call skipln(1)
10320                   format ('0', 24x, 'LOCAL RELAY DATA CARDS')
                      endif
                      call whrec1('RD ', msrdl, msrdh, msrdsz)
                      lrdallow = msrdsz*100
                      if (lrd .gt. lrdallow) then
                        write (errbuf(1), '(2a)')
     &                   ' INPUT1 - insufficient temp file ',
     &                   'storage for local relay data.'
                        call prterr('E', 1)
                        iabort = 1
                      endif
                      ilrd = 0
                      m100 = 100
                      do i = 1, lrd
                        kecs = lrly3n(2, i)
                        ilrd = ilrd + 1
                        call redecc(lrelay(1, ilrd), kecs, 8)
                        if (ilrd .eq. 100) then
                          msrdl = msrdl + 1
                          msrdh = msrdh + 1
                          write (l1, rec = msrdl) ((lrelay(itrr, j),
     &                     itrr = 1, 8), j = 1, 100)
                          write (l1, rec = msrdh) ((lrelay(itrr, j),
     &                     itrr = 1, 8), j = 1, 100)
                          if (noprnt .eq. 0) then
                          endif
                          ilrd = 0
                        endif
                      enddo
                      m100 = mod(lrd, 100)
                      if (m100 .ne. 0) then
                        msrdl = msrdl + 1
                        msrdh = msrdh + 1
                        write (l1, rec = msrdl) ((lrelay(i,j),i=1,8),
     &                    j=1,m100)
                        write (l1, rec = msrdh) ((lrelay(i,j),i=1,8),
     &                    j=1,m100)
                        if (noprnt .ne. 0) then
                          do i = 1, m100
                            write (outbuf, 10330) (lrelay(j,i),j=1,8)
10330                       format (1x, 8a10)
                            call prtout(1)
                          enddo
                        endif
                      endif
                      call whrec1('RD ', msrdl, msrdh, msrdsz)
                      write (l1, rec = msrdl) lrd
                      write (l1, rec = msrdh) lrd
                    else
                      write (errbuf(1), 10340) lrd, 200
10340                 format ('Number of Local Relay cards submittted ',
     & i4, ' exceeds limit of ', i4)
                      call prterr('E', 1)
                      istop3 = 1
                    endif
                  endif
                  goto 240
                endif
  170           if (fulltyp .eq. 'RR') goto 320
                if (igrp7 .eq. 7) then

C                 Section 45 - group 7 sorting
C                 
C                 ALPHABETICALLY SORTING THE BUS NAMES ASSOCIATED WITH
C                 RELAY AND REARRANGING THE ENTRIES IN THE LRROD TABLE
C                 
                  jgrp7 = 7
                  igrp7 = 0
                  inew = 0
                  if (lrr .ne. 0) then
                    if (lrr .le. 100) then
                      lqiks = 8
                      call qiksrt(1, lrr, komp31, swap31)

C                     Section 46 - group 7 data checking
                      if (noprnt .ne. 0) then
                        write (outbuf, 10060)
                        call prtout(1)
                        write (outbuf, 10350)
                        call prtout(1)
                        call skipln(1)
10350                   format ('0', 24x, 'REMOTE RELAY DATA CARDS')
                      endif
                      do i = 1, lrr
                        kecs = lrod3n(2, i)
                        call redecc(lrrod(1, i), kecs, 8)
                      enddo
                      call whrec1('RR ', msrrl, msrrh, msrrsz)
                      write (l1, rec = msrrl) lrr
                      write (l1, rec = msrrh) lrr
                      msrrl = msrrl + 1
                      msrrh = msrrh + 1
                      write (l1, rec = msrrl) lrrod
                      write (l1, rec = msrrh) lrrod

                      m100 = lrr
                      if (noprnt .ne. 0) then
                        do i = 1, m100
                          write (outbuf, 10360) (lrrod(j,i),j=1,8)
10360                     format (1x, 8a10)
                          call prtout(1)
                        enddo
                      endif
                    else
                      write (errbuf(1), 10370) lrr, 100
10370                 format ('Number of Remote Relay cards submittted '
     &, i4, ' exceeds limit of ', i4)
                      call prterr('E', 1)
                      istop3 = 1
                    endif
                  endif
                  goto 240
                endif

C               Jump if latest card is an "LS"
  180           if (fulltyp .eq. 'LS') goto 350

C               Jump if one card past last "LS" card
                if (igrp8 .eq. 8) then

C                 Here if we all "LS" cards read in.
                  ch80 = buffer
                  jgrp8 = 8
                  igrp8 = 0
                  inew = 0

C                 Section 51 - group 8 sorting
C                 
C                 CALL LSREAD TO SORT AND READ LS CARDS
C                 
                  call lsread()

C                 Section 52 - group 8 data checking
                  buffer = ch80

C                 Process next card (bypass new card reading)
                  goto 240
                endif
  190           if (type .eq. 'D' .and. 
     &              index(' SACVTP', subtyp) .ne. 0) goto 360
                if (type .eq. 'D' .and. 
     &              index('BC', subtyp) .ne. 0) goto 370
                if (igrp9 .eq. 9) then
 
C                 Section 48 - group 9 sorting
C                 
C                 ALPHABETICALLY SORTING OF DC CARDS AND MAKING A LIST
C                 
                  jgrp9 = 9
                  inew = 0
                  igrp9 = 0
                  if (jdc .ne. 0) then
                    if (jdc .le. 150) then
                      lqiks = 12
                      call qiksrt(1, jdc, komp31, swap31)

C                     Section 49 - group 9 data checking
                      do i = 1, jdc
                        kecs = idcinn(2, i)
                        call redecc(dccard(1, i), kecs, 8)
                      enddo
                      call whrec1('DC ', msdcl, msdch, msdcsz)
                      write (l1, rec = msdcl) jdctot, jdc
                      write (l1, rec = msdch) jdctot, jdc
                      msdcl = msdcl + 1
                      msdch = msdch + 1
                      write (l1, rec = msdcl) ((dccard(j, k), j = 1, 8),
     &                 k = 1, jdc)
                      write (l1, rec = msdch) ((dccard(j, k), j = 1, 8),
     &                 k = 1, jdc)
                      kwords = 8*jdc
                      if (noprnt .ne. 0) then
                        write (outbuf, 10060)
                        call prtout(1)
                        write (outbuf, 10380)
                        call prtout(1)
                        call skipln(1)
10380                   format ('0', 24x, 'DC DATA CARDS')
                        do k = 1, jdc
                          write (outbuf, 10390) (dccard(j, k), j = 1,
     &                     8)
                          call prtout(1)
                        enddo
10390                   format (1x, 8a10)
                      endif
                    else
                      write (errbuf(1), 10400) jdc, 150
10400                 format ('Number of D-C cards submittted ', i4, 
     &                        ' exceeds limit of ', i4)
                      call prterr('E', 1)
                      istop3 = 1
                    endif
                  endif
                  goto 240
                endif
  200           if (type .eq. 'R' .and. 
     &              index('BC', subtyp) .ne. 0) goto 380
                if (igrp10 .eq. 10) then

C                 Section 54 - group 10 sorting & data checking
C                 
C                 SORTING RB AND RC CARDS
C                 
                  jgrp10 = 10
                  igrp10 = 0
                  inew = 0
                  kwords = mbrk*irb
                  call redecs(msort, TEMP_ECSADDR, kwords)
                  call whrec1('MSR', msrtl, msrth, msrtsz)
                  write (l1, rec = msrtl) (msort(nnn), nnn = 1, kwords)
                  if (irb .gt. 1) then
                    lqiks = 16
                    call qiksrt(1, irb, komp31, swap31)
                    lqiks = 17
                    call qiksrt(1, irb, komp31, swap31)
                  endif
                  goto 240
                endif
  210           if (fulltyp .eq. 'RS') goto 390
  220           if (fulltyp .eq. 'RM') goto 400
                if (igrp11 .eq. 11) then

C                 Section 56 - group 11 sorting
C                 
C                 ALPHABETICALLY SORTING THE BUS NAMES ASSOCIATED WITH
C                 LOAD SHEDDING RELAY CF-1
C                 
                  jgrp11 = 11
                  igrp11 = 0
                  inew = 0
                  if (nufreq .ne. 0) then
                    if (nufreq .le. 300) then
                      lqiks = 18
                      call qiksrt(1, nufreq, komp31, swap31)

C                     Section 57 - group 11 data checking
                      if (noprnt .ne. 0) then
                        write (outbuf, 10060)
                        call prtout(1)
                        write (outbuf, 10410)
                        call prtout(1)
                        call skipln(1)
10410                   format ('0', 24x,
     &                   'UNDERFREQUENCY LOAD SHEDDING RELAYS CF-1')
                      endif
                      kount = 0
                      do i = 1, nufreq
                        kount = kount + 1
                        kecs = kfreq(i)
                        call redecc(cf1tab(1, kount), kecs, 8)
                      enddo
                      if (noprnt .ne. 0) then
                        do k = 1, kount
                          write (outbuf, 10090) (cf1tab(j, k), j = 1,
     &                     8)
                          call prtout(1)
                        enddo
                      endif
                    else
                      write (errbuf(1), 10420) nufreq, 300
10420                 format ('Number of Underfrequence Load Shedding Re
     &lay CF-1 cards submittted ', i4, ' exceeds limit of ', i4)
                      call prterr('E', 1)
                      istop3 = 1
                    endif
                  endif
                  goto 240
                elseif (igrp12 .eq. 12) then

C                 Section 59 - group 12 sorting
C                 
C                 ALPHABETICALLY SORTING THE BUS NAMES ASSOCIATED WITH
C                 UNDERFREQUENCY RELAYS
C                 
                  jgrp12 = 12
                  igrp12 = 0
                  inew = 0
                  if (ngenf .ne. 0) then
                    lqiks = 19
                    call qiksrt(1, ngenf, komp31, swap31)

C                   Section 60 - group 12 data checking
                    if (noprnt .ne. 0) then
                      write (outbuf, 10060)
                      call prtout(1)
                      write (outbuf, 10430)
                      call prtout(1)
                      call skipln(1)
10430                 format ('0', 24x,
     &                 'GENERATOR UNDERFREQUENCY RELAYS')
                    endif
                    kount = 0
                    do i = 1, ngenf
                      kount = kount + 1
                      kecs = kgenf(i)
                      call redecc(rmtab(1, kount), kecs, 8)
                    enddo
                    if (noprnt .ne. 0) then
                      do k = 1, kount
                        write (outbuf, 10090) (rmtab(j, k), j = 1, 8)
                        call prtout(1)
                      enddo
                    endif
                  endif
                  goto 240
                endif
  230           if (fulltyp .eq. 'RL') goto 410
                if (igrp13 .ne. 13) goto 420

C               Section 62 - group 13 sorting and data checking
                jgrp13 = 13
                igrp13 = 0
                inew = 0
C               
C               CALL RLINP TO FORM ROVX TABLE FOR DEFAULT DISTANCE
C               
                call rlinp()
  240           continue
              enddo

C             Section 25 - group 2 array build-up
 
C             READING LOAD REPRESENTATION DATA CARDS AND BUILDING
C             IDENTIFICATION TABLES
 
C             A2   CHANGE SUBTYP C & D TO A & C OR D IN COLUMN 80
 
  250         if (subtyp .eq. 'C') then
                buffer(2:2) = 'A'
                buffer(80:80) = 'C'
              elseif (subtyp .eq. 'D') then
                buffer(2:2) = 'A'
                buffer(80:80) = 'D'
              endif
              if (igrp2 .ne. 2) then
                if (jgrp2 .eq. 2) goto 480
                igrp2 = 2
                ilast = 2
                kecs = TEMP_ECSADDR
              endif
              read (buffer, 10440) name1c, base, zone, aname
10440         format (bz, 3x, a8, f4.0, a2, a10)
              ix = 0
              iy = 0
              iz = 0
              if (aname .ne. ' ') ix = 1
              if (zone .ne. '  ') iy = 1
              if (name1c .ne. '        ') iz = 1
              if ((ix+iy+iz) .eq. 1) then
                if (aname .ne. ' ') then
                  ldar = ldar + 1
                  areac2(ldar) = aname
                  arac1c(ldar) = kchgcd
                  irea1n(ldar) = kecs
                elseif (zone .eq. '  ') then
                  namen = nambas(base)
                  if (namen .eq. 0) then
                     errflag = 1
                     iabort = 1
                     goto 510
                  endif
                  errflag = 0
                  lrep = lrep + 1
                  lrep1(lrep) = kecs
                  lrep2c(lrep) = name1c
                  lrep2n(lrep) = namen
                else
                  ldz = ldz + 1
                  dzonec(1, ldz) = zone
                  dzonec(2, ldz) = kchgcd
                  idznen(ldz) = kecs
                endif
                call ritecc(buf10, kecs, 8)
                kecs = kecs + 8
C               -  Jump to read next input card
              else
                write (errbuf(1), 10450) buffer
10450           format ('0', a)
                write (errbuf(2), 10460)
                call prterr('E', 2)
10460           format ('The previous record contains an ambiguous BUS/Z
     &ONE/AREA entity')
                iabort = 1
              endif
              goto 510

C             Section 32 - group 3 array build-up
C             READING THE LOAD NETTING CARDS AND BUILDING THE LNET TAB
C             
  260         if (igrp3 .ne. 3) then
                if (jgrp3 .eq. 3) goto 480
                igrp3 = 3
                ilast = 3
                kecs = TEMP_ECSADDR
              endif
              read (buffer, 10470) (tempc(k), tempn(k), k = 1, 5)
10470         format (bz, 3x, 5(a8, f4.0, 3x))
              do k = 1, 5
                if (tempc(k) .ne. ' ') then

C                 Jump to read next input card
                  namen = nambas(tempn(k))
                  if (namen .eq. 0) then
                     errflag = 1
                     iabort = 1
                     goto 10472
                  endif
                  name1c = tempc(k)
                  errflag = 0
                  ln = ln + 1
                  lnetc(ln) = name1c
                  lnetn(ln) = namen
                  lnetcc(ln) = kchgcd
                endif
10472           continue
              enddo
              goto 510

C             Section 35 - group 4 array build-up
C             READING LOAD SHEDDING CARDS
 
  270         if (igrp4 .ne. 4) then
                if (jgrp4 .eq. 4) goto 480
                igrp4 = 4
                ilast = 4
                kecs = TEMP_ECSADDR
              endif
C             
C             CHECK FOR AREA REPRESENTATION
C             
              read (buffer, 10480) name1, zone
10480         format (bz, 3x, a8, 2x, a2)
              if (zone .eq. '  ') then
                namen = ls + 1
              else
C               
C               CHECK FOR ZONE REPRESENTATION
C               
                read (buffer, 10490) c10
10490           format (3x, a10)
                if (c10 .eq. ' ') then
                  namen = ls + 1
                  read (buffer, 10500) zone
10500             format (13x, a2)
                  name1c = zone
                else
C                 
C                 TOTAL REPRESENTATION
C                 
                  read (buffer, 10510) c7
10510             format (9x, a7)
                  if (c7 .eq. '      ') then
                    read (buffer, 10520) name1c
10520               format (3x, a8)
                    if (name1c .eq. 'TOTAL   ') then
                      namen = ls + 1
                      goto 280
                    endif
                  endif
C                 
C                 BUS REPRESENTATION
C                 
                  read (buffer, 10530) name1c, base
10530             format (bz, 3x, a8, f4.0)
                  namen = nambas(base)
                  if (namen .eq. 0) then
                     errflag = 1
                     iabort = 1
                     goto 510
                  endif
                endif
              endif
  280         errflag = 0
              ls = ls + 1
              lshd1c(ls) = name1c
              lshd1n(ls) = namen
              lshd2c(ls) = subtyp
              lshed2(ls) = kecs
              goto 490

C             Section 38 - group 5 array build-up
C             READING SERIES CAPACITOR GAP RELAYING DATA CARDS
C             
  290         if (igrp5 .ne. 5) then
                if (jgrp5 .eq. 5) goto 480
                igrp5 = 5
                ilast = 5
                kecs = TEMP_ECSADDR
              endif
              read (buffer, 10540) name1k, base1, name2k, base2, id,
     &         ksect, bore
10540         format (bz, 6x, a8, f4.0, 1x, a8, f4.0, a1, i1, 30x, a1)
              name1n = nambas(base1)
              name2n = nambas(base2)
              if (name1n .eq. 0 .or. name2n .eq. 0) then
                 errflag = 1
                 iabort = 1
                 goto 510
              endif
              if (id .eq. '0') id = ' '
              if (ksect .le. 0) then
                write (errbuf(1), 10550) buffer
10550           format ('0', a)
                write (errbuf(2), 10560)
10560           format ('The previous Series Capacitor Card must have a n
     &on-zero section number')
                call prterr('E', 2)
                iabort = 1
                goto 510
              else
                lsc = lsc + 1
C               
C               WRITE RG CARD IMAGE TO SOLUTION HISTORY FILE
C               
                irectp = 47
                idesc = 256 + 80
                irecln = 20
                if (debug) then
                  call dbgeko2('INPUT1 - writing RG relay card to ',
     &             'history file.')
                  call dbgwri('  IRECTP /record type/ = ', irectp)
                  call dbgwri('  IDESC /rec descrip/  = ', idesc)
                  call dbgwri('  IRECLN /rec length/  = ', irecln)
                  call dbgwrc('  Partial card written = ', buffer(1:40)
     &             )
                endif
                call puthisrc(irectp, idesc, irecln, buffer)
                lsrc1c(lsc) = name1k
                lsrc1n(lsc) = name1n
                lsrc2c(lsc) = name2k
                lsrc2n(lsc) = name2n
                lsrc3c(1, lsc) = id
                lsrc3c(2, lsc) = kchgcd
                lsrc3n(1, lsc) = ksect
                lsrc3n(2, lsc) = kecs
                goto 490
              endif

C             Section 63 - group 14 array build-up
C             READING ZNO SERIES CAPACITOR DATA CARDS
C             
  300         if (igrp14 .ne. 14) then
                if (jgrp14 .eq. 14) goto 480
                igrp14 = 14
                ilast = 14
                kecs = TEMP_ECSADDR
              endif
              read (buffer, 10570) sstyp, name1k, base1, name2k, base2,
     &         id, ksect
10570         format (bz, 3x, 1a, 2x, a8, f4.0, 1x, a8, f4.0, a1, i1,
     &         30x, a1)
              name1n = nambas(base1)
              name2n = nambas(base2)
              if (name1n .eq. 0 .or. name2n .eq. 0) then
                 errflag = 1
                 iabort = 1
                 goto 510
              endif
              if (id .eq. '0') id = ' '
              if (ksect .le. 0) then
                write (errbuf(1), 10580) buffer
10580           format ('0', a)
                write (errbuf(2), 10590)
10590           format ('The previous ZNO Capacitor Card must have a non
     &-zero section number')
                call prterr('E', 2)
                iabort = 1
                goto 510
              else
                iznkt = iznkt + 1
                izn1c(iznkt) = name1k
                izn1n(iznkt) = name1n
                izn2c(iznkt) = name2k
                izn2n(iznkt) = name2n
                izn3c(1, iznkt) = id
                izn3c(2, iznkt) = sstyp
                izn3n(1, iznkt) = ksect
                izn3n(2, iznkt) = kecs
C               
C               WRITE RZ CARD IMAGE TO SOLUTION HISTORY FILE
C               
                irectp = 47
                idesc = 256 + 80
                irecln = 20
                if (debug) then
                  call dbgeko2('INPUT1 - writing RZ relay card ',
     &             'history file.')
                  call dbgwri('  IRECTP /record type/ = ', irectp)
                  call dbgwri('  IDESC /rec descrip/  = ', idesc)
                  call dbgwri('  IRECLN /rec length/  = ', irecln)
                  call dbgwrc('  Partial card written = ', buffer(1:40)
     &             )
                endif
                call puthisrc(irectp, idesc, irecln, buffer)
                goto 490
              endif

C             Section 41 - group 6 array build-up
C             READING RELAY DATA CARDS AND BUILDING AN IDENTIFICATION
C             LREDTA
C             
  310         if (igrp6 .ne. 6) then
                if (jgrp6 .eq. 6) goto 480
                kecs = TEMP_ECSADDR
                igrp6 = 6
                ilast = 6
              endif
              read (buffer, 10600) name1k, base1, name2k, base2, id,
     &         ksect
10600         format (bz, 6x, a8, f4.0, 1x, a8, f4.0, a1, i1)
              name1n = nambas(base1)
              name2n = nambas(base2)
              if (name1n .eq. 0 .or. name2n .eq. 0) then
                 errflag = 1
                 iabort = 1
                 goto 510
              endif
              lrd = lrd + 1
C             
C             WRITE RELAY CARD IMAGE TO SOLUTION HISTORY FILE
C             
              irectp = 47
              idesc = 256 + 80
              irecln = 20
              if (debug) then
                call dbgeko2(
     &           'INPUT1 - SN 2960+7: writing ? relay card to ',
     &           'history file.')
                call dbgwri('  IRECTP /record type/ = ', irectp)
                call dbgwri('  IDESC /rec descrip/  = ', idesc)
                call dbgwri('  IRECLN /rec length/  = ', irecln)
                call dbgwrc('  Partial card written = ', buffer(1:40))
              endif
              call puthisrc(irectp, idesc, irecln, buffer)
              if (id .eq. '0') id = ' '
C             
C             FOR THESE RELAY TYPES SECTION NUMBER (KSECT) MUST BE
C             ZERO OR BLANK INPUT1
C             
              if (ksect .gt. 0) then
                write (errbuf(1), 10610) buffer
10610           format ('0', a)
                write (errbuf(2), 10620)
                call prterr('E', 2)
10620           format ('The previous Relay Card must have a zero sectio
     &n number')
                iabort = 1
                goto 510
              else
                lrly3c(1, lrd) = id
                lrly3n(1, lrd) = ksect
                lrly3c(2, lrd) = kchgcd
                lrly3n(2, lrd) = kecs
                lrly2c(lrd) = name2k
                lrly2n(lrd) = name2n
                lrly1c(lrd) = name1k
                lrly1n(lrd) = name1n
                goto 490
              endif

C             Section 44 - group 7 array build-up
C             
C             READING THE REMOTE RELAY DATA CARDS AND BUILDING IDENTIF
C             TABLES. THE LRROD TABLE IDENTIFIES THE LOCAL RELAY
C             THE LRROD2 TABLE IDENTIFIES THE REMOTE RELAY
C             
  320         if (igrp7 .ne. 7) then
                if (jgrp7 .eq. 7) goto 480
                igrp7 = 7
                ilast = 7
                kecs = TEMP_ECSADDR
              endif
              read (buffer, 10630) crlytp, name1k, base1, name2k,
     &         base2, id1, ksect1, kodec, name3k, base3, name4k, base4,
     &         id2, ksect2
10630         format (bz, 4x, a1, 1x, a8, f4.0, 1x, a8, f4.0, a1, i1,
     &         a1, a8, f4.0, 2x, a8, f4.0, a1, i1)
C             
C             WRITE RR CARD IMAGE TO SOLUTION HISTORY FILE
C             
              irectp = 47
              idesc = 256 + 80
              irecln = 20
              if (debug) then
                call dbgeko2(
     &           'INPUT1 - writing RR remote relay card to ',
     &           'history file.')
                call dbgwri('  IRECTP /record type/ = ', irectp)
                call dbgwri('  IDESC /rec descrip/  = ', idesc)
                call dbgwri('  IRECLN /rec length/  = ', irecln)
                call dbgwrc('  Partial card written = ', buffer(1:40))
              endif
              call puthisrc(irectp, idesc, irecln, buffer)
              if (name3k .eq. ' ' .and. name4k .eq. ' ') then
                write (errbuf(1), 10640) buffer
                write (errbuf(2), 10650)
                call prterr('E', 2)
10640           format ('0', a)
10650           format (
     &           '  THE BUS NAMES IN COLUMNS 35-60 ARE BOTH BLANK.')
                iabort = 1
                goto 510
              else
                name1n = nambas(base1)
                name2n = nambas(base2)
                if (name1n .eq. 0 .or. name2n .eq. 0) then
                   errflag = 1
                   iabort = 1
                   goto 510
                endif
                if (name3k .ne. ' ') then
                  name3n = nambas(base3)
                  if (name3n .eq. 0) then
                    errflag = 1
                    iabort = 1
                    goto 510
                  endif
                endif
                if (name4k .ne. ' ') then
                  name4n = nambas(base4)
                  if (name4n .eq. 0) then
                     errflag = 1
                     iabort = 1
                     goto 510
                  endif
                endif
                if (index ('MDR', kodec) .eq. 0) then
                  if (index ('BG', kodec) .ne. 0) goto 330
                  irtn = 3
                  goto 340
                endif
                if (name3k .eq. ' ' .or. name4k .eq. ' ') then
                  irtn = 4
                  goto 340
                endif
  330           if (index('DGO123U', crlytp) .eq. 0) then
                  irtn = 5
                  goto 340
                endif
                if (id1 .eq. '0') id1 = ' '
                if (id2 .eq. '0') id2 = ' '
C               
C               FOR REMOTE RELAYS,SECTION NUMBER KSECT2 MUST BE ZE
C               KSECT1 MUST BE ZERO OR BLANK UNLESS A SERIES CAPAC
C               THE CONTROLLING RELAY.
C               
                if (ksect1 .le. 0 .or. crlytp .eq. 'G') then
                  if (ksect2 .le. 0) then
                    lrr = lrr + 1
                    lrod1c(lrr) = name1k
                    lrod1n(lrr) = name1n
                    lrod2c(lrr) = name2k
                    lrod2n(lrr) = name2n
                    lrod4c(lrr) = name3k
                    lrod4n(lrr) = name3n
                    lrod5c(lrr) = name4k
                    lrod5n(lrr) = name4n
                    lrod3c(1, lrr) = id1
                    lrod3c(2, lrr) = kchgcd
                    lrod3n(1, lrr) = ksect1
                    lrod3n(2, lrr) = kecs
                    lrod6c(1, lrr) = id2
                    lrod6c(2, lrr) = kodec
                    lrod6n(lrr) = ksect2
                    goto 490
                  endif
                endif
                write (errbuf(1), 10660) buffer
                write (errbuf(2), 10670)
                call prterr('E', 2)
10660           format ('0', a)
10670           format ('The previous Remote Relay Card must have a zero
     & section number')
                iabort = 1
                goto 510

  340           write (outbuf, 10680) buffer
                call prtout(1)
10680           format ('0', a)
                if (irtn .eq. 2) then
                  write (errbuf(1), 10690)
                  call prterr('E', 1)
10690             format (1x,
     &             'YOU MUST ENTER AT LEAST ONE CONTROLLED BUS NAME.')
                  iabort = 1
                elseif (irtn .eq. 3) then
                  write (errbuf(1), 10700)
                  call prterr('E', 1)
10700             format (1x, 'COLUMN 34 MUST BE M, D, R, B, OR G.')
                  iabort = 1
                elseif (irtn .eq. 4) then
                  write (errbuf(1), 10710)
                  call prterr('E', 1)
10710             format (1x,
     &             'BOTH CONTROLLED BUSES MUST BE GIVEN WHEN COLUMN 34 I
     &S M, D, or R.'
     &             )
                  iabort = 1
                elseif (irtn .eq. 5) then
                  write (errbuf(1), 10720)
                  call prterr('E', 1)
10720             format (1x, 'COLUMN 5 MUST BE D,G,O,R,1,2,3, OR U')
                  iabort = 1
                else
                  target = 0.0
                  iabort = 1
                endif
                goto 510
              endif

C             Section 50 - group 8 array buildup
C             PROBLEM MODIFICATION -- CONTROL ON THE SWING PROBLEM INC
C             INITAL SHORT-CIRCUIT,BREAKER OPERATION,AND BRANCH IMPEDA
C             MODIFICATION
C             
  350         if (igrp8 .ne. 8) then
                if (jgrp8 .eq. 8) goto 480
                igrp8 = 8
                ilast = 8
C               
C               WRITE CASE CARD TO SOLUTION FILE TO BE PRINTED IN OU
C               
                irectp = 47
                idesc = 256 + 80
                irecln = 20
                if (debug) then
                  call dbgeko2('INPUT1 - writing CASE input card to ',
     &             'history file.')
                  call dbgwri('  IRECTP /record type/ = ', irectp)
                  call dbgwri('  IDESC /rec descrip/  = ', idesc)
                  call dbgwri('  IRECLN /rec length/  = ', irecln)
                  call dbgwrc('  Partial card written = ', casecrd(1
     &             :40))
                endif
                call puthisrc(irectp, idesc, irecln, casecrd)
              endif

              ifcd = ifcd + 1
              write (outbuf, 10730) buffer
              call prtout(1)
10730         format ('0', a)
              lscard(ifcd) = buffer

C             Jump to read next input card
              goto 510

C             Section 47 - group 9 array build-up
C             
C             CODING DC DATA CARDS
C             
  360         if (igrp9 .ne. 9) then
                if (jgrp9 .eq. 9) then
                  jgrp9 = 0
                  goto 480
                else
                  kecs = TEMP_ECSADDR
                  igrp9 = 9
                  ilast = 9
                endif
              endif
              if (subtyp .eq. 'S') then
                read (buffer, 10740) name1c, base1
10740           format (bz, 3x, a8, f4.0)
                name1n = nambas(base1)
                if (name1n .eq. 0) then
                   errflag = 1
                   iabort = 1
                   goto 510
                endif
                kode = 2
              else
                if (subtyp .eq. 'T') then
                  buffer(1:2) = 'DB'
                  subtyp = 'B'
                endif
                if (subtyp .eq. 'C') then
                  read (buffer, 10740) name1c, base1
                  namen = nambas(base1)
                  if (namen .eq. 0) then
                     errflag = 1
                     iabort = 1
                     goto 510
                  endif
                  kode = 3
                elseif (subtyp .eq. 'V') then
 
                  read (buffer, 10740) name1c, base1
                  namen = nambas(base1)
                  if (namen .eq. 0) then
                     errflag = 1
                     iabort = 1
                     goto 510
                  endif
                  kode = 4
                elseif (subtyp .eq. 'P') then
                  read (buffer, 10740) name1c, base1
                  namen = nambas(base1)
                  if (namen .eq. 0) then
                     errflag = 1
                     iabort = 1
                     goto 510
                  endif
                  kode = 5
                else
                  read (buffer, 10750) name1c, base, pori
10750             format (bz, 3x, a8, f4.0, 30x, a1)
                  namen = nambas(base)
                  if (namen .eq. 0) then
                     errflag = 1
                     iabort = 1
                     goto 510
                  endif
                  kode = 1
                endif
              endif
              jdc = jdc + 1
              idcbuc(jdc) = name1c
              idcbun(jdc) = namen
              idcinn(1, jdc) = kode
              idcinn(2, jdc) = kecs
              call ritecc(buf10, kecs, 8)
              kecs = kecs + 8

C             Jump to read next input card
              goto 510

  370         write (errbuf(1), 10760) buffer
              write (errbuf(2), 10770)
              call prterr('E', 2)
10760         format ('0', a)
10770         format (
     &         ' THIS DIRECT CURRENT CARD WITH SUBTYPE B OR C IS NO LONG
     &ER acceptable.'
     &         )
C             -  Jump to read next input card
              goto 510

C             Section 53 - group 10 array build-up
C             DECODING RB AND RC CARDS
 
  380         if (igrp10 .ne. 10) then
                if (jgrp10 .eq. 10) goto 480
                igrp10 = 10
                ilast = 10
                kecs = TEMP_ECSADDR
              endif
              write (outbuf, 10780) buffer
              call prtout(1)
10780         format (1x, a)
C             
C             WRITE DEFAULT DISTANCE RELAY CARD IMAGE TO SOLUTION
C             
              irectp = 47
              idesc = 256 + 80
              irecln = 20
              if (debug) then
                call dbgeko2(
     &           'INPUT1 - writing dflt dist relay card to ',
     &           'history file.')
                call dbgwri('  IRECTP /record type/ = ', irectp)
                call dbgwri('  IDESC /rec descrip/  = ', idesc)
                call dbgwri('  IRECLN /rec length/  = ', irecln)
                call dbgwrc('  Partial card written = ', buffer(1:40))
              endif
              call puthisrc(irectp, idesc, irecln, buffer)
              if (subtyp .eq. 'C') then
C               
C               CALL RCINP TO DECODE RC RESISTANCE BRAKE CARD
C               
                call rcinp()

C               Jump to read next input card
              else
C               
C               CALL RBINP TO DECODE RB RESISTANCE BRAKE CARD
C               
                call rbinp()
C               Jump to read next input card

              endif
              goto 510

C             Section 55 - group 11 array build-up
C             READING UNDERFREQUENCY LOAD SHEDDING RELAY CARDS  CF-1
C              ( RS )
C             
  390         if (igrp11 .ne. 11) then
                if (jgrp11 .eq. 11) goto 480
                kecs = TEMP_ECSADDR
                igrp11 = 11
                ilast = 11
              endif
              read (buffer, 10530) name1c, base1
              kb1 = nambas(base1)
              if (kb1 .eq. 0) then
                 errflag = 1
                 iabort = 1
                 goto 510
              endif
              nufreq = nufreq + 1
              ufreqc(nufreq) = name1c
              lufrqn(nufreq) = kb1
              kfreq(nufreq) = kecs
C             
C             WRITE RS CARD IMAGE TO SOLUTION HISTORY FILE
C             
              irectp = 47
              idesc = 256 + 80
              irecln = 20
              if (debug) then
                call dbgeko2('INPUT1 - writing RS relay card to ',
     &           'history file.')
                call dbgwri('  IRECTP /record type/ = ', irectp)
                call dbgwri('  IDESC /rec descrip/  = ', idesc)
                call dbgwri('  IRECLN /rec length/  = ', irecln)
                call dbgwrc('  Partial card written = ', buffer(1:40))
              endif
              call puthisrc(irectp, idesc, irecln, buffer)
              goto 490

C             Section 58 - group 12 array build-up
C             READING GENERATOR UNDERFREQUENCY RELAY CARDS AND BUILDIN
C             IDENTIFICATION TABLES
C              ( RM )
C             
  400         if (igrp12 .ne. 12) then
                if (jgrp12 .eq. 12) goto 480
                kecs = TEMP_ECSADDR
                igrp12 = 12
                ilast = 12
              endif
              read (buffer, 10860) name1c, base, id
              kb = nambas(base)
              if (kb .eq. 0) then
                 errflag = 1
                 iabort = 1
                 goto 510
              endif
              ngenf = ngenf + 1
              ugenfc(1, ngenf) = name1c
              ugenfc(2, ngenf) = id
              iugenn(ngenf) = kb
              kgenf(ngenf) = kecs
C             
C             WRITE RM CARD IMAGE TO SOLUTION HISTORY FILE
C             
              irectp = 47
              idesc = 256 + 80
              irecln = 20
              if (debug) then
                call dbgeko2('INPUT1 - writing RM relay card to ',
     &           'history file.')
                call dbgwri('  IRECTP /record type/ = ', irectp)
                call dbgwri('  IDESC /rec descrip/  = ', idesc)
                call dbgwri('  IRECLN /rec length/  = ', irecln)
                call dbgwrc('  Partial card written = ', buffer(1:40))
              endif
              call puthisrc(irectp, idesc, irecln, buffer)
              goto 490

C             Section 61 - group 13 array buildup
C             READING DEFAULT DISTANCE RELAY CARDS  ---  RL, RL V,
C             
  410         if (igrp13 .ne. 13) then
                if (jgrp13 .eq. 13) goto 480
                igrp13 = 13
                ilast = 13
C               
C               WRITE RELAY DATA CARD IMAGE TO SOLUTION HISTORY FI
C               
                irectp = 47
                idesc = 256 + 80
                irecln = 20
                if (debug) then
                  call dbgeko2(
     &             'INPUT1 - SN 5710+5: writing type ? relay card ',
     &             'history file.')
                  call dbgwri('  IRECTP /record type/ = ', irectp)
                  call dbgwri('  IDESC /rec descrip/  = ', idesc)
                  call dbgwri('  IRECLN /rec length/  = ', irecln)
                  call dbgwrc('  Partial card written = ', buffer(1:40)
     &             )
                endif
                call puthisrc(irectp, idesc, irecln, buffer)
              endif
C             
C             CALL RLREAD TO READ RL (DEFAULT DISTANCE RELAY) INPU
C             
              call rlread()
C             Jump to read next input card
              goto 510

C             Section 17 - build array for RV relay (part of group 13
C             (oughtta be moved to SN 5710)
C             
C              CALL RVREAD TO DECODE RV VOLTAGE DIFFERENCE LOAD DRO
C             
  420         if (fulltyp .eq. 'RV') then
                call rvread()
C               Jump to read next input card
                goto 510
              elseif (fulltyp .eq. 'LZ') then
 
C               READING BRANCH IMMITTANCE CARDS
 
                read (buffer, 10790) i1c, base1, j1c, base2, id, gi1,
     &           bi1, rr, xr, gj1, bj1
10790           format (bz, 4x, a8, f4.0, 2x, a8, f4.0, 1x, a1, 5x,
     &           6f7.4)
                inew = 0
                if (id .eq. '0') id = ' '
                ksect = 0
                kb1 = nambas(base1)
                kb2 = nambas(base2)
                if (kb1 .eq. 0 .or. kb2 .eq. 0) then
                  errflag = 1
                  iabort = 1
                  goto 510
                endif
                i1 = inam(i1c, kb1)
                j1 = inam(j1c, kb2)
                if ((i1 .eq. 0) .or. (j1 .eq. 0)) then
                  iabort = 1
                else
                  iline = iline - 1
                  ii = iline + iline
                  xrsq = rr*rr + xr*xr
                  gijs(iline) = rr/xrsq
                  bijs(iline) =  - xr/xrsq
                  ijslc(iline) = id
                  ijsln(1, iline) = i1
                  ijsln(2, iline) = j1
                  ijsln(3, iline) = ksect
C                 
C                 WRITE LZ CARD TO .SOL FILE
C                 
                  irectp = 47
                  idesc = 256 + 80
                  irecln = 20
                  if (debug) then
                    call dbgeko2('INPUT1 - writing LZ card to ',
     &               'history file.')
                    call dbgwri('  IRECTP /record type/ = ', irectp)
                    call dbgwri('  IDESC /rec descrip/  = ', idesc)
                    call dbgwri('  IRECLN /rec length/  = ', irecln)
                    call dbgwrc('  Partial card written = ', buffer(1
     &               :40))
                  endif
                  call puthisrc(irectp, idesc, irecln, buffer)
                  if ((gi1+bi1) .eq. 0.0) then
                    gsl(ii-1) = gj1
                    bsl(ii-1) = bj1
                  else
                    gsl(ii-1) = gi1
                    bsl(ii-1) = bi1
                  endif
                  if ((gj1+bj1) .eq. 0.0) then
                    gsl(ii) = gi1
                    bsl(ii) = bi1
C                   -  Jump to read next input card
                  else
                    gsl(ii) = gj1
                    bsl(ii) = bj1
C                   -  Jump to read next input card
                  endif
                endif
                goto 510
              elseif (fulltyp .eq. '  ') then
                goto 500
              elseif (type .eq. 'C') then
                goto 500
              elseif (fulltyp .eq. 'F1') then
                goto 440
              elseif (fulltyp .eq. 'FF') then
                goto 520
              else
                goto 480
              endif

C             Section 21 - convert exciter codes (Fx to Ex)
C             REASSIGN TYPE F CARDS TO TYPE E CARDS--NEW IEEE EXCITERS
 
  430         if (subtyp .eq. 'Z') then
                subtyp = 'X'
                buffer(1:2) = 'EX'
                type = 'E'
                goto 450
              elseif (subtyp .ne. '1') then
                if (subtyp .eq. 'A') then
                  buffer(1:2) = 'EM'
                  subtyp = 'M'
                  type = 'E'
                  goto 450
                elseif (subtyp .eq. 'B') then
                  buffer(1:2) = 'EN'
                  subtyp = 'N'
                  type = 'E'
                  goto 450
                elseif (subtyp .eq. 'C') then
                  buffer(1:2) = 'EO'
                  subtyp = 'O'
                  type = 'E'
                  goto 450
                elseif (subtyp .eq. 'D') then
                  buffer(1:2) = 'EP'
                  subtyp = 'P'
                  type = 'E'
                  goto 450
                elseif (subtyp .eq. 'E') then
                  buffer(1:2) = 'EQ'
                  subtyp = 'Q'
                  type = 'E'
                  goto 450
                elseif (subtyp .eq. 'F') then
                  buffer(1:2) = 'ER'
                  subtyp = 'R'
                  type = 'E'
                  goto 450
                elseif (subtyp .eq. 'G') then
                  buffer(1:2) = 'ES'
                  subtyp = 'S'
                  type = 'E'
                  goto 450
                elseif (subtyp .eq. 'H') then
                  buffer(1:2) = 'ET'
                  subtyp = 'T'
                  type = 'E'
                  goto 450
                elseif (subtyp .eq. 'J') then
                  buffer(1:2) = 'EU'
                  subtyp = 'U'
                  type = 'E'
                  goto 450
                elseif (subtyp .eq. 'K') then
                  buffer(1:2) = 'EV'
                  subtyp = 'V'
                  type = 'E'
                  goto 450
                elseif (subtyp .eq. 'L') then
                  buffer(1:2) = 'EW'
                  subtyp = 'W'
                  type = 'E'
                  goto 450
                else
                  goto 480
                endif
              endif
C             
C             DECODE F1 CARD
C             
  440         read (buffer, 10800) tbusfx, tzerox, iblod, dmpall, ispsw,
     &         iamrts
10800         format (bz, 4x, f5.4, 2x, f5.4, 1x, i1, 1x, f3.2, 1x, i1,
     &         1x, i1)
              if (buffer(5:9) .ne. ' ') then
                write (outbuf, 10801) tbusf, tbusfx
10801           format ('  Bus frequency filter changed from default (',
     & f6.3, ') to (', f6.2, ')')  
                call prtout (1)
                tbusf = tbusfx
              endif
              if (buffer(12:16) .ne. ' ') then
                write (outbuf, 10802) tzero, tzerox
10802           format ('  Minimum time constant changed from default ('
     &, f6.3, ') to (', f6.2, ')')  
                call prtout (1)
                tzero = tzerox
              endif
              irectp = 47
              idesc = 256 + 80
              irecln = 20
              if (debug) then
                call dbgeko('INPUT1 - writing F1 card to history file.'
     &           )
                call dbgwri('  IRECTP /record type/ = ', irectp)
                call dbgwri('  IDESC /rec descrip/  = ', idesc)
                call dbgwri('  IRECLN /rec length/  = ', irecln)
                call dbgwrc('  Partial card written = ', buffer(1:40))
              endif
              call puthisrc(irectp, idesc, irecln, buffer)
              if (iamrts .ne. 0.0) then
                write (errbuf(1), 10810)
10810           format (
     &           ' ALL TWO AXIS MACHINES WILL USE THE DEFAULT DAMPER ',
     &           'WINDING DATA ')
                write (errbuf(2), 10820)
10820           format (
     &           ' EVEN IF DAMPER WINDING DATA IS ENTERED FOR THE ',
     &           ' MACHINE.')
                call prterr('W', 2)
              endif
C             
C             IF SPARE POINTS HAVE BEEN REQUESTED, ZERO POINT TABL
C             
              if (ispsw .ne. 0) then
                do itrr = 1, MAXSP
                  sparpt(itrr) = 0.0
                enddo
              endif
              if (tbusf .ne. 0.0) then
                write (errbuf(1), 10830) tbusf
10830           format (5x,
     &           ' PROGRAM WILL FILTER BUS FREQUENCY USING A TIME ',
     &           'CONSTANT OF ', f5.4, ' SECONDS. ')
                call prterr('W', 1)
              endif
              if (tzero .ne. 0.0) then
                write (errbuf(1), 10831) tzero
10831           format (5x,
     &           ' PROGRAM WILL SET CERTAIN EXCITER, SVS, AND PSS TIME C
     &ONSTANTS > ', f6.2, ' SECONDS. ')
                call prterr('W', 1)
              endif
              if (iblod .ne. 0) then
                write (errbuf(1), 10840)
10840           format (5x, ' PROGRAM WILL SAVE REAL AND REACTIVE ',
     &           'BUS LOADS ON SOL FILE.')
                call prterr('W', 1)
              endif
              if (dmpall .ne. 0.0) then
                write (errbuf(1), 10850) dmpall
10850           format (5x,
     &           ' PROGRAM WILL FORCE ALL MACHINES TO HAVE A ',
     &           'DAMPING FACTOR OF ', f4.2)
                call prterr('W', 1)
C               -  Jump to read next input card
              endif
              goto 510
 
C             Section 22 - group 1 array build-up
C             READING MACHINE DATA CARDS AND BUILDING IDENTIFICATION T
C             
  450         if (igrp1 .ne. 1) then
                if (jgrp1 .eq. 1) goto 480
                igrp1 = 1
                ilast = 1
                kecs = TEMP_ECSADDR
              endif
              read (buffer, 10860) namec, base, id
10860         format (bz, 3x, a8, f4.0, a1)
              namen = nambas(base)
              if (namen .eq. 0) then
                 errflag = 1
                 iabort = 1
                 goto 510
              endif
              errflag = 0
              name1c = namec
              name2c = id

C             Machine cards
              if (type .eq. 'M') then
                if (index('CFIS', subtyp) .eq. 0) then
                  if (subtyp .eq. ' ') then
                    kode = 1
                    goto 470
                  elseif (subtyp .eq. 'Z') then
                    kode = 3
                    goto 470
                  elseif (subtyp .eq. 'B') then
                    buffer(71:) = typeb
                    buffer(2:2) = 'F'
                  else
                    goto 460
                  endif
                endif
                kode = 2
                goto 470
              elseif (type .eq. 'G') then
                if (index('SWCGHP2', subtyp) .ne. 0) then
C                 Governor card
                  kode = 4
                  goto 470
                endif
              elseif (type .eq. 'T') then
                if (index('ABCDEFGW', subtyp) .ne. 0) then
C                 Turbine card
                  kode = 5
                  goto 470
                endif
              elseif (type .eq. 'E') then
 
C               CHECK FOR 'OLD'  EXCITER SUBTYPES
 
                if (index('ABCDEFGHJKYZ', subtyp) .eq. 0) then
 
C                 CHECK FOR 'NEW' IEEE EXCITER SUBTYPES
 
                  if (index('MNOPQRSTUVW', subtyp) .eq. 0) then
                    if (subtyp .ne. 'X') goto 460
 
                    kode = 7
                    goto 470
                  endif
                endif
                kode = 6
                goto 470
              elseif (type .eq. 'S') then
                if (index('PFSZ', subtyp) .ne. 0) then

C                 Static VAR card
                  kode = 8
                  goto 470
                elseif (subtyp .eq. 'T') then
C                 
C                 TRANSIENT STABILIZER MODEL
C                 
                  kode = 12
                  goto 470
                endif
              elseif (type .eq. 'V') then
                kode = 9
                goto 470
              elseif (type .ne. 'W') then
                goto 480
              elseif (index('ABC', subtyp) .ne. 0) then
                kode = 10
                goto 470
              elseif (index('DEF', subtyp) .ne. 0) then
                kode = 11
                type = 'X'
                buffer(1:1) = 'X'
                goto 470
              endif
  460         write (errbuf(1), 10870) buffer
              write (errbuf(2), 10880)
              call prterr('E', 2)
10870         format ('0', a)
10880         format (' Correct TYPE but incorrect SUBTYPE.')
              iabort = 1
              goto 510
  470         mac = mac + 1
              i = 0
              mach1n(1, mac) = kecs
              mach1n(2, mac) = kode
              mach1n(3, mac) = namen
              mach1n(4, mac) = i
              mach1c(1, mac) = kchgcd
              mach1c(2, mac) = id
              mach2c(1, mac) = name1c
              mach2c(2, mac) = name2c
              mach2n(mac) = namen
              goto 490

C             Error messages in type/subtype parsing stage
  480         write (errbuf(1), 10890) buffer
              write (errbuf(2), 10900)
              call prterr('E', 2)
10890         format ('0', a)
10900         format (' Misplaced data card or incorrect type-subtype (C
     &olumn 1-2)')
              inew = 0
              if (ilast .eq. 9) jgrp9 = 0
              iabort = 1
              goto 510
  490         call ritecc(buf10, kecs, 8)
              kecs = kecs + 8

C             Jump to read next input card
              goto 510

        endif

C       Here if card is a comment ("C " or "  ")
C       Section 20 - save comment cards in history file
  500   continue
	if (fulltyp .eq. 'CV') then
          call cvinp(buffer)
        else
	  iswdte = 3
          ifirst = 1
          jfirst = 8
          inew = 0
          write (outbuf, 10040) buffer
          call prtout(1)
          irectp = 43
          idesc = 256 + 80
          irecln = 20
          call puthisrc(irectp, idesc, irecln, buffer)
        endif
C       Jump to read next input card
  510   continue
      enddo
C     
C     CALL FFREAD TO DECODE FF CARD
C     
  520 call ffread()
      tbusf = tbusf*frqbse
      nbrake = irb
      do itrr = 1, ntotd
C       
C       INITIALIZE BUS FREQUENCY AND FILTERED BUS FREQUENCY TABLES
C       AND TIME FACTOR ABUS
C       
        deltfq(itrr) = 0.0
        delfqo(itrr) = 0.0
      enddo
      if (tbusf .ne. 0.0) then
        abus = 1.0 + 2.0*tbusf/dt
        abusr = 1.0/abus
      endif
C     GO TO 1320

C     Section 70 - Merge new dynamic data in with data base, if a DB
C     referenced.
      target = 0.0
      if (istop3 .ne. 0) then
        write (errbuf(1), 10920)
        call prterr('E', 1)
10920   format ('0 SDATAO Saved Data Output file will not be created.')
        iabort = 1
      endif
C     
C     CALL SDATA TO MERGE OLD AND NEW DATA
C     
      call sdata()
c dlc remove second call to mpost
c dlc      call mpost('INPUT1')
      if (iline .ne. MAXLS+1) then
        lst = iline
        do i = lst, MAXLS
          id = ijslc(i)
          j1 = ijsln(1, i)
          j2 = ijsln(2, i)
          ksect = ijsln(3, i)
          msortc(i) = exnamc(j1)
          msortn(i) = ixnamn(j1)
          msortc(i+MAXLS) = exnamc(j2)
          msortn(i+MAXLS) = ixnamn(j2)
          msortc(i+2*MAXLS) = id
          msortn(i+2*MAXLS) = ksect
          msortn(i+3*MAXLS) = i
        enddo
        lqiks = 14
        call qiksrt(lst, MAXLS, komp31, swap31)
        write (outbuf, 10930)
        call prtout(1)
        call skipln(1)
10930   format ('0 BRANCH IMMITTANCE CARDS')
        do i = lst, MAXLS
          name1c = msortc(i)
          kbase = msortn(i)
          base1 = basekv(kbase)
          name2c = msortc(i+MAXLS)
          kbase = msortn(i+MAXLS)
          base2 = basekv(kbase)
          id = msortc(i+2*MAXLS)
          nline = msortn(i+3*MAXLS)
          gx = gijs(nline)
          bx = bijs(nline)
          denom = gx*gx + bx*bx
          rr = gx/denom
          xr =  - bx/denom
          nline = 2*nline
          gi1 = gsl(nline-1)
          bi1 = bsl(nline-1)
          gj1 = gsl(nline)
          bj1 = bsl(nline)
          write (outbuf, 10940) ' LZ', name1c, base1, name2c, base2,
     &     id, gi1, bi1, rr, xr, gj1, bj1
10940     format (a3, 2x, a8, 2x, f5.1, 2x, a8, 2x, f5.1, 1x, a1, 4x,
     &     6f14.7)
          call prtout(1)
        enddo
      endif
      lst = iline
      ibrk = ibr - 1
      ibr5 = 5*ibrk
      kecs = ibr5 + 1
      write (outbuf, 10950)
      call prtout(1)
10950 format ('0 SUBROUTINE INPUT1 HAS BEEN PROCESSED.')
      if (noload .ne. 0) then
        lrep = 0
        ldz = 0
        ldar = 0
        write (outbuf, 10960)
        call prtout(1)
10960   format ('0 ALL LOADS ARE CONSTANT IMPEDANCE ')
      endif
      buffer = ' '
      return
      end
