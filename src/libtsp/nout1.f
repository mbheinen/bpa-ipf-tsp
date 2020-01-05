C    %W% %G%
      subroutine nout1
C     
C     THIS SUBROUTINE READS THE OUTPUT DATA CARDS AND FORMS TABLES
C     DESCRIBING THE REQUIRED OUTPUT DATA.  READS NEEDED DATA FROM
C     THE POWER FLOW DATA HISTORY FILE (FOR003) AND THE SWING
C     DATA HISTORY FILE (FOR008).  IT IS CALLED BY SWINGM.
C     
C     -  Revs:
C     Apr/24/92 - DEM - Took PF and SW case name vars from common
C                       block instead of local storage.
C                     - Initialized MGEN array with index including
C                       parameter.
C     -
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/titles.inc'
      include 'tspinc/comn56.inc'
      include 'tspinc/link56.inc'
      include 'tspinc/dc.inc'
      include 'tspinc/out512.inc'
      include 'tspinc/mvnout.inc'
      include 'tspinc/gdif.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/worst.inc'
      include 'tspinc/wgs.inc'
      include 'tspinc/wgf.inc'
      include 'tspinc/wga.inc'
      include 'tspinc/wgv.inc'
      include 'tspinc/fltim.inc'
      include 'tspinc/wcom.inc'
      include 'tspinc/mvn.inc'
      include 'tspinc/filter.inc'
      include 'tspinc/rddat.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/linmod.inc'
      include 'tspinc/ibusd.inc'
      include 'tspinc/igend.inc'
      include 'tspinc/nwgntn.inc'
      include 'tspinc/ilind.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/idctbl.inc'
      include 'tspinc/indx2n.inc'
      include 'tspinc/newtab.inc'
      include 'tspinc/geno.inc'
      include 'tspinc/mgen.inc'
      include 'tspinc/rdcal.inc'
      include 'tspinc/room.inc'
      include 'tspinc/spare1.inc'
      include 'tspinc/spare2.inc'
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/znox.inc' 
      include 'tspinc/rddtamvrpt.inc' 

      common /nam/jdelet, jdbus(100)

      equivalence (nolin1, noline(1)), (nolin2, noline(2)), 
     &            (nozapp, noline(3)), (nolin4, noline(4)), 
     &            (nolin5, noline(5)), (nolin6, noline(6)),
     &            (nolin7, noline(7)), (nolin8, noline(8))
      dimension indx(20)
      equivalence (kxtra, ktemp, temp)
      dimension alf(8), ialf(8), ktemp(20), temp(20), kxtra(20), 
     &          iopton(20), klm(10)
      character*8 bus1, bus2, pflbl, plotd, xoutpt, yend, 
     &            refbus, namn1, namn2, klm1, klm2, bus
      character*80 alf80
      character*2 ls
      dimension ibusop(4), fltime(100), ifltcd(100)
      equivalence (alf, alf80)

C     Logical functions
      logical dbghere, ibelng3, found

C     Local variables
      integer count(100), kount(100)
      character*10 bdate, pfcas, clabl1, clabl2, pversn, usrnam, 
     &             swr, auxpf
      character*1 ipar, itbc, id, refid, subtyp, type, contin
      dimension snlkxx(4, 10)
      character time1*8, date1*9
      character fulltyp*2
      character ch80*80
      character orgpgm*4
      logical debug, wgrid

      data pflbl, plotd/'PF DATA', 'VRSND001'/
      data yend, xoutpt/'END', 'OUTPUT'/

C     Begin     Begin     Begin     Begin     Begin     Begin

      debug = dbghere('NOUT1   ')
      call mpost('NOUT1')
      write (outbuf, 10000)
      call prtout(1)
      call skipln(1)
10000 format ('0 SUBROUTINE NOUT1')
      conv = 57.2957795
      degov = 180.0/3.14159265
      degrad = 57.2957795
      ctd = 0.72
      ct1 = 3.18
      ct2 = 3.18
      ifltkt = 0
      tlast =  - 1.0
      ibh = 0
      igh = 0
      ilh = 0
      mvadr1 = 0
      idifkt = 0
      mvkt = 0
      mvgkt = 0
      mvlkt = 0
      mvbkt = 0
      mvdkt = 0
      irdknt = 0
      ibswt = 0
      igswt = 0
      ilswt = 0
      idswt = 0
      ibdat = 1
      ibdatt = 1
      igdat = 1
      igdatt = 1
      ildat = 1
      ildatt = 1
      iddat = 1
      imod = 0
      ibgd = 0
      iggd = 0
      ilgd = 0

C     L11=11
      nus11 = 0
      ispknt = 0
      call time(time1)
      call date(date1)

C     Read 1st card after "90" card
      call readin()

C     See if an aux format option card                               !dem
      auxfmt = 'STD'
      if (buffer(1:4) .eq. '/AUX') then
        call nxtwrd(buffer, 5, k1b, k1e)
        if (k1b .gt. 0) then
          if (buffer(k1b:k1e) .eq. 'POST') auxfmt = 'PST'
        endif
        call putout('0', buffer)
        call prtout(1)
        alf80 = ' Auxiliary file will be in format for PostScript '//
     &   'plotting prog.'
        call puterr(1, alf80)
        call prterr('W', 1)
        call readin()
      endif

C     New aux format uses /keyword to identify sections
      if (auxfmt .eq. 'PST') then
        write (l11, '(2A)') '!  New format auxiliary file from BPA ', 
     &   'stability program'
        write (l11, '(2A)') '!  Stab case ', swcase
        write (l11, '(A)') '/TITLE'
        write (l11, '(4A,4X,A8,4X,A9)') 'CASE: ', swcase, 
     &   ' PROG VRSN: ', ver, time1, date1
      else
        write (l11, 10010) ver, time1, date1
10010   format ('C SWING VERSION ', a5, 4x, a8, 4x, a9)
      endif

C     INITIALIZE KTZPP IN CASE WE DON'T GO THRU LINEOUT
      ktzpp = 0
C     
C     L3 CONTAINS INFORMATION FROM THE POWER FLOW TAPE
C     L4 CONTAINS INFORMATION TO BE USED FOR PLOTTING
C     L8 CONTAINS INFORMATION FROM THE SWING SOLUTION DATA FILE
C     L11 CONTAINS AUXILIARY OUTPUT DATA
C     
      rewind l4
      call inithis('READ')
C     REWIND L8
      target = 0.0

C     Read first history record (case ID's)
      call gethisi(irectp, 1)
      if (debug) then
        call dbgeko('NOUT1 - reading intro record from history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
      endif
      call gethisc(orgpgm, 1)
      if (debug) call dbgwrcn('  ORGPGM /recorded prog name/ = ', 
     & orgpgm, 8)
      if (orgpgm .ne. 'STAB') then
        call puterr(1, 'File 8 is not a BPA Stability History file')
        call prterr('E', 1)
        call erexit()
      endif
      call gethisi(irecln, 1)
      call gethisc(plott, 2)
      call gethisc(ch80, 6)
      pfcase = ch80(1:10)
      swcase = ch80(13:22)
      ch80 = 'BPA Stability Program - history file version '//plott
      call putout('0', ch80)
      call prtout(1)
      if (debug) then
        call dbgwrc('  PLOTT /recorded prog versn/ = ', plott)
        call dbgwrc('  PLOTD /expected prog versn/ = ', plotd)
      endif
      if (plott .lt. plotd) then
        ch80 = 'The requested .SOL file must be read with an older '//
     &   'version of the program. '
        call puterr(1, ch80)
        call prterr('E', 1)
        call erexit()
      endif

      call upcase(pfcase)
      target = 0.0
C     if (debug) then                                                   !dem
C     call dbgeko ('NOUT1 - Comparison of case names')                !dem
C     call dbgwrc ('  PFCAS  = ',PFCAS)                               !dem
C     call dbgwrc ('  PFCASE = ',PFCASE)                              !dem
C     endif                                                             !dem
      msindx(1) = 1
      msindx(5) = 1
      msindx(8) = 1
      msindx(3) = 4
      msindx(4) = 4
      msindx(14) = 4
      msindx(2) = 2
      msindx(11) = 2
      msindx(13) = 2
      msindx(6) = 7
      msindx(7) = 7
      msindx(10) = 7
      msindx(9) = 12
      msindx(12) = 12
      ktcom = 0
      iwckt = 0
      iref = 0

      write (outbuf, 10070) swcase, pfcase
10070 format ('SWING CASE: ', a, ' POWERFLOW CASE: ', a)
      call hedlod()

C     ZEROING TABLES

      ka = MAXGEN*16
      do la = 1, ka
        mgen(la) = 0
      enddo
      do i = 1, 10
        ispcde(i) = 0
      enddo
      do i = 1, 8
        idcl(i) = 0
        dcmin(i) = 0.0
        dcmax(i) = 0.0
      enddo

C     Read in bus names from hist file
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT1 - reading bus names from ', 'history file.'
     &   )
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 19) then
        ntotd = idesc/256
        call gethisc(exnamc(1), irecln)
      else
        call puterr(1, 'NOUT1 - record of bus names missing.')
        call prterr('E', 1)
        call erexit()
      endif

C     read in bus KV codes
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT1 - reading bus KV codes from ', 
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 21) then
        call gethisi(ixnamn, irecln)
      else
        call puterr(1, 'NOUT1 - record of bus KV codes missing.')
        call prterr('E', 1)
        call erexit()
      endif
      namecs = 1

C     Read in KV code list
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT1 - KV code list from ', 'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 26) then
        ibxyz = irecln
        call gethisi(basekv, irecln)
      else
        call puterr(1, 
     &   'NOUT1 - KV code list missing from history file.')
        call prterr('E', 1)
        call erexit()
      endif
      if (auxfmt .eq. 'PST') write (l11, '(A)') '/SUBTITLE'

C     Begin loop for each comment card
      do while (.true.)
        target = 0.0
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT1 - reading a comment card from ', 
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif

C       Record types 35,39,43 are old type 1 comments (show w/plots)
        if (ibelng3(irectp, 35, 39, 43)) then
          call gethisc(alf80, irecln)
          call dbgwrc('  /partial card/ = ', alf80(1:40))
          jfirst = 1
          wgrid = .true.

C         Here when comment goes on a grid page (up to 15 allowed)
          ktcom = ktcom + 1
          if (ktcom .gt. 15) goto 110
          ind = (ktcom-1)*8
          do i = 1, 8
            clf(ind+i) = alf(i)
          enddo

C         Echo user comment to printout files
          write (outbuf, '(a,a80)') '0   ', alf80
          call prtout(1)
        elseif (irectp .eq. 47 .or. irectp .eq. 51) then
          call gethisc(alf80, irecln)
          call dbgwrc('  /partial card/ = ', alf80(1:40))
          jfirst = 2
          wgrid = .false.
        else
          goto 120
        endif
C       
C       OUTPUT TO AUXILIARY FILE
C       SWING AND POWERFLOW EXECUTION DATES, AND COMMENTS
C       
C       -  As of Nov/03/92, "CASE" & "LS" cards flagged when ! wgrid
C       -                   Text on plot pages when wgrid
C       
C       -  Show Stab & PF dates (1 record from solution file)
        target = 0.0
        if (auxfmt .eq. 'STD') then
          if (wgrid) then
            write (l11, '(2A)') 'C ', alf80(3:80)
          else
            write (l11, '(A)') alf80
          endif
        endif
        if (auxfmt .eq. 'PST') write (l11, '(A80)') alf80
C       
C       WRITE CASE CARD AND LS CARDS TO AUX FILE AND OUTPUT FILE IF
C       IFIRST = 2
C       Count number of case/switch cards (when ! wgrid)

        if (.not. wgrid) then
          iwckt = iwckt + 1
          wrscom(iwckt) = alf80
        endif

C       New aux format surpresses leading 'C '
        write (outbuf, '(a,a80)') '0   ', alf80
        call prtout(1)
        read (alf80, 10080) ls
10080   format (bz, a2, 34x, i1)
        if (ls .eq. 'LS') then
          read (alf80, 10080) ls, mde
          if (ls .eq. 'LS' .and. mde .gt. 0 .and. mde .lt. 4) then
            ifltkt = ifltkt + 1
            read (alf80, 10090) ifltcd(ifltkt), fltime(ifltkt)
10090       format (bz, 35x, i2, 2x, f6.0)
          endif
        endif
        read (alf80, 10080) ls
        if (ls .eq. 'FF') then
          read (alf80, 10100) frqbse
10100     format (bz, 54x, f2.0)
          if (frqbse .eq. 0.0) frqbse = 60.
        endif
        read (alf80, 10080) ls
        if (ls .eq. 'F1') then
          read (alf80, 10110) tbusf
10110     format (bz, 4x, f5.4)
        endif
  110   continue
      enddo
C     End of loop for comment & line switching cards

  120 if (ktcom .gt. 15) then
        write (errbuf(1), 10120) 15, ktcom
        call prterr('E', 1)
10120   format ('0 EXCEEDED MAXIMUM NUMBER OF COMMENT CARDS. LIMIT IS ',
     &    i3, ' NUMBER ENCOUNTERED IS ', i3)
        ktcom = 15
      endif

C     Here have first history record after last comment
C     Bypass unused bus count record if present
      target = 0.0

C     History record after last comment contains revised bus count
      if (irectp .eq. 53) then
        call gethisi(kbsknt, 1)
        if (debug) call dbgeko2(
     &   'NOUT1 - previous record had revised bus ', 'count.')
      else
        call puterr(1, 
     &   'NOUT1 -  revised bus count missing from history.')
        call prterr('E', 1)
        call erexit()
      endif

C     All comments written out, now can add prototype axis sections
C     to aux file if using new format.
      if (auxfmt .eq. 'PST') then
        write (l11, '(A)') '/END'
        write (l11, '(A)') '/XAXIS'
        write (l11, '(A)') '  TITLE time (cycles)'
        write (l11, '(A)') '  RANGE AUTO'
        write (l11, '(A)') '/END'
        write (l11, '(A)') '/YAXIS'
        write (l11, '(A)') '  TITLE ?'
        write (l11, '(A)') '  RANGE AUTO'
        write (l11, '(A)') '/END'
      endif
      iflt =  - 1
      kt = 0
C     
C     FORM TABLES OF FAULT START AND STOP TIMES
C     
      do l = 1, ifltkt
        if (ifltcd(l) .gt. 0 .and. iflt .lt. 0) then
          iflt = 1
          kt = kt + 1
          fstrt(kt) = fltime(l)
        endif
        if (ifltcd(l) .lt. 0 .and. iflt .gt. 0) then
          iflt =  - 1
          fstop(kt) = fltime(l)
        endif
      enddo
      tbusf = tbusf*frqbse
C     
C     HZCONV CONVERTS FREQUENCY IN RADIANS/CYCLE TO HERTZ
C     
      hzconv = frqbse/6.2831853
      ifltkt = kt

C     Read bus renumbering list
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT1 - reading alph-to-new bus renum list ', 
     &   'from history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 57) then
        call gethisi(indx2n, irecln)
      else
        call puterr(1, 'NOUT1 -  bus renum list missing.')
        call prterr('E', 1)
        call erexit()
      endif
      kx2nec = namecs + ntotd
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT1 - reading counts of gens & dc terms ', 
     &   'from history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 61) then
        call gethisi(isg, 1)
        call gethisi(ldc, 1)
        call gethisi(ldc1, 1)
      else
        call puterr(1, 'NOUT1 - gen & dc count record missing.')
        call prterr('E', 1)
        call erexit()
      endif
      ldco = 10*ldc

C     Fetch gen bus names
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT1 - reading gen names, & ID''s ', 
     &   'from history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 67) then
        call gethisc(nwgntc, irecln)
      else
        call puterr(1, 'NOUT1 - gen names & ID''s record missing.')
        call prterr('E', 1)
        call erexit()
      endif

C     Fetch gen bus kv level codes
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT1 - reading gen bus KV level codes ', 
     &   'from history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 69) then
        call gethisi(nwgntn, irecln)
      else
        call puterr(1, 'NOUT1 - gen bus KV codes missing.')
        call prterr('E', 1)
        call erexit()
      endif
      kigent = kx2nec + ntotd

C     Fetch revised bus names
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT1 - reading revised bus names ', 
     &   'from history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 75) then
        nmx = idesc/256
        call gethisc(newtbc, irecln)
      else
        call puterr(1, 'NOUT1 - revised bus names missing.')
        call prterr('E', 1)
        call erexit()
      endif

C     Fetch revised bus KV codes
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT1 - reading revised bus KV codes ', 
     &   'from history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 77) then
        call gethisi(inwtb, irecln)
      else
        call puterr(1, 'NOUT1 - revised bus KV codes missing.')
        call prterr('E', 1)
        call erexit()
      endif
      knewt = kigent + isg
      knext = knewt + nmx
      ksave = knext
      ibecs = knext
      kbecs = knext
      igecs1 = knext
      igecs = knext
      kgecs = knext
      ilecs = knext
      klecs = knext
      idecs = knext
      kdecs = knext
      if (kmdcbs .ne. 0) then
C       
C       CHECK POWER FLOW VERSION NUMBER
C       
        if (ntotcs .ne. 0) then
          write (errbuf(1), 10130)
10130     format ('0 DC DATA ERRORS IN DATAO FILE')
          write (errbuf(2), 10140)
10140     format ('SAVED BASE DATA FILE IS UNACCEPTABLE FOR TRANSIENT ST
     &ablility studies.')
          call prterr('E', 2)
          write (errbuf(1), 10150)
10150     format ('0 Datao file contains old multi-terminal dc data whic
     &h is no longer acceptable.')
          write (errbuf(2), 10160)
          call prterr('E', 2)
10160     format (' rerun your datao file using a newer version of the p
     &ower flow program.')
          call erexit()
        endif
      endif
      nmxp = ntotd
      kmbus = ntotd + 1
      kmbrch = ltoto + 1

C     THERE ARE NO DELETES IN THE NEW POWERFLO  **
      jdelet = 0

      write (outbuf, 10170) buffer
      call prtout(1)
10170 format ('0', a)
      read (buffer, 10280) type
      if (type .eq. 'M') then
C       
C       DECODING MAIN HEADER CARD
C       
        read (buffer, 10180, err=122) ipltkt, plprpg, tmax, exmax, 
     &    exmin, aref, edt, maxcyc, nskp, wtim1, wtim2, tstrt, nsymskp
10180   format (bz, 3x, i2, a1, f5.1, 2(1x, f10.5), 1x, f10.0, 1x, 
     &    f5.2, 1x, i5, 1x, i3, 1x, f4.0, 1x, f4.0, f5.1, i2)
        go to 124

  122   write (errbuf(1), 10182) buffer(1:80)
10182   format (' Data error in record (', a, ')')
        call prterr ('W', 1)
  124   continue

        if (nsymskp .eq. 0) nsymskp = 8
        if (tstrt .gt. tmax) then
          write (errbuf(1), 10190)
10190     format ('0', 5x, ' TSTRT MUST BE LESS THAN TMAX ON MH CARD.', 
     &     ' TSTRT IS SET TO 0.0')
          call prterr('W', 1)
          tstrt = 0.0
        endif
        if (wtim2 .le. 0.0) wtim2 =  - 1.0
        nskp = nskp - 1
        if (nskp .le. 0) nskp = 0
        if (exmax .le. 0.0) exmax = 1.3
        if (exmin .le. 0.0) exmin = 0.8
        if (maxcyc .lt. 650) maxcyc = 650
        if (edt .le. 0.0) edt = 60.
        if (ipltkt .le. 0) ipltkt = 4
        if (ipltkt .gt. 15) ipltkt = 15
        if (aref .le. 0.0) aref = 60.
        if (tstrt .le. 0.0) tstrt = 0.0
        if (aref .gt. 89.9 .and. aref .lt. 90.1) aref = 89.9
        tref = tan(aref/degrad)
        tmaxp = tmax
        if (tmaxp .le. 0.0) tmaxp = 10000.
        write (outbuf, 10200) exmax
10200   format ('0', 'EXMAX        =', f11.7)
        call prtout(1)
        write (outbuf, 10210) exmin
10210   format (1x, 'EXMIN        =', f11.7)
        call prtout(1)
        write (outbuf, 10220) nskp
10220   format (1x, 'NSKP         =', i11)
        call prtout(1)
        write (outbuf, 10230) maxcyc
10230   format (1x, 'MAXCYC       =', i11)
        call prtout(1)
        write (outbuf, 10240) edt
10240   format (1x, 'EDT          =', f11.4)
        call prtout(1)
        write (outbuf, 10250) ipltkt
10250   format (1x, 'IPLTKT       =', i11)
        call prtout(1)
        write (outbuf, 10260) aref
10260   format (1x, 'AREF         =', f11.6)
        call prtout(1)
        do while (.true.)
          call readin()
          write (outbuf, 10270) buffer
          call prtout(1)
10270     format ('0', a)
C         
C         CHECKING HEADER CARD TYPE
C         
          read (buffer, 10280) type, subtyp
10280     format (a1, a1)
          fulltyp = buffer(1:2)
          if (buffer(1:8) .eq. '/ENDPLOT') goto 270
          if (fulltyp .eq. '99') goto 270
C         
C         "RD" CARDS SPECIFIY THE TRIP LINE FOR AN R-RDOT RELAY
C         
          if (type .eq. 'R' .and. subtyp .eq. 'D') then
            irdknt = irdknt + 1
            read (buffer, 10290) rname1(irdknt), rbkv1(irdknt), rname2
     &       (irdknt), rbkv2(irdknt), rpar(irdknt), rzero(irdknt), 
     &       rslope(irdknt)
10290       format (bz, 3x, 2(a8, f4.0, 1x), a1, 1x, f5.2, 1x, f5.2)
            if (rzero(irdknt) .le. 0.0) rzero(irdknt) = 80.
            if (rslope(irdknt) .eq. 0.0) rslope(irdknt) =  - 15.
          elseif (type .eq. 'M' .and. subtyp .eq. 'V') then
            call mvdcde()
C           
C           CALL SPCDCE TO READ SP CARDS FOR SPARE POINTS OUTPUT
C           
          elseif (type .eq. 'S' .and. subtyp .eq. 'P') then
            call spdcde()
          else

C           Types:  <B>us <G>en a<L>ine <9>0_or_99 <D>C_link
            if (type .eq. 'B') then
C             
C             DECODING BUS HEADER CARD
C             
              read (buffer, 10300) subtyp
10300         format (1x, a1)
              if (subtyp .eq. 'H') then
C               -  Process -BH- card
                ibecs = knext
                kbecs = knext
                ibopkt = 4
                read (buffer, 10310) (ktemp(3*k-2), temp(3*k-1), 
     &           temp(3*k), k = 1, 4)
10310           format (bz, 8x, 4(i1, 3x, f6.0, 1x, f6.0, 1x))
                ibswt = 1
                k = 1
                do while (ktemp(k) .ge. 1)
                  if (ktemp(k) .gt. 4) goto 260
                  if (temp(k+1) .lt. temp(k+2)) then
                    write (errbuf(1), 10320) buffer
                    write (errbuf(2), 10330)
                    call prterr('E', 2)
10320               format ('0', a)
10330               format (1x, 'MAX.LT.MIN.')
                  else
                    ibh = ibh + 1
                    ibuscl(ibh) = ktemp(k)
                    busmax(ibh) = temp(k+1)
                    busmin(ibh) = temp(k+2)
                  endif
                  k = k + 3
                  if (k .ge. 14) goto 260
                enddo
                goto 260
              elseif (subtyp .eq. ' ') then
C               
C               DECODING INDIVIDUAL BUS CARD
C               
C               Process "B" card
                read (buffer, 10340) bus, base, (ibusop(itrr), itrr = 
     &           1, ibopkt), igroup
10340           format (bz, 3x, a8, f4.0, 4(2x, i1), 4x, i3)

C               IBUSOP subscript plot parm interp:
C               1:Volt_mag  2:Freq  3:MW_load  4:MVAR_load
                ibuskt = 0
                do itrr = 1, ibopkt
                  if (ibusop(itrr) .gt. 8) goto 250
                  if (ibusop(itrr) .gt. 3 .and. ibusop(itrr) .lt. 8) 
     &             nus11 = nus11 + 1
                  ibuskt = ibuskt + ibusop(itrr)
                enddo
                if (ibuskt .le. 0) goto 230
                if (bus .ne. 'INTRMDTE') then

C                 Get bus's node number from its name
                  kb = nambse(base)
                  k1 = innam5(bus, kb)
                  if (k1 .eq. 0) goto 260
                  k1 = indx2n(k1)
                elseif (newtbc(1) .eq. 'INTRMDTE') then
                  k1 = 1
                  kb = inwtb(1)
                else
                  k1 = nmx
                  kb = inwtb(nmx)
                endif
                if (ibdat .ne. 1) then

C                 Check if bus already on earlier bus plot card
                  kdum = ibdat - 1
                  do k = 1, kdum
                    if (ibusd(1, k) .eq. k1) goto 240
                  enddo
                endif

C               Store plotting specs for IBDAT_th bus requested 
c               (100 at most)
                ibusd(1, ibdat) = k1
                ibusd(6, ibdat) = iabs(igroup)
                do itrr = 1, ibopkt
                  ibusd(itrr+1, ibdat) = ibusop(itrr)
                enddo
                ibdat = ibdat + 1
                ibdatt = ibdatt + 1
                knext = knext + 4
                if (ibdatt .gt. 100) then
                  write (errbuf(1), 10350) buffer
                  write (errbuf(2), 10360)
                  call prterr('E', 2)
10350             format ('0', a)
10360             format (' NOUT1 - MORE THAN 100 BUS PLOT CARDS.')
                endif
                goto 260
              endif
            elseif (type .eq. 'G') then
C             
C             DECODING GENERATOR HEADER CARDS
C             
              read (buffer, 10370) subtyp, contin
10370         format (1x, 2a1)
              if (subtyp .eq. 'H') then
                if (contin .eq. 'C') then
C                 
C                 CONTINUATION GENERATOR HEADER CARD
C                 
                  read (buffer, 10380) (ktemp(3*k-2), temp(3*k-1), 
     &             temp(3*k), k = 1, 4)
10380             format (bz, 5x, 4(i2, 3x, f6.0, 1x, f6.0, 1x))
                else
                  igswt = 1
                  read (buffer, 10390) refbus, refbas, refid, 
     &             (ktemp(3*k-2), temp(3*k-1), temp(3*k), k = 1, 3)
10390             format (bz, 6x, a8, f4.0, 1x, a1, 1x, 3(i2, 3x, f6.0, 
     &             1x, f6.0, 1x))
                  igecs = knext
                  kgecs = knext
                  if (refbus .ne. ' ') then
                    kb = nambse(refbas)
                    iref = igensr(refbus, kb, refid)
                    if (iref .eq. 0) then
                      write (errbuf(1), 10400) buffer
                      write (errbuf(2), 10410)
                      call prterr('E', 2)
10400                 format ('0', a)
10410                 format (
     &                 ' THE REFERENCE BUS IDENTIFICATION CAN NOT BE LOC
     &ated in the nwgntc table'
     &                 )
                      goto 260
                    endif
                  endif
                endif
                k = 1
                do while (ktemp(k) .ge. 1)
                  if (ktemp(k) .gt. 14) goto 260
                  if (temp(k+1) .lt. temp(k+2)) then
                    write (errbuf(1), 10320) buffer
                    call prterr('E', 1)
                    write (errbuf(1), 10420) ktemp(k)
10420               format ('0', 
     &               ' MAXIMUM IS LESS THAN MINIMUM FOR GENERATOR CLASS'
     &               , 'IFICATION ', i3)
                    call prterr('E', 1)
                  else
                    igh = igh + 1
                    igencl(igh) = ktemp(k)
                    genmax(igh) = temp(k+1)
                    genmin(igh) = temp(k+2)
                  endif
                  k = k + 3
                  if (igh .gt. 2) then
                    if (igh .eq. 3 .and. contin .ne. 'C') goto 260
                    if (igh .gt. 6) then
                      if (igh .eq. 7) goto 260
                      if (igh .gt. 10) then
                        if (igh .eq. 11) goto 260
                        if (igh .ge. 14) goto 130
                      endif
                    endif
                  endif
                enddo
                goto 260
  130           if (igh .ne. 15) then
                  write (errbuf(1), 10430) buffer
                  write (errbuf(2), 10440)
                  call prterr('E', 2)
10430             format ('0', a)
10440             format (
     &             ' AT LEAST ONE CLASSIFICATION HAS BEEN DUPLICATED.')
                endif
                goto 260
              elseif (subtyp .eq. ' ') then
C               
C               ENCODING INDIVIDUAL GENERATOR CARD
C               
                read (buffer, 10450) bus, base, id, (igend(i, igdat), i 
     &           = 2, 17)
10450           format (bz, 3x, a8, f4.0, 1x, a1, 15i3, 13x, i3)

C               First subscript of gen plot req array IGEND
C                 1:Gen_num  2:Angle  3:Freq_dev  4:E_fd  5:Flux_E_pq
C                 6:Mn_fld_sat  7:P_mech  8:P_elec  9:Exc_sat 10:V_reg
C                 11:P_accel  12:Q_gen  13:V_pss_out  14:T_damp
C                 15:I_fd  16:Area_P_accel  17:Grp_code

                itot = 0
                do i = 2, 16
                  igenop = igend(i, igdat)
                  if (igenop .gt. 7) goto 250

C                 NUS11 counts number of parms to the aux file
                  if (igenop .gt. 3) nus11 = nus11 + 1
                  itot = itot + igenop
                enddo
                if (itot .le. 0) goto 230
                kb = nambse(base)
                k1 = igensr(bus, kb, id)
                if (k1 .eq. 0) goto 260
                if (igdat .ne. 1) then
                  kdum = igdat - 1
                  do k = 1, kdum
                    if (igend(1, k) .eq. k1) goto 240
                  enddo
                  if (igdatt .gt. 100) kdum = igdatt - igdat
                endif
                igend(1, igdat) = k1
                igend(17, igdat) = iabs(igend(17, igdat))
10460           format (1x, 17i6)
                igdat = igdat + 1
                igdatt = igdatt + 1
                knext = knext + 17
                if (igdatt .gt. MAXGEN) then
                  write (errbuf(1), 10470) buffer
                  write (errbuf(2), 10480) MAXGEN
                  call prterr('E', 2)
10470             format ('0', a)
10480             format (' MORE THAN ', i4, ' GENERATOR CARDS.')
                endif
                goto 260
              elseif (subtyp .eq. 'D') then
                call gdifrd()
                goto 260
              endif
            elseif (type .eq. 'L') then
C             
C             DECODING LINE HEADER CARDS
C             
              read (buffer, 10490) subtyp
10490         format (1x, a1)
              if (subtyp .eq. 'H') then
C               IF (FULLTYP .EQ. '99') GO TO 1900                                 !dem
C               IF (TYPE.EQ.A9) GO TO 1900
                ilecs = knext
                ilswt = 1
                klecs = knext
                read (buffer, 10500) (ktemp(3*k-2), temp(3*k-1), 
     &           temp(3*k), k = 1, 3), nsym, ntim, ctd, ct1, ct2
10500           format (bz, 4x, 3(i1, 3x, f6.0, 1x, f6.0, 1x), i3, 1x, 
     &           i3, 3(1x, f4.3))
                k = 1
C               
C               SET UP R - RDOT TIME CONSTANTS
C               
                ctd = ctd*60.
                ct1 = ct1*60.
                ct2 = ct2*60.
                if (ctd .eq. 0.0) ctd = 0.72
                if (ct1 .eq. 0.0) ct1 = 3.18
                if (ct2 .eq. 0.0) ct2 = 3.18
                do while (ktemp(k) .ge. 1)
                  if (ktemp(k) .gt. 3) goto 140
                  if (temp(k+1) .lt. temp(k+2)) then
                    write (errbuf(1), 10320) buffer
                    call prterr('E', 1)
                  else
                    ilh = ilh + 1
                    ibrncl(ilh) = ktemp(k)
                    brnmax(ilh) = temp(k+1)
                    brnmin(ilh) = temp(k+2)
                  endif
                  k = k + 3
                  if (k .ge. 8) goto 260
                enddo
                goto 260
  140           if (ktemp(k) .le. 3) then
                  write (errbuf(1), 10510)
10510             format ('0 DO NOT ENTER APPARENT LINE IMPEDANCE MAXIMU
     &m and mimimum limits on line header card.')
                  write (errbuf(2), 10511)
10511             format ('INSTEAD ENTER LIMITS ON INDIVIDUAL LINE CARD.
     &')
                  call prterr('E', 2)
                endif
                goto 260
              else
                if (subtyp .ne. 'C') then
                  if (subtyp .ne. ' ') then
                    if (subtyp .ne. 'M') goto 220
C                   
C                   DECODING IMPEDANCE MODIFICATION CARDS
C                   
                    imod = imod + 1
                    read (buffer, 10520) bus1, base1, bus2, base2, 
     &               ipar, (brnmod(i, imod), i = 2, 6)
10520               format (bz, 4x, a8, f4.0, 2x, a8, f4.0, 1x, a1, 5x, 
     &               5f7.4)
C                   
C                   WRITE 'LM' CARD IMAGE TO WRSCOM SO IT CAN BE 
C                   PRINTED ON THE PLOT FILE.
C                   
                    iwckt = iwckt + 1
                    wrscom(iwckt) = buffer
                    kb1 = nambse(base1)
                    kb2 = nambse(base2)
                    if (ipar .eq. '0') ipar = ' '
                    k1 = innam5(bus1, kb1)
                    k2 = innam5(bus2, kb2)
                    if ((k1 .ne. 0) .and. (k2 .ne. 0)) then
                      if (imod .ne. 1) then
                        kdum = imod - 1
                        do k = 1, kdum
                          if (lnmodn(1, k) .eq. k1) then
                            if (lnmodn(2, k) .eq. k2) then
                              if (lnmodc(imod) .eq. ipar) then
                                if (brnmod(6, k) .eq. brnmod(6, imod)) 
     &                           goto 240
                              endif
                            endif
                          endif
                        enddo
                      endif
                      lnmodn(1, imod) = k1
                      lnmodn(2, imod) = k2
                      lnmodc(imod) = ipar
                      write (outbuf, 10530)
                      call prtout(1)
                      write (outbuf, 10540) (lnmodn(i, imod), i = 1, 2)
     &                 , lnmodc(imod), (brnmod(i, imod), i = 2, 6)
                      call prtout(1)
10530                 format ('0', 5x, ' BRNMOD ')
10540                 format (1x, 2i5, 2x, a1, 5f12.3)
                      goto 260
                    else
                      imod = imod - 1
                      goto 260
                    endif
                  endif
                endif
C               
C               DECODING INDIVIDUAL LINE CARD
C               
                read (buffer, 10550) bus1, base1, bus2, base2, ipar, 
     &           ksect, iopt1, iopt2, iopt3, rmax, rmin, xmax, xmin, 
     &           igrp, iopt4, iopt5
10550           format (bz, 2x, 2(1x, a8, f4.0), 1x, a1, 1x, i1, 3i3, 4
     &           (1x, f6.4), 2x, i3, 2i2)
                if (iopt1 .gt. 8) goto 250
                if (iopt2 .gt. 8) goto 250
                if (iopt3 .gt. 8) goto 250
                if (iopt4 .gt. 8) goto 250
                if (iopt5 .gt. 8) goto 250
                if (iopt1 .gt. 3 .and. iopt1 .lt. 8) nus11 = nus11 + 1
                if (iopt2 .gt. 3 .and. iopt2 .lt. 8) nus11 = nus11 + 1
                if (iopt3 .gt. 3 .and. iopt3 .lt. 8) nus11 = nus11 + 1
                if (iopt4 .gt. 3 .and. iopt4 .lt. 8) nus11 = nus11 + 1
                if (iopt5 .gt. 3 .and. iopt5 .lt. 8) nus11 = nus11 + 1
                itot = iopt1 + iopt2 + iopt3 + iopt4 + iopt5
                if (itot .le. 0) goto 230
                kb1 = nambse(base1)
                kb2 = nambse(base2)
                if (.not. ((kb1 .eq. 0) .or. (kb2 .eq. 0))) then
                  if (ipar .eq. '0') ipar = ' '
                  k1 = innam5(bus1, kb1)
                  k2 = innam5(bus2, kb2)
                  if (.not. (k1 .eq. 0 .or. k2 .eq. 0)) then
                    if (ildat .ne. 1) then
                      kdum = ildat - 1
                      do k = 1, kdum
                        if (ilindn(1, 1, k) .eq. k1) then
                          if (ilindn(1, 2, k) .eq. k2) then
                            if (ilindc(1, k) .eq. ipar) then
                              if (ilindn(1, 3, k) .eq. ksect) goto 150
                            endif
                          endif
                        endif
                      enddo
                      goto 160
  150                 ildat = k
                      ildatt = ildatt - 1
                      goto 170
                    endif
  160               ilindn(1, 1, ildat) = k1
                    ilindn(1, 2, ildat) = k2
                    ilindn(1, 3, ildat) = ksect
                    ilindc(1, ildat) = ipar
                    ilindc(2, ildat) = bus1
                    ilindc(3, ildat) = bus2
                    ilindn(8, 1, ildat) = igrp
  170               if (subtyp .eq. 'C') then
                      ilindn(5, 2, ildat) = iopt1 + ilindn(5, 2, ildat)
                      ilindn(6, 2, ildat) = iopt2 + ilindn(6, 2, ildat)
                      ilindn(7, 2, ildat) = iopt3 + ilindn(7, 2, ildat)
                      if (iopt2 .gt. 0) then
                        brndn(2, 3, ildat) = rmax
                        brndn(3, 3, ildat) = rmin
                        brndn(4, 3, ildat) = xmax
                        brndn(5, 3, ildat) = xmin
                      endif
                      if (iopt3 .gt. 0) then
                        brndn(7, 3, ildat) = rmax
                        brndn(8, 3, ildat) = rmin
                        brndn(9, 3, ildat) = xmax
                        brndn(10, 3, ildat) = xmin
                      endif
c
c                     Search for TCSC if iopt4 > 0
c
                      if (iopt4 .gt. 0 .or. iopt5 .gt. 0) then
                        found = .false.
                        ii = 1
                        do while (ii .le. iznmax .and. .not. found)
                          if (bus1 .eq. bname(iznbus(ii)) .and.
     &                        base1 .eq. buskv(iznbus(ii)) .and.
     &                        bus2 .eq. bname(jznbus(ii)) .and.
     &                        base2 .eq. buskv(jznbus(ii)) .and.
     &                        ipar .eq. iznpar(ii)) then
                            found = .true.
                            if (iopt4 .gt. 0) then
                              ilindn(2, 2, ildat) = ii
                              ilindn(4, 1, ildat) = iopt4 
     &                                            + ilindn(4, 1, ildat)
                            endif
                            if (iopt5 .gt. 0) then
                              ilindn(2, 2, ildat) = ii
                              ilindn(4, 2, ildat) = iopt5
     &                                            + ilindn(4, 2, ildat)
                            endif
                          else if (bus1 .eq. bname(iznbus(ii)) .and.
     &                             base1 .eq. buskv(iznbus(ii)) .and.
     &                             bus2 .eq. bname(jznbus(ii)) .and.
     &                             base2 .eq. buskv(jznbus(ii)) .and.
     &                             ipar .eq. iznpar(ii)) then
                            found = .true.
                            if (iopt4 .gt. 0) then
                              ilindn(2, 2, ildat) = ii
                              ilindn(4, 1, ildat) = iopt4 
     &                                            + ilindn(4, 1, ildat)
                            endif
                            if (iopt5 .gt. 0) then
                              ilindn(2, 2, ildat) = ii
                              ilindn(4, 2, ildat) = iopt5
     &                                            + ilindn(4, 2, ildat)
                            endif
                          else
                            ii = ii + 1
                          endif
                        enddo
                      endif
                    else
                      ilindn(5, 1, ildat) = iopt1
                      ilindn(6, 1, ildat) = iopt2
                      ilindn(7, 1, ildat) = iopt3
                      brndn(9, 1, ildat) = rmax
                      brndn(9, 2, ildat) = rmin
                      brndn(10, 1, ildat) = xmax
                      brndn(10, 2, ildat) = xmin
                    endif
                    ildatt = ildatt + 1
                    ildat = ildatt
                    knext = knext + 10
                  endif
                endif
                goto 260
              endif
            elseif (type .eq. 'D') then
C             
C             DECODING DC HEADER CARD
C             
              read (buffer, 10560) subtyp
10560         format (1x, a1)
              if (subtyp .eq. 'H') then
                idecs = knext
                kdecs = knext
                read (buffer, 10570) (ktemp(3*k-2), temp(3*k-1), 
     &           temp(3*k), k = 1, 4)
10570           format (bz, 4x, 4(i1, 3x, 2f7.0))
                k = 1
                idswt = 1
                idh = 0
                do while (ktemp(k) .ge. 1)
                  if (ktemp(k) .gt. 9) goto 260
                  if (temp(k+1) .lt. temp(k+2)) then
                    write (outbuf, 10320) buffer
                    call prtout(1)
                  else
                    idh = idh + 1
                    idcl(idh) = ktemp(k)
                    dcmax(idh) = temp(k+1)
                    dcmin(idh) = temp(k+2)
                  endif
                  k = k + 3
                  if (k .ge. 11) goto 260
                enddo
                goto 260
              elseif (subtyp .eq. ' ') then

C               DECODING INDIVIDUAL DC OUTPUT CARDS

                read (buffer, 10580) bus1, base1, bus2, base2, itbc, 
     &           (ktemp(i), i = 1, 18), igrp
10580           format (bz, 3x, 2(a8, f4.0, 1x), a1, 1x, 18(i1, 1x), 
     &           i3)
                itot = 0
                k1 =  - 1
                k2 =  - 1
                if (bus1 .ne. '        ') then
                  kb1 = nambse(base1)
                  k1 = innam5(bus1, kb1)
                endif
                if (bus2 .ne. '        ') then
                  kb2 = nambse(base2)
                  k2 = innam5(bus2, kb2)
                endif

C               K1,K2 ARE -1 IF BUS NAMES ARE BLANK.  
C               K1,K2 ARE 0 IF BUS NAMES ARE NOT IN POWER FLOW.  
C               OTHERWISE K1,K2 ARE POSITIVE INTEGERS.

                if (itbc .eq. ' ') itb = 0
                if (itbc .eq. '0') itb = 0
                if (itbc .eq. '1') itb = 1
                if (itb .eq. 0 .or. itb .eq. 1) then
                  if (itb .ne. 1) then

C                   PROCESS TERMINAL OUTPUT REQUESTS.  
C                   BUS NAMES THAT ARE BLANK ARE NOT IN POWER FLOW, 
C                   OR NOT A CONVERTER AND ARE IGNORED.

                    k1p = k1
                    k2p = k2
                    if (k1p .gt. 0) then
                      k1p = indx2n(k1p)
                      if (k1p .le. 0) then
                        write (errbuf(1), 10590) bus1, base1
                        call prterr('W', 1)
10590                   format ('0', 2x, a8, 2x, f5.1, 2x, 
     &                   'IS NOT A DC CONVERTER BUS.  OUTPUT   REQUEST I
     &gnored.'
     &                   )
                      endif
                    endif
                    if (k2p .gt. 0) then
                      k2p = indx2n(k2p)
                      if (k2p .le. 0) then
                        write (errbuf(1), 10590) bus2, base2
                        call prterr('E', 1)
                      endif
                    endif
                    if (k1p .le. 0) then

C                     PROCESS DC BRANCH OUTPUT REQUEST
C                     FOR BRANCH OUTPUT BOTH K1,K2 MUST BE POSITIVE

                      if (k2p .le. 0) goto 260
                    endif
                  elseif (.not. (k1 .gt. 0 .and. k2 .gt. 0)) then
                    write (errbuf(1), 10600) buffer
10600               format ('0', a)
                    write (errbuf(2), 10610)
10610               format (' BRANCH AS DEFINED ABOVE IS NOT VALID AND I
     $ s ignored')
                    call prterr('W', 2)
                    goto 260
                  endif
                  itot = 0

C                 ZERO OUT ANY REQUESTS FOR FIRING ANGLES AND RH BUS DC CURRENT ON
C                 BRANCH OUTPUT REQUEST

                  if (itb .eq. 1) then
                    ktemp(1) = 0
                    ktemp(2) = 0
                    ktemp(4) = 0
                    do i = 7, 18
                      ktemp(i) = 0
                    enddo
                  endif
                  do i = 1, 18
                    itot = itot + ktemp(i)
                  enddo
                  if (itot .le. 0) goto 230
                  ind = 0
                  do i = 1, 18
                    iopton(i) = ktemp(i)
                    if (iopton(i) .le. 0) iopton(i) = 0
                    if (iopton(i) .gt. 7) goto 250
                    if (iopton(i) .gt. 3) nus11 = nus11 + 1
                    indx(i) = iopton(i)
                  enddo
                  if (iddat .ne. 1) then
                    kdum = iddat - 1

C                   THE FOLLOWING LOGIC CHECKS FOR DUPLICATE BUS AND 
C                   BRANCH OUTPUT REQUESTS.  IF FOUND THEY ARE IGNORED.

                    if (itb .gt. 0) then

C                     CHECK FOR DUPLICATE BRANCHES
C                     BRANCH REQUEST BUS2-BUS1 IS NOT A DUPLICATE OF 
C                     BRANCH REQUEST BUS1-BUS2

                      do k = 1, kdum

C                       BYPASS IF THE PAST ENTRY IS FOR DC TERMINAL

                        if (idctln(10, 5, k) .ne. 0) then
                          if (k1 .eq. idctln(2, 1, k)) then
                            if (k2 .eq. idctln(2, 2, k)) goto 180
                          endif
                        endif
                      enddo
                      goto 210

C                     Next two comparsion superseded by prior two
C                     IF(K1.NE.I1) GO TO 1871
C                     IF(K2.NE.I2) GO TO 1871

  180                 write (errbuf(1), 10620) buffer
10620                 format ('0', a)
                      write (errbuf(2), 10630)
10630                 format (' ABOVE DC BRANCH OUTPUT REQUEST ENCOUNTER
     &ed previously.  record ignored')
                      call prterr('W', 2)
                      goto 260
                    else
C                     CHECK FOR DUPLICATE BUSES
                      do k = 1, kdum

C                       BYPASS IF THE PAST ENTRY IS FOR A DC BRANCH

                        if (idctln(1, 5, k) .ne. 1) then
                          if (.not. (k1 .le. 0 .and. k2 .le. 0)) then
                            if (k1 .gt. 0) then
                              if (k1 .ne. idctln(2, 1, k)) then
                                if (k1 .ne. idctln(2, 2, k)) goto 190
                              endif
                              k1 = 0
                              do i = 1, 4
                                ktemp(i) = 0
                              enddo
                            endif
  190                       if (k2 .gt. 0) then
                              if (k2 .ne. idctln(2, 1, k)) then
                                if (k2 .ne. idctln(2, 2, k)) goto 200
                              endif
                              k2 = 0
                              do i = 5, 8
                                ktemp(i) = 0
                              enddo
                            endif
                          endif
                        endif
  200                   continue
                      enddo
                      if (.not. (k1 .gt. 0 .or. k2 .gt. 0)) then
                        write (errbuf(1), 10640) buffer
                        write (errbuf(2), 10650)
                        call prterr('W', 2)
10640                   format ('0', a)
10650                   format ('  BOTH BUS NAMES IN THE ABOVE WERE PREV
     &iously encountered.  card ignored.')
                        goto 260
                      endif
                    endif
                  endif

C                 STORE INFORMATION FOR ONLY CORRECT BRANCH/TERMINAL 
C                 OUTPUT REQUEST IN DCTBL(IDCTBL)

  210             if (iddat .gt. 50) then
                    write (errbuf(1), 10660) buffer
                    call prterr('E', 1)
10660               format ('0', a, 'MORE THAN 50 DC LINE CARDS.')
                    iddat = 51
                  else
                    idctlc(1, iddat) = bus1
                    idctln(1, 1, iddat) = kb1
                    idctlc(2, iddat) = bus2
                    idctln(1, 2, iddat) = kb2
                    idctln(2, 1, iddat) = k1
                    idctln(2, 2, iddat) = k2
                    do k = 1, 10
                      idctln(k, 3, iddat) = indx(k)
                    enddo
                    do k = 11, 18
                      idctln(k-8, 2, iddat) = indx(k)
                    enddo
                    idctln(1, 4, iddat) = igrp
                    idctln(1, 5, iddat) = itb
                    iddat = iddat + 1
                    knext = knext + 5
                  endif
                  goto 260
                else
                  write (errbuf(1), 10670) buffer
10670             format ('0', a)
                  write (errbuf(2), 10680)
                  call prterr('W', 2)
10680             format ('  VALUE OF ITB IN COLUMN 30 MUST BE 1,0, OR B
     &lank ... card ignored' )
                  goto 260
                endif
              else
                write (outbuf, 10710) buffer
                call prtout(1)
                goto 260
              endif
            else
              write (errbuf(1), 10690) buffer
10690         format ('0', a)
              write (errbuf(2), 10700)
              call prterr('E', 2)
10700         format ('0', 5x, ' THE CODE IN COLUMN 1 IS INCORRECT.')
              goto 260
            endif
  220       write (errbuf(1), 10710) buffer
10710       format ('0', a)
            write (errbuf(2), 10720) subtyp, ' '
10720       format ('0', 5x, 2(a1, 2x), 
     &       ' THE CODE IN COLUMN 2 IS INCORRECT.')
            goto 260
  230       write (errbuf(1), 10730) buffer
10730       format ('0', a)
            write (errbuf(2), 10740)
            call prterr('W', 2)
10740       format (' CARD IGNORED -- NO OUTPUT OPTIONS REQUESTED. ')
            goto 260
  240       write (errbuf(1), 10750) buffer
10750       format (1x, a)
            write (errbuf(2), 10760)
            call prterr('W', 2)
10760       format (
     &       '  THE IDENTIFICATION ON THIS CARD HAS ALREADY BEEN ENCOUNT
     &ered.  this card will be ignored.'
     &       )
            goto 260
  250       write (errbuf(1), 10770) buffer
10770       format ('0', a)
            write (errbuf(2), 10780)
            call prterr('W', 2)
10780       format ('  OUTPUT CODE MUST BE 0-7 -- CARD IGNORED')
          endif
  260     continue
        enddo
C       
C       DECODING THE 99 CARD
C       IWGFSW=1 MEANS OUTPUT THE 20 HIGHEST FREQUENCY DEVIATIONS
C       IWGVSW=1 MEANS OUTPUT THE 20 HIGHEST FIELD VOLT DEVIATIONS
C       IWGSSW=1 MEANS OUTPUT THE 20 HIGHEST PSS DEVIATIONS
C       IWGASW=1 MEANS OUTPUT THE 20 HIGHEST ANGLE DEVIATIONS
C       
  270   read (buffer, 10790) nop, nopz, iwgfsw, iwgvsw, iwgssw, iwgasw
10790   format (bz, 4x, i1, 2x, i1, 1x, i1, 1x, i1, 1x, i1, 1x, i1)
C       
C       IF GD CARDS EXIST, CALL GDIFMD TO MODIFY REQUESTED OUTPUT
C       TABLES SO THEY INCLUDE THE GENERATOR DIFFERENCE OPTIONS
C       
        if (idifkt .gt. 0) call gdifmd()
C       
C       IF MV CARDS EXIST, CALL MVMOD TO MODIFY REQUESTED OUPUT
C       TABLES SO THEY INCLUDE THE MV OPTIONS
C       
        if (mvkt .gt. 0) call mvmod()
        ibdatt = ibdatt - 1
        ibdat = ibdat - 1
        igdatt = igdatt - 1
        igdat = igdat - 1
        ildatt = ildatt - 1
        ildat = ildat - 1
C       
C       SORTING DC OUTPUT CARDS --- BUBBLE UP SORT ---
C       
        iddat = iddat - 1
        if (iddat .ne. 0) then

C         CONVERT DC TERMINAL BUS NUMBERS FROM EXTERNALS TO INTERNALS

          do i = 1, iddat
            k1 = idctln(2, 1, i)
            k2 = idctln(2, 2, i)
            if (k1 .gt. 0) k1 = indx2n(k1)
            if (k2 .gt. 0) k2 = indx2n(k2)
            idctln(2, 1, i) = k1
            idctln(2, 2, i) = k2
          enddo
          if (iddat .ne. 1) then
            lim = iddat + 1
            do while (.true.)
              kdone = 1
              lim = lim - 1
              namn1 = idctlc(1, 1)
              inum1 = idctln(1, 1, 1)
              namn2 = idctlc(2, 1)
              inum2 = idctln(1, 2, 1)
              do i = 2, lim
                komp = kompr(namn1, idctlc(1, i), kdum)
                if (komp .ge. 0) then
                  if (komp .eq. 0) then
                    if (inum1 .lt. idctln(1, 1, i)) goto 280
                    komp = kompr(namn2, idctlc(2, i), kdum)
                    if (komp .lt. 0) goto 280
                    if (komp .eq. 0) then
                      if (inum2 .le. idctln(1, 2, i)) goto 280
                    endif
                  endif
                  kdone = 2
                  il = i - 1
                  klm1 = idctlc(1, il)
                  klm2 = idctlc(2, il)
                  idctlc(1, il) = idctlc(1, i)
                  idctlc(2, il) = idctlc(2, i)
                  idctlc(1, i) = klm1
                  idctlc(2, i) = klm2
                  do ii = 1, 2
                    klm(ii) = idctln(1, ii, il)
                    idctln(1, ii, il) = idctln(1, ii, i)
                    idctln(1, ii, i) = klm(ii)
                  enddo

C                 <>  THE 3RD COLUMN OF IDCTBL WAS PACKED AND <>
C                 <>  CONTAINED 10 ITEMS.                     <>

                  do ii = 1, 10
                    klm(ii) = idctln(ii, 3, il)
                    idctln(ii, 3, il) = idctln(ii, 3, i)
                    idctln(ii, 3, i) = klm(ii)
                  enddo
                  do ii = 3, 10
                    klm(ii) = idctln(ii, 2, il)
                    idctln(ii, 2, il) = idctln(ii, 2, i)
                    idctln(ii, 2, i) = klm(ii)
                  enddo
                  do ii = 4, 5
                    klm(ii) = idctln(1, ii, il)
                    idctln(1, ii, il) = idctln(1, ii, i)
                    idctln(1, ii, i) = klm(ii)
                  enddo
                  do ii = 1, 2
                    klm(ii) = idctln(2, ii, il)
                    idctln(2, ii, il) = idctln(2, ii, i)
                    idctln(2, ii, i) = klm(ii)
                  enddo
                  goto 290
                endif
  280           namn1 = idctlc(1, i)
                inum1 = idctln(1, 1, i)
                namn2 = idctlc(2, i)
                inum2 = idctln(1, 2, i)
  290           continue
              enddo
              if (kdone .eq. 1) goto 300
            enddo
          endif
  300     continue
        endif
        istop5 = 0
        if (ibswt .eq. 0 .and. ibdat .gt. 1) then
          istop5 = 1
          write (errbuf(1), 10810)
10810     format ('0 THE BUS HEADER CARD IS MISSING BUT INDIVIDUAL BUS O
     &utput is requested.')
          call prterr('E', 1)
        endif
        if (igswt .eq. 0 .and. igdat .gt. 1) then
          istop5 = 1
          write (errbuf(1), 10820)
10820     format ('0 THE GENERATOR HEADER CARD IS MISSING BUT INDIVIDUAL
     & generator output is requested.')
          call prterr('E', 1)
        endif
        if (ilswt .eq. 0 .and. ildat .gt. 1) then
          istop5 = 1
          write (errbuf(1), 10830)
10830     format ('0 THE LINE HEADER CARD IS MISSING BUT INDIVIDUAL LINE
     & output is requested.')
          call prterr('E', 1)
        endif
        if (idswt .eq. 0 .and. iddat .gt. 1) then
          istop5 = 1
          write (errbuf(1), 10840)
10840     format ('0 THE DC HEADER CARD IS MISSING BUT INDIVIDUAL DC LIN
     &e output is requested.')
          call prterr('E', 1)
        endif
        if (istop5 .eq. 1) call erexit()
        kmodec = knext
        knext = kmodec + 6*25
        write (outbuf, 10850)
10850   format ('0 SUBROUTINE NOUT1 HAS BEEN PROCESSED.')
        call prtout(1)
      else
        write (errbuf(1), 10860)
10860   format ('0', 5x, 
     &   'OUTPUT CARDS MUST START WITH A MAIN HEADER CARD')
        call prterr('E', 1)
      endif
      return
      end
