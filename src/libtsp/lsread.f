C    %W% %G%
      subroutine lsread
C     
C     THIS SUBROUTINE SORTS THE LS CARDS IN TIME ORDER,
C     MAKES ERROR CHECKS AND DECODES EACH CARD TO FORM SWITCHING T
C     IT IS CALLED BY INPUT1.
C     
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/packtn.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/fltopt.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/param.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/ibrnch.inc'
      include 'tspinc/lscrd.inc'

      common /ndxktn/ndxktn(2, MAXLS)

C     -  Functions

      logical dbghere

C     -  Local variables

      character*1 type, subtyp, kchgcd, id, id1, id2
      character*8 name2x, name3x, name4x
      character*8 namec, name1c, name2c, name1k, name2k, name3k, name4k
      character*8 i1c, j1c
      character*1 m1c, m2c
      character*1 minus
      logical debug

      data minus/'-'/

C     -     begin     begin     begin     begin     begin     begin

      debug = dbghere('LSREAD  ')
      rcpmva = 1./bmva
      nfcd = ifcd
      sum = 0.0
C     
C     SORTING LS CARDS IN TIME ORDER
C     
      do iii = 1, nfcd
        read (lscard(iii), 10000) cyc(iii)
10000   format (bz, 39x, f6.0)
      enddo
      iter = 1
      nfcdm = nfcd - 1
      do iii = 1, nfcdm
        if (iter .eq. 2) goto 100
        iter = 2
        ijk = nfcd - iii
        do kkk = 1, ijk
          if (cyc(kkk) .gt. cyc(kkk+1)) then
            lscrd = lscard(kkk)
            lscard(kkk) = lscard(kkk+1)
            lscard(kkk+1) = lscrd
            save = cyc(kkk)
            cyc(kkk) = cyc(kkk+1)
            cyc(kkk+1) = save
            iter = 1
          endif
        enddo
      enddo
  100 write (outbuf, 10010)
      call prtout(1)
10010 format ('0', 24x, '********************************************',
     & '***')
      write (outbuf, 10020)
10020 format ('0', 44x, ' SORTED LS CARDS ')
      call prtout(1)
      do iii = 1, nfcd
        write (outbuf, 10030) lscard(iii)
C       
C       WRITE LS CARD IMAGE TO SOLUTION FILE TO BE PRINTED IN OUTPUT
C       
        irectp = 47
        idesc = 256 + 80
        irecln = 20
        if (debug) then
          call dbgeko2('LSREAD - writing a switching card to the ',
     &     'history file')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
          call dbgwrc('  Partial card written = ', lscard(iii)(1:40))
        endif
        call puthisrc(irectp, idesc, irecln, lscard(iii))
C       WRITE(L8) 2,10,LSCARD(III)
        call prtout(1)
10030   format (1x, a80)
      enddo
      ifcd = 0
      do iii = 1, nfcd
        buffer = lscard(iii)
        ifcd = ifcd + 1
        read (buffer, 10040) mdei
10040   format (bz, 35x, i2)
        mdei = iabs(mdei)
        if (mdei .le. 3) then
          read (buffer, 10050) m1c, i1c, base1, m2c, j1c, base2, id,
     &     mflt(ifcd), cyc(ifcd), fltr(ifcd), fltx(ifcd), perct(ifcd)
10050     format (bz, 3x, 2(a1, a8, f4.0, 1x), a1, 3x, i2, 2x, 4f6.0)
          if (id .eq. '0') id = ' '
          ksect = 0
          kb1 = nambas(base1)
          kb2 = nambas(base2)
          ibrnhc(1, ibr) = i1c
          ibrnhn(1, 1, ibr) = kb1
          ibrnhc(2, ibr) = j1c
          ibrnhn(1, 2, ibr) = kb2
          ibrnhc(3, ibr) = id
          ibrnhn(1, 3, ibr) = ksect
          i1 = inam(i1c, kb1)
          j1 = inam(j1c, kb2)
          if (i1 .ne. 0 .and. j1 .ne. 0) then
C         
C         PROBLEM CONTROL CARDS FOR LOAD AND/OR GENERATION MODIFICATIO
C         
            ibrnhc(4, ibr) = id
            ibrnhn(1, 4, ibr) = i1
            ibrnhn(2, 4, ibr) = j1
            ibrnhn(3, 4, ibr) = ksect
            if (m1c .eq. minus) i1 =  - i1
            if (m2c .eq. minus) j1 =  - j1
            iftabn(ifcd) = i1
            jftabn(ifcd) = j1
            ipcdtc(ifcd) = id
            ipcdtn(ifcd) = ksect
            if (ibr .ne. 1) then
              ibrnhn(1, 5, ibr) = 0
 
C             CHECKING TO DETERMINE IF A SIMILAR LINE SWITCHING OR MODIF
C             CARD HAS ALREADY BEEN PROCESSED
 
              ibrp = ibr - 1
              do i = 1, ibrp
                if (ibrnhc(4, i) .eq. id .and. 
     &              ibrnhn(1, 4, i) .eq. j1 .and. 
     &              ibrnhn(2, 4, 1) .eq. i1 .and. 
     &              ibrnhn(3, 4, 1) .eq. ksect) goto 120
                if (ibrnhc(4, i) .eq. id .and. 
     &              ibrnhn(1, 4, i) .eq. i1 .and. 
     &              ibrnhn(2, 4, 1) .eq. j1 .and. 
     &              ibrnhn(3, 4, 1) .eq. ksect) goto 120
              enddo
            endif
            ibrnhn(1, 5, ibr) = ifcd
            ibr = ibr + 1
            goto 120
          endif
        elseif (mdei .le. 4) then
          read (buffer, 10060) name1c, base, idgnc(ifcd), mflt(ifcd),
     &     cyc(ifcd), tpp, tqp, tpc, tqc, tpz, tqz, tpg
10060     format (bz, 4x, a8, f4.0, a1, 18x, i2, 2x, f6.0, 7f5.0)
          tpg =  - abs(tpg)
          if (tpg .le. -90000.) then
            write (outbuf, 10070) name1c, base, idgnc(ifcd), cyc(ifcd)
            call prtout(1)
          endif
10070     format ('0  100 PER CENT GENERATION DROP REQUESTED AT ', a8,
     &     2x, f5.1, 2x, a1, ' AT ', f6.1, ' CYCLES')
          if (tpg .ne. 0.0) then
            if (idgnc(ifcd) .eq. 'L') then
              write (errbuf(1), 10080) buffer
10080         format ('0', a)
              write (errbuf(2), 10090)
10090         format (
     &         ' LP GEN DROP CARD IS IGNORED. LP GEN WILL BE DROPPED',
     &         ' IN PROPORTION WITH HP GEN DROP IF CARD GIVEN')
              call prterr('W', 2)
              ifcd = ifcd - 1
              goto 120
            endif
          endif
          kb = nambas(base)
          namen = kb
          ibt = inam(name1c, kb)
          if (ibt .ne. 0) then
C         
C         LINE SWITCHING FOR DC LINES -- LS CARDS WITH COLUMN 37 EQUAL 5
 
            iftabn(ifcd) = ibt
            tpp = tpp*rcpmva
            tqp = tqp*rcpmva
            if (tpc .ne. 0.0 .or. tqc .ne. 0.0) then
              tpc = tpc*rcpmva
              tqc = tqc*rcpmva
            endif
            dmzg(ifcd) = tpz*rcpmva
            dmzb(ifcd) = tqz*rcpmva
            dmpg(ifcd) = tpg*rcpmva
            if (tpg .le. -90000.) dmpg(ifcd) = tpg
            dmpln(1, ifcd) = tpp
            dmpln(2, ifcd) = tqp
            dmcln(1, ifcd) = tpc
            dmcln(2, ifcd) = tqc
            goto 120
          endif
        elseif (mdei .le. 5) then
          read (buffer, 10100) m1c, name1k, base1, m2c, name2k, base2,
     &     iid, mflt(ifcd), cyc(ifcd), fltr(ifcd), dpiord(ifcd), perct
     &     (ifcd)
10100     format (bz, 3x, a1, a8, f4.0, 1x, a1, a8, f4.0, 1x, i2, 3x,
     &     i1, 2x, 4f6.0)
          if (iabs(iid) .ne. 0) then
            if (iabs(iid) .le. 8) then
              kb1 = nambas(base1)
              kb2 = nambas(base2)
              i1 = inam(name1k, kb1)
              j1 = inam(name2k, kb2)
              if (i1 .eq. 0) goto 110
              if (j1 .eq. 0) goto 110
              if (m1c .eq. minus) i1 =  - i1
              if (m2c .eq. minus) j1 =  - j1
              iftabn(ifcd) = i1
              jftabn(ifcd) = j1
              ipcdtn(ifcd) = iid
              goto 120
            endif
          endif
          write (errbuf(1), 10110) buffer
10110     format ('0', a)
          write (errbuf(2), 10120)
10120     format ('0', ' column 33 must only be 1 thru 8')
          call prterr('E', 2)
C         
C         LINE SWITCHING FOR MULT DC
C         
        elseif (mdei .le. 6) then
          read (buffer, 10130) m1c, i1c, base1, m2c, j1c, base2, iopt,
     &     mflt(ifcd), cyc(ifcd), angblk(ifcd), dpiord(ifcd), perct
     &     (ifcd), ardc, eldc
10130     format (bz, 3x, 2(a1, a8, f4.0, 1x), i2, 3x, i1, 2x, 6f6.0)
          j1 = 0
          ndxktn(1, ifcd) = 0
          ndxktn(2, ifcd) = 0
          ioptm = iabs(iopt)
          if (ioptm .ge. 1) then
            if (ioptm .le. 8) then
              kb1 = nambas(base1)
              i16 = inam(i1c, kb1)
              if (i16 .eq. 0) goto 110
C             
C             IF NOT OPTION 2 OR 5 , THEN DO NOT WORRY ABOUT THE S
C             EXCEPTING OPTION 8
C             
              if (ioptm .eq. 2 .or. ioptm .eq. 5 .or. ioptm .eq. 8) then
                kb2 = nambas(base2)
                j16 = inam(j1c, kb2)
                if (j16 .eq. 0) goto 110
                if (ioptm .eq. 2) then
C                 
C                 CHECK THAT PERCNT IS .GT.0. AND .LT.100. WHEN IOPT
C                 
                  if (perct(ifcd) .le. 0 .or. perct(ifcd) .ge. 100) then
                    write (errbuf(1), 10140) buffer
10140               format ('0', a)
                    write (errbuf(2), 10150)
10150               format (
     &               '0 WHEN IOPT (COL.33) IS 2, PERCNT (COL.58-163) MUS
     &T BE .GT. 1.0. AND .LT.100.')
                    call prterr('E', 2)
                    iabort = 1
                  endif
                endif
                if (ioptm .eq. 5 .or. ioptm .eq. 8) then
                  if (ardc .le. 0.) ardc = 0.0
                  if (eldc .le. 0.) eldc = 0.0
                  fltr(ifcd) = ardc
                  fltx(ifcd) = eldc
                endif
                if (m1c .eq. minus) m1 =  - 1
                if (m2c .eq. minus) m2 =  - 1
                if (m1c .eq. ' ') m1 = 0
                if (m2c .eq. ' ') m2 = 0
C               
C               CHECK THAT BOTH BUS NAMES HAVE SAME SIGN AND WHEN DR
C               MULT DC BRANCH NO OTHER OPTION IS USED.
C               
                if (m1 .ne. 0 .or. m2 .ne. 0) then
                  if (m1 .ne. -1 .or. m2 .ne. -1) then
                    write (errbuf(1), 10160) buffer
10160               format ('0', a)
                    write (errbuf(2), 10170)
10170               format (
     &               '0  COLUMNS 4 AND 18 SHOULD BOTH BE EITHER BLANK OR
     &  - .')
                    call prterr('E', 2)
                    iabort = 1
C                   
C                   MULTDC BRANCH ONLY OPTION 5 IS USED.
C                   
                  elseif (ioptm .ne. 5 .and. ioptm .ne. 8) then
                    write (errbuf(1), 10180) buffer
10180               format ('0', a)
                    write (errbuf(2), 10190)
10190               format (
     &               '0 COLUMN 33 MUST BE  5 WHEN BOTH COLUMNS 14 AND 18
     & ARE  - .')
                    call prterr('E', 2)
                    iabort = 1
                  endif
                endif
                ndxktn(1, ifcd) = m1
                ndxktn(2, ifcd) = m2
                jftabc(ifcd) = j1c
                jftabn(ifcd) = kb2
              endif
              iftabc(ifcd) = i1c
              iftabn(ifcd) = kb1
              ipcdtn(ifcd) = iopt
              goto 120
            endif
          endif
          write (errbuf(1), 10200) buffer
10200     format ('0', a)
          write (errbuf(2), 10210)
10210     format ('0 COLUMN 33 MUST BE 1 THRU 7 ONLY')
          call prterr('E', 2)
 
C         PROBLEM CONTROL CARD FOR D.C. OFF-SET DAMPING
 
        elseif (mdei .le. 7) then
          read (buffer, 10060) name1c, base, idgnc(ifcd), mflt(ifcd),
     &     cyc(ifcd), gdam
          kb = nambas(base)
          ibt = inam(name1c, kb)
          if (ibt .ne. 0) then
 
C         READING FAST VALVING CARDS
 
            iftabn(ifcd) = ibt
            gdam = gdam*0.01
            dmpg(ifcd) = gdam
C           
C           PLEASE NOTE FAULT DAMPING QUANTITY IS BEING STORED IN
C           
            sum = sum + gdam
            goto 120
          endif
        elseif (mdei .le. 8) then
          read (buffer, 10060) name1c, base, idgnc(ifcd), mflt(ifcd),
     &     cyc(ifcd), pwr1, secnd1, pwr2, secnd2
          kb = nambas(base)
          ibt = inam(name1c, kb)
          if (ibt .ne. 0) then
C         
C         DECODE MANUAL TRANSIENT STABILIZER TRIP OR MANUAL SVS FR
C         
            iftabn(ifcd) = ibt
            pwr = pwr2 - pwr1
            secnd = secnd2 - secnd1
            if (secnd .gt. 0.) then
              fvslpe = pwr/secnd
C             
C             FAST VALVING CURVE IS CALCULATED AS PERCENT PER SECOND AND
C             STORED IN DMPG TABLE
C             
              dmpg(ifcd) = fvslpe
              goto 120
            else
              write (errbuf(1), 10220) buffer
10220         format ('0', a)
              write (errbuf(2), 10230)
10230         format (
     &  '0  ERROR----TIME VALUES ON FAST VALVING CARD MUST INCREASe  ')
              call prterr('E', 2)
            endif
          endif
        elseif (mdei .gt. 9) then
          write (errbuf(1), 10240) buffer
10240     format ('0', a)
          write (errbuf(2), 10250)
10250     format ('0',
     &     '  COLUMN 37 MUST BE BETWEEN 1 AND 9 INCLUSIVE .')
          call prterr('E', 2)
        else
          read (buffer, 10260) name1c, base, idgnc(ifcd), ipcdtn(ifcd),
     &     mflt(ifcd), cyc(ifcd)
10260     format (bz, 4x, a8, f4.0, a1, 15x, i1, 2x, i2, 2x, f6.0)
          kb = nambas(base)
          ibt = inam(name1c, kb)
          if (ibt .ne. 0) then
            iftabn(ifcd) = ibt
            jftabn(ifcd) = 0
            goto 120
          endif
        endif
  110   ifcd = ifcd - 1
        iabort = 1
  120   continue
      enddo
      if (ifcd .le. 0) then
        write (errbuf(1), 10270)
10270   format ('0 NO LINE SWITCHING CARDS WERE READ ')
        call prterr('E', 1)
        istop3 = 1
      endif
      if (ifcd .gt. MAXLS) then
        write (errbuf(1), 10280) MAXLS
10280   format ('0 MORE THAN ', i3, ' LS CARDS ')
        call prterr('E', 1)
        istop3 = 1
      endif
      do j = 1, ntotd
        do i = 1, 3
          ipactn(i, j) = 0
        enddo
      enddo
C     
C     CODE GEN, LOAD SW
C     
      do j = 1, ifcd
        i1 = mflt(j)
        if (i1 .le. 6) then
          if (i1 .ne. 4) goto 130
          if (dmpg(j) .eq. 0.0) then
            if (dmpln(1, j) .eq. 0.0 .and. dmpln(2, j) .eq. 0.0) then
              if (dmcln(1, j) .eq. 0.0 .and. dmcln(2, j) .eq. 0.0) then
                if (dmzg(j) .eq. 0.0) then
                  if (dmzb(j) .eq. 0.0) goto 130
                endif
              endif
            endif
          endif
        endif
        i = iftabn(j)
        ipactn(3, i) = 1
  130   continue
      enddo
      if (ifltsw .eq. 1) then
        if (iabs(mflt(1)) .gt. 0 .and. iabs(mflt(1)) .lt. 4) goto 140
        write (errbuf(1), 10290)
10290   format ('0  THE FIRST SWITCHING CARD MUST BE AT FAULT CARD.',
     & ' IT MUST HAVE 1,2, OR 3 IN COL.37 AND A BLANK OR A - IN COL.36.'
     & )
        call prterr('E', 1)
        istop3 = 1
      endif
  140 if (abs(sum) .gt. 0.001) then
        write (errbuf(1), 10300)
10300   format ('0 PLEASE NOTE: THE SUM TOTAL OF ALL THE D.C. ',
     &   'OFF-SET DAMPING DOES NOT ADD UP TO ZERO')
        call prterr('E', 1)
      endif
      return
      end
