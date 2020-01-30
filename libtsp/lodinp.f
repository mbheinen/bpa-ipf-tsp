C    %W% %G%
      subroutine lodinp
C     
C     THIS SUBROUTINE FORMS THE DATA TABLES FOR THE LOAD
C     REPRESENTATION MODEL.  IT IS CALLED BY INPUT2.
C     
      include 'tspinc/params.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/param.inc'
      include 'tspinc/newton.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/ldrep.inc'
      include 'tspinc/ldshdn.inc'
      include 'tspinc/ldidxn.inc'
      include 'tspinc/ldndxp.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/workc.inc'
      include 'tspinc/areanz.inc'

      dimension larea(60), lzone(150), lbus(MAXWRK), nxznen(4, MAXBUS)
      character*80 larea, lzone, lbus
      equivalence (work80, lbus)

C     -  Local variables
      dimension temp(15)
      character*2 azonec, zonec
      character*1 scode, id, nxtznc, sbtypc
      character*10 areaic
      character*8 namec
      logical debug, finished

C     -     Begin     Begin     Begin     Begin     Begin     Begin
      call mpost('LODINP')
      debug = .false.
      do i = 1, ntot
        do j = 1, 4
          nxznen(j, i) = 0
        enddo
      enddo
C     
C     LREP IS THE NUMBER OF DATA CARDS ENTERED BY BUS
C     LDZ IS THE NUMBER OF DATA CARDS ENTERED BY ZONE
C     LDAR IS THE NUMBER OF DATA CARDS ENTERED BY AREA
C     
      if (lrep .ne. 0 .or. ldz .ne. 0 .or. ldar .eq. 0) then
        write (outbuf, 10000)
        call prtout(1)
10000   format ('0 CHECKING BUS AND BASE IDENTIFICATION ON LOAD ', 
     &   'REPRESENTATION CARDS.')
        if (lrep .ne. 0) then
          call whrec1('LDB', ilo, mslrep, isz)
          if (debug) then
            call dbgeko2('LODINP - reading indiv bus load rep stuff ', 
     &       'from temp file # 1')
            call dbgwri('  first record # = ', mslrep)
          endif
C         MSLREP=145
          if (lrep .ne. 0) then
            icount = 0
            finished = .false.
            do while (.not. finished)
              mslrep = mslrep + 1
              if (lrep - icount .gt. 100) then
                i100 = 100
              else
                i100 = lrep - icount
              endif
              read (l1, rec = mslrep) (lbus(icount+nn), nn = 1, i100)
              icount = icount + i100
              if (icount .ge. lrep) finished = .true.
            enddo
C           
C           READ BUS NAME FROM EACH BUS CARD AND MAKE SURE THE
C           BUS EXISTS.  LBUS CONTAINS THE CARD IMAGES FOR ALL
C           LOAD CARDS BY BUS.
C           
            ix = 0
            do i = 1, lrep
              ix = ix + 1
              read (lbus(i), 10010) namec, base
10010         format (bz, 3x, a8, f4.0)
              buffer = lbus(i)
              kb = nambas(base)
              j = inam(namec, kb)
              if (j .eq. 0) then
                write (outbuf, 10020) lbus(i)
10020           format ('0', a80)
                call prtout(1)
              else
C               
C               NXZNEN(3,J) = IX MEANS LOAD DATA FOR BUS NUMBER J HAS
C               BEEN ENTERED BY BUS CARD AND THE CARD IS LBUS(IX)
C               
                nxznen(3, j) = ix
              endif
            enddo
          endif
        endif
C       
C       READ ZONE ID FROM EACH ZONE CARD AND MAKE SURE THE
C       ZONE EXISTS.  LZONE CONTAINS THE CARD IMAGES FOR ALL
C       LOAD CARDS BY ZONE.
C       
        if (ldz .ne. 0) then
          call whrec1('LDZ', ilo, mslrep, isz)
          if (debug) then
            call dbgeko2('LODINP - reading zone load rep stuff from ', 
     &       'temp file # 1')
            call dbgwri('  first record # = ', mslrep)
          endif
C         MSLREP=141
          finished = .false.
          icount = 0
          do while (.not. finished)
            mslrep = mslrep + 1
            if (ldz - icount .gt. 100) then
              i100 = 100
            else
              i100 = ldz - icount
            endif
            read (l1, rec = mslrep) (lzone(icount+nn), nn = 1, i100)
            icount = icount + i100
            if (icount .ge. ldz) finished = .true.
          enddo

          iy = 0
          do j = 1, ldz
            iswt = 0
            iy = iy + 1
            read (lzone(j), 10030) zonec
10030       format (15x, a2)
            do jj = 1, ntot
C             
C             EXZONC(JJ) CONTAINS THE POWER FLOW ZONE THAT BUS JJ IS IN.
C             
              azonec = exzonc(jj)
              if (kompr(zonec, azonec, kdum) .eq. 0) then
C               
C                NXZNEN(2,JJ) MEANS LOAD DATA FOR BUS JJ HAS BEEN ENTERED
C                BY ZONE AND CARD IMAGE IS LZONE(IY)
C               
                nxznen(2, jj) = iy
                iswt = 1
              endif
            enddo
            if (iswt .ne. 1) then
              write (errbuf(1), 10040) zonec
10040         format ('0', a2, 5x, 
     &         ' THIS ZONE IS NOT FROM THE POWER FLOW.')
              call prterr('E', 1)
              iabort = 1
            endif
          enddo
        endif
        if (ldar .ne. 0) then

C         -  Load rep specs for an entire area
          iz = 0
          call whrec1('LDA', ilo, mslrep, isz)
          if (debug) then
            call dbgeko2('LODINP - reading zone load rep stuff from ', 
     &       'temp file # 1')
            call dbgwri('  first record # = ', mslrep)
          endif
C         MSLREP=139
          mslrep = mslrep + 1
          read (l1, rec = mslrep) (larea(nn), nn = 1, ldar)
C         
C         SEARCH THRU THE BUS LOAD DATA ENTERED BY AREA AND MAKE SURE
C         THE AREAS EXIST
C         
          do k = 1, ldar
            iswt = 0
            iz = iz + 1
            read (larea(k), 10050) areaic
10050       format (17x, a10)
C           
C           AREANC CONTAINS NAMES OF ALL AREAS IN THE STUDY
C           NTOTC IS THE NUMBER OF AREAS
C           
            do kk = 1, ntotc
              if (kompr(areaic, areanc(kk), kdum) .eq. 0) then
                iswt = 1
C               
C               AREAZC CONTAINS ALL ZONES IN EACH AREA
C               
                do jj = 1, 10
                  zonec = areazc(jj, kk)
                  if (zonec .eq. ' ') then
                    if (jj .ne. 1) goto 110
                  endif
                  do jjj = 1, ntot
C                   
C                   EXZONC CONTAINS ZONE ID FOR EACH BUS
C                   
                    azonec = exzonc(jjj)
C                   
C                   NXZNEN(1,JJJ) = IZ MEANS BUS NUMBER JJJ HAS LOAD DATA
C                   ENTERED BY AREA CARD IN LAREA(IZ)
C                   
                    if (kompr(zonec, azonec, kdum) .eq. 0) 
     &                nxznen(1, jjj) = iz
                  enddo
  110             continue
                enddo
              endif
            enddo
            if (iswt .ne. 1) then
              write (errbuf(1), 10060) areaic
10060         format (' Area ', a10, 
     &          ' ON AN LA OR LB  CARD IS NOT FROM THE POWER FLOW.')
              call prterr('E', 1)
              iabort = 1
              write (errbuf(1), 10070)
10070         format (' THE FOLLOWING AREAS ARE IN THE POWER FLOW DATA B
     &ASE:')
              call prterr('E', 1)
              do jjj = 1, ntotc, 7
                kkk = min0(jjj+6, ntotc)
                write (errbuf(1), 10080) (areanc(itrr), itrr = jjj, 
     &           kkk)
10080           format (5x, 7(a10, 2x))
                call prterr('E', 1)
              enddo
            endif
          enddo
        endif
C       
C       SEARCH THRU ALL BUSSES IN THE STUDY AND
C       OBTAIN LOAD REPRESENTATION AS ESTABLISHED BY HIERARCHY:
C       DATA ENTERED BY BUS BASE IS FIRST
C       DATA ENTERED BY ZONE IS SECOND
C       DATA ENTERED BY AREA IS THIRD
C       
        do j = 1, ntot
          do i = 1, jdelet
            if (jdbus(i) .eq. j) goto 130
            if (jdbus(i) .gt. j) goto 120
          enddo
  120     iclas1 = nxznen(3, j)
          if (iclas1 .eq. 0) then
            iclas2 = nxznen(2, j)
C           
C           BUS J HAS LOAD DATA FROM ZONE CARD IN TABLE
C           LZONE(NXZNEN(3,J))
C           
            if (iclas2 .eq. 0) then
C             
C             BUS J HAS LOAD DATA FROM AREA CARD IN TABLE
C             LAREA(NXZNEN(3,J))
C             
              iclas3 = nxznen(1, j)
              if (iclas3 .ne. 0) then
                nxznen(3, j) = iclas3
                nxznen(4, j) = 3
              endif
            else
              nxznen(3, j) = iclas2
              nxznen(4, j) = 2
            endif
          else
C           
C           BUS J HAS LOAD DATA FROM BUS CARD TABLE
C           LBUS(NXZNEN(3,J))
C           
            nxznen(4, j) = 1
          endif
  130     continue
        enddo
        ipwr = 2
        kldid = kldin + 1
        kldidt = kldid
        kldint = kldin
        kbusld = kldid + 1
        kbuslt = kbusld
        istsw = 0
        i1 = 0
        mm = 0
        m = 0
        i3 = 1
        iecsl = 1
C       
C       PROCESS BUS LOAD DATA FOR EACH BUS IN THE STUDY
C       USING HIERARCHY OF BUS, ZONE, AREA ALREADY ESTABLISHED
C       
        do i = 1, ntot
C         
C         IPNT IS INDEX IN THE LOAD TABLE FOR THIS BUS
C         
          ipnt = nxznen(3, i)
          if (ipnt .eq. 0) then
            do k = 1, 6
              ldidxn(k, i3) = 0
            enddo
          else
C           
C           ICLASS TELLS WHICH DATA TABLE THE LOAD DATA IS STORED
C           FOR THIS BUS
C           
            iclass = nxznen(4, i)
            psum = 0.0
            qsum = 0.0
            if (iclass .eq. 1) buff80 = lbus(ipnt)
            if (iclass .eq. 2) buff80 = lzone(ipnt)
            if (iclass .eq. 3) buff80 = larea(ipnt)
            read (buff80, 10090) sbtypc, namec, base, 
     &        (temp(k), k = 1, 8), xldp, xldq
10090       format (bz, 1x, a1, 1x, a8, f4.0, 12x, 10f5.3)

            xldp = xldp*100.0
            xldq = xldq*100.0
C           
C           INSERT TRANSFORMER LOAD SATURATION TEST
C           WARING!!! SATURATION DATA HAS NEVER BEEN DEBUGGED
C           
            read (buff80, 10100) dpt, dpq, scode
10100       format (bz, 67x, 2f5.3, 2x, a1)
            if (keybrd(32) .ne. 0) then
              kb = ixnamn(i)
              base = basekv(kb)
              write (outbuf, 10110) exnamc(i), base, buff80
              call prtout(1)
10110         format (1x, a8, 2x, f6.1, 5x, a80)
            endif
            ipnt = i
C           
C           TEST FOR TRANSFORER OR LOAD SATURATION REPRESENTATION.
C           IF SCODE IS BLANK, THERE IS NO SATURATION DATA
C           
            if (scode .eq. ' ') then
C             
C             INEWTS   =1 ADMITTANCE MATRIX SOLUTION
C             FOR CONSTANT IMPEDANCE LOAD, NO ENTRIES IN THE LOAD TABLES
C             ARE REQUIRED
C             
              if (inewts .ne. 2) then
                if (sbtypc .eq. 'A' .and. temp(1) .eq. 1.0 .and. 
     &              temp(2) .eq. 1.0) goto 150
              endif
              xldp = xldp / 6.28318
              xldq = xldq / 6.28318
              ldp = ifix(xldp)
              ldq = ifix(xldq)
C             
C             CHECK FOR FREQ. DEP. DATA CONSISTANCY.  OTHERWISE ABORT
C             ASSUME NO FREQUENCY DEPENDANCE TO BEGIN WITH IFREQ = 1
C            
              ifreq = 1
              if (ldp .ne. 0 .or. ldq .ne. 0) then
C               
C               SUB TYPE B FREQUENCY DEPENDENCE
C             
                if (sbtypc .eq. 'B') then
                  ifreq = 3
                  if (abs(temp(7)) .lt. .001 .or. 
     &                abs(temp(8)) .lt. .001) goto 140
                else
C                 
C                 SUB TYPE A FREQUENCY DEPENDENCE
C                 
                  ifreq = 2
                  if (abs(temp(7)) .ge. 0.001 .or. 
     &                abs(temp(8)) .ge. 0.001) goto 140
                endif
                iabort = 1
                write (errbuf(1), 10120) buff80
10120           format ('0', a80)
                write (errbuf(2), 10130)
10130           format ('  FREQ. DEP. DATA INCONSISTANT WITH ', 
     &           'SUBTYPE IN THE ABOVE CARD.')
                call prterr('E', 2)
              endif
            else
              ifreq = 4
              if (scode .eq. 'D') ifreq = 5
C             
C             PERFORM ERROR CHECKING FOR SATURATION DATA
C             
              if (temp(5) .eq. 0) temp(5) = bmva
              if (temp(6) .eq. 0.0 .and. ifreq .eq. 4) temp(6) = base
              if (ifreq .eq. 4) then
                if (temp(6) .gt. 1.1*base .or. temp(6) .lt. 0.9*base) 
     &            then
                  write (errbuf(1), 10140)
10140             format (' "KV BASE" is not within 10% ', 
     &             'of Bus base KV on following saturation record ')
                  errbuf(2) = ' '
                  write (errbuf(3), 10150) buff80
10150             format (1x, a80)
                  call prterr('E', 3)
                  iabort = 1
                endif
              endif
              if (temp(7) .le. 0.0 .or. temp(7) .gt. 1.0) then
                write (errbuf(1), 10160)
10160           format (' Saturation Current should be ', 
     &           'between 0.0 and 1.0 on following saturation record ')
                errbuf(2) = ' '
                write (errbuf(3), 10170) buff80
10170           format (1x, a80)
                call prterr('E', 3)
                iabort = 1
              endif
              if (temp(8) .le. 1.0) then
                write (errbuf(1), 10180)
10180           format (' Saturation Voltage should be ', 
     &           'greater than 1.0 on following saturation record ')
                errbuf(2) = ' '
                write (errbuf(3), 10190) buff80
10190           format (1x, a80)
                call prterr('E', 3)
                iabort = 1
              endif
              if ((xldp .gt. 0.0 .and. xldq .le. 0.0) .or. 
     &            (xldp .le. 0.0 .and. xldq .gt. 0.0)) then
                write (errbuf(1), 10200)
10200           format (' If measured saturation values are specified, c
     &urrent and voltage are both required on following record.')
                errbuf(2) = ' '
                write (errbuf(3), 10210) buff80
10210           format (1x, a80)
                call prterr('E', 3)
                iabort = 1
              endif
            endif
  140       if (ifreq .ne. 1) ifqsw = 1
C           
C           I1 COUNTS THE NUMBER OF BUSES WITH LOAD REPRESENTATION
C           
            i1 = i1 + 1
            ldndxp(1, i1) = ldp
            ldndxp(2, i1) = ldq
            ldndxp(3, i1) = ifreq
            ldndxp(4, i1) = ipnt
  150       ityp1 = 0
            ityp2 = 0
            ityp3 = 0
            ityp4 = 0
            nitem = 0
C           
C           CHECK FOR CONSTANT IMPEDANCE LOADS
C           
            if (abs(temp(1)) .ge. .001 .or. 
     &          abs(temp(2)) .ge. .001) then
              psum = psum + temp(1)
              qsum = qsum + temp(2)
              m = m + 1
              ityp1 = 1
              nitem = nitem + 1
              busldn(1, m) = temp(1)
              busldn(2, m) = temp(2)
              istsw = 1
            endif
C           
C           CHECK FOR FREQUENCY SENSITIVE LOAD
C           
            if (abs(temp(7)) .ge. .001 .or. 
     &          abs(temp(8)) .ge. .001) then
C             
C             BYPASS PSUM TALLY FOR SATURATION DATA
C             
              if (ifreq .le. 3) then
                psum = psum + temp(7)
                qsum = qsum + temp(8)
              endif
              m = m + 1
              ityp2 = 2
              nitem = nitem + 1
              busldn(1, m) = temp(7)
              busldn(2, m) = temp(8)
            endif
C           
C           CHECK FOR CONSTANT MVA LOADS
C           
            if (abs(temp(5)) .ge. .001 .or. 
     &          abs(temp(6)) .ge. .001) then
C             
C             BYPASS PSUM TALLY FOR SATURATION DATA
C             
              if (ifreq .le. 3) then
                psum = psum + temp(5)
                qsum = qsum + temp(6)
              endif
              m = m + 1
              ityp3 = 3
              nitem = nitem + 1
              busldn(1, m) = temp(5)
              busldn(2, m) = temp(6)
            endif
C           
C           CHECK FOR CONSTANT CURRENT LOAD
C           
            if (abs(temp(3)) .ge. .001 .or. 
     &          abs(temp(4)) .ge. .001) then
              psum = psum + temp(3)
              qsum = qsum + temp(4)
              m = m + 1
              ityp4 = 4
              nitem = nitem + 1
              busldn(1, m) = temp(3)
              busldn(2, m) = temp(4)
            endif
            if (psum .lt. .999 .or. psum .gt. 1.001) then
              write (errbuf(1), 10220) buff80
              write (errbuf(2), 10230) psum
              call prterr('E', 2)
10220         format ('0', a80)
10230         format (5x, 'THIS CARD IS INCORRECT. SUM OF PER UNIT ', 
     &         'REAL POWERS NOT EQUAL 1. ', 5x, f6.3)
              iabort = 1
            else
              psum = 1.0
            endif
            if (qsum .lt. .999 .or. qsum .gt. 1.001) then
              write (errbuf(1), 10220) buff80
              write (errbuf(2), 10240) qsum
              call prterr('E', 2)
10240         format (5x, 'THIS CARD IS INCORRECT. SUM OF PER UNIT ', 
     &         'REACTIVE POWERS NOT EQUAL 1. ', 5x, f6.3)
              iabort = 1
            else
              qsum = 1.0
            endif
C           
C           ZERO OUT LDIDXF BEFORE STORING DATA
C           
            do k = 1, 6
              ldidxn(k, i3) = 0
            enddo
            ldidxn(1, i3) = ityp4
            ldidxn(2, i3) = ityp3
            ldidxn(3, i3) = ityp2
            ldidxn(4, i3) = ityp1
            ldidxn(5, i3) = nitem
            ldidxn(6, i3) = iecsl
            iecsl = iecsl + nitem
          endif
          i3 = i3 + 1
          kldin = kldint
        enddo
        kldid = kldidt
        kbusld = kbuslt
        lrep = i1
        if (lrep .eq. 0) ipwr = 1
        kshed = kbusld + 1
        keclst = kshed
      endif
      return
      end
