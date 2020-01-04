C    %W% %G%
      subroutine rginp
C     
C     This subroutine decodes the series capacitor data card
c     and forms initial tables.  It is called by INPUT2.
C     
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/rgcom.inc'
      include 'tspinc/kntrly.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/workc.inc'
      include 'tspinc/bypass.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/namec.inc'

C     -  Local variables
      character id*1, nxid*1
      integer count1, count2, first1, first2, first
      complex y(2, 2), yeq(2, 2), y1(2, 2), y2(2, 2), yxy(2, 2), 
     &        y3(2, 2)
      complex*16 yscr(3, 3)
      character*8 name1c, name2c
      character*1 idc, borec
      logical debug
 
C     Begin     Begin     Begin     Begin     Begin     Begin

      debug = .true.
C     
C     READ SERIES CAPACITOR DATA CARDS FROM SCRATCH FILE
C     
      call whrec1('RG ', ilo, ihi, isz)
      ihi = ihi + 1
      if (debug) then
        call dbgeko('RGINP - reading series cap gap stuff from ',
     &   'temp file # 1')
        call dbgwri('  record # = ', ihi)
      endif
      read (l1, rec = ihi) ((work(ll, nn), ll = 1, 8), nn = 1, lsc)
C     read (l1,REC=178)((WORK(LL,NN),LL=1,8),NN=1,LSC)
      kntrg = 0
      do i = 1, lsc
        read (work80(i), 10000) name1c, base1, name2c, base2, idc,
     &   ksect, gapfc, reinc, retd, nrecyc, permsc, borec, ttrp
10000   format (bz, 6x, a8, f4.0, 1x, a8, f4.0, a1, i1, 3f5.2, i5,
     &   f5.2, 5x, a1, 9x, f5.2)
        buffer = work80(i)
        kb1 = nambas(base1)
        kb2 = nambas(base2)
        j1 = inam(name1c, kb1)
        j2 = inam(name2c, kb2)
        if (j1 .eq. 0 .or. j2 .eq. 0) then
          write (outbuf, 10010) (work(iy, i), iy = 1, 8)
10010     format ('0', 8a10)
          call prtout(1)
        elseif (borec .ne. 'B' .and. borec .ne. 'E') then
          write (outbuf, 10010) (work(iy, i), iy = 1, 8)
          write (errbuf(1), 10020) borec
          call prterr('E', 1)
10020     format ('0 LETTER (', a1,
     &     ') APPEARS IN COLUMN 64. THIS LETTER ', 'MUST BE B OR E.')
          iabort = 1
C         
C         CHECK NUMBER OF REINSERTION ATTEMPTS REQUESTED
C         
        elseif (nrecyc .gt. 15) then
          iabort = 1
          write (outbuf, 10010) (work(iy, i), iy = 1, 8)
          write (errbuf(1), 10030)
          call prterr('E', 1)
10030     format ('0',
     &     ' ERROR...SERIES CAP GAP FLASH RELAY REQUESTING ',
     &     'MORE THAN 15 REINSERTIONS')
        else
          if (iswbps .ne. 0) then
C           
C           STORE THIS LINE IN THE DEFAULT DISTANCE RELAY BYPASS T
C           
            nbypas = nbypas + 1
            kbpass(1, nbypas) = j1
            kbpass(2, nbypas) = j2
          endif
          kntrg = kntrg + 1
          if (kntrg .gt. MAXRG) goto 100
          if (idc .eq. '0') idc = ' '
          if (idc .eq. '0') idc = ' '
          ksecrg(kntrg) = iabs(ksect)
          if (borec .eq. 'B') kborrg(kntrg) = 1
          if (borec .eq. 'E') kborrg(kntrg) = 2
          ibusrg(kntrg) = j1
          jbusrg(kntrg) = j2
          iparrg(kntrg) = idc
          gpfcrg(kntrg) = gapfc
          rincrg(kntrg) = reinc
          retdrg(kntrg) = retd
          permrg(kntrg) = permsc
          nrecrg(kntrg) = nrecyc
          ttrprg(kntrg) = ttrp
          icd1rg(kntrg) = 1
          icd3rg(kntrg) = 0
          icd4rg(kntrg) = 0
          timprg(kntrg) = 0
          timrrg(kntrg) = 0
          timtrg(kntrg) = 0
C         
C         CALL GETYEQ TO OBTAIN ADMITTANCES FROM THE BRANCH DATA T
C         
          call getyeq(j1, j2, idc, ksect, yeq, y1, yxy, y2, y3, ierr)
          if (ierr .eq. -1) then
            write (errbuf(1), 10040) exnamc(j1), basekv(ixnamn(j1)),
     &       exnamc(j2), basekv(ixnamn(j2)), idc, ksect
10040       format (5x, 2(a8, 1x, f5.1, 1x), 1x, a1, 1x, i1)
            write (errbuf(2), 10050)
10050       format (5x,
     &       ' THIS LINE FROM AN RG SERIES CAPACITOR CARD COULD ',
     &       'NOT BE FOUND IN THE BRANCH DATA TABLE.')
            call prterr('E', 2)
            iabort = 1
          elseif (ierr .eq. -3) then
            write (errbuf(1), 10040) exnamc(j1), basekv(ixnamn(j1)),
     &       exnamc(j2), basekv(ixnamn(j2)), idc, ksect
            write (errbuf(2), 10060)
10060       format (5x,
     &       ' THIS LINE FROM AN RG SERIES CAPACITOR CARD IS ',
     &       'EITHER A DC LINE OR A REGULATING TRANSFORMER AND ',
     &       'CANNOT BE USED.')
            call prterr('E', 2)
            iabort = 1
          else
C           
C           GET EQUIVALENT PI ADMITTANCE FOR TOTAL LINE
C           
            gijarg(kntrg) =  - real(yeq(1, 2))
            bijarg(kntrg) =  - aimag(yeq(1, 2))
            gioarg(kntrg) = real(yeq(1, 1)+yeq(1, 2))
            bioarg(kntrg) = aimag(yeq(1, 1)+yeq(1, 2))
            gjoarg(kntrg) = real(yeq(2, 2)+yeq(2, 1))
            bjoarg(kntrg) = aimag(yeq(2, 2)+yeq(2, 1))
C           
C           GET EQUIVALENT PI ADMITTANCE FOR ALL SECTIONS TO THE L
C           OF KSECT
C           
            gijprg(kntrg) =  - real(y1(1, 2))
            bijprg(kntrg) =  - aimag(y1(1, 2))
            gioprg(kntrg) = real(y1(1, 1)+y1(1, 2))
            bioprg(kntrg) = aimag(y1(1, 1)+y1(1, 2))
            gjoprg(kntrg) = real(y1(2, 2)+y1(2, 1))
            bjoprg(kntrg) = aimag(y1(2, 2)+y1(2, 1))
C           
C           GET EQUIVALENT PI ADMITTANCE FOR ALL SECTIONS WITH
C           SECTION KSECT SHORT CIRCUITED
C           
            gijsrg(kntrg) =  - real(y3(1, 2))
            bijsrg(kntrg) =  - aimag(y3(1, 2))
            giosrg(kntrg) = real(y3(1, 1)+y3(1, 2))
            biosrg(kntrg) = aimag(y3(1, 1)+y3(1, 2))
            gjosrg(kntrg) = real(y3(2, 2)+y3(2, 1))
            bjosrg(kntrg) = aimag(y3(2, 2)+y3(2, 1))
          endif
        endif
      enddo
      goto 110
  100 write (errbuf(1), 10070) MAXRG
10070 format (' NUMBER OF SERIES CAPACITOR "RG" CARDS EXCEEDS ',
     & ' THE MAXIMUM OF ', i3)
      call prterr('E', 1)
      iabort = 1
  110 return
      end
