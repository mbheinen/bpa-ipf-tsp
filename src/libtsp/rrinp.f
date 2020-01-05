C    %W% %G%
      subroutine rrinp
C     
C     This subroutine decodes the remote relay
c     data card and forms initial tables.  It is called by INPUT2.
C     
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/rrcom.inc'
      include 'tspinc/kntrly.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/workc.inc'
      include 'tspinc/bypass.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/prt.inc'

C     -  Local variables
      character*8 name1c, name2c, blnk8
      character*1 idc, kod1, kod2, idc1
      logical debug
      data blnk8/'        '/
 
C     -     Begin     Begin     Begin     Begin     Begin     Begin
      debug = .true.
C     
C     READ REMOTE RELAY DATA CARDS FROM SCRATCH FILE
C     
      call whrec1('RR ', ilo, ihi, isz)
      ihi = ihi + 1
      if (debug) then
        call dbgeko('RRINP - reading remote relay stuff from ',
     &   'temp file # 1')
        call dbgwri('  record # = ', ihi)
      endif
      read (l1, rec = ihi) ((work(ll, nn), ll = 1, 8), nn = 1, lrr)

C     read (l1,REC=185)((WORK(LL,NN),LL=1,8),NN=1,LRR)

      kntrr = 0
      do i = 1, lrr
        read (work80(i), 10000) kod1, name1c, base1, name2c, base2,
     &   idc, ksect, kod2
10000   format (bz, 4x, a1, 1x, a8, f4.0, 1x, a8, f4.0, a1, i1, a1)
        buffer = work80(i)
        kb1 = nambas(base1)
        kb2 = nambas(base2)
        j1 = inam(name1c, kb1)
        j2 = inam(name2c, kb2)
        if (j1 .eq. 0 .or. j2 .eq. 0) then
          write (outbuf, 10010) (work(iy, i), iy = 1, 8)
10010     format ('0', 8a10)
          call prtout(1)
        else
          if (kod1 .eq. 'D') then
            typ = 1.
          elseif (kod1 .eq. 'O') then
            typ = 2.
          elseif (kod1 .eq. 'G') then
            typ = 4.
          elseif (kod1 .eq. '1') then
            typ = 5.
          elseif (kod1 .eq. '2') then
            typ = 6.
          elseif (kod1 .eq. '3') then
            typ = 7.
          elseif (kod1 .eq. 'U') then
            typ = 8.
          elseif (kod1 .eq. 'V') then
            typ = 9.
          else
            write (errbuf(1), 10020) kod1
10020       format (' THE CODE IN COLUMN 5 IS INCORRECT. CODE = ', a1)
            call mpost('RRINP')
            call prterr('E', 1)
            write (outbuf, 10010) (work(iy, i), iy = 1, 8)
            call prtout(1)
            iabort = 1
            goto 100
          endif
          kntrr = kntrr + 1
          if (kntrr .gt. MAXRR) goto 110
          ibs1rr(kntrr) = j1
          jbs1rr(kntrr) = j2
          ipr1rr(kntrr) = idc
          isc1rr(kntrr) = ksect
          rltprr(kntrr) = typ
C         
C         READ CONTROLLED LINE NAMES IF FOR LINE MODIFICATION
C         
          if (kod2 .eq. 'M' .or. kod2 .eq. 'D' .or. kod2 .eq. 'R') then
            read (work80(i), 10030) name1c, base1, name2c, base2, idc,
     &       ksect
10030       format (bz, 34x, a8, f4.0, 2x, a8, f4.0, a1, i1)
            buffer = work80(i)
            kb1 = nambas(base1)
            kb2 = nambas(base2)
            j1 = inam(name1c, kb1)
            j2 = inam(name2c, kb2)
            if (j1 .eq. 0 .or. j2 .eq. 0) then
C           
C             DECODE BUS LOAD DROPPING REMOTE RELAY
C           
              write (outbuf, 10010) (work(iy, i), iy = 1, 8)
              call prtout(1)
            else
              ibs2rr(kntrr) = j1
              jbs2rr(kntrr) = j2
              ipr2rr(kntrr) = idc
              isc2rr(kntrr) = ksect
C             
C             CALL BRNCHY TO OBTAIN ORGINAL ADMITTANCES FOR CONTRO
C             
              call brnchy(j1, j2, idc, ierr, gkm, bkm, gmk, bmk, gk1, 
     &                    bk1, gk2, bk2)
C             
C             IF IERR = -1, THEN THE LINE COULD NOT BE FOUND IN TH
C             DATA TABLES FROM THE POWER FLOW.
C             
              if (ierr .eq. -1) then
                iabort = 1
                write (errbuf(1), 10040) name1c, base1, name2c, base2,
     &           idc, ksect
                call prterr('E', 1)
10040           format (
     &           'NO BRANCH DATA CAN BE FOUND FOR REMOTE RELAY CARD ',
     &           a8, f5.1, 2x, a8, f5.1, 1x, a1, 1x, i1)
              else
                gijorr(kntrr) = gkm
                bijorr(kntrr) = bkm
                gioorr(kntrr) = gk1
                bioorr(kntrr) = bk1
                gjoorr(kntrr) = gk2
                bjoorr(kntrr) = bk2
C               
C               GET ADMITTANCES IF THIS RR MODIFIES A LINE
C               
                if (kod2 .eq. 'M') then
                  mkodrr(kntrr) = 1
                  read (work80(i), 10050) r, x, b
10050             format (bz, 62x, 3(f6.5))
                  denom = 1./(r*r+x*x)
                  gijmrr(kntrr) = r*denom
                  bijmrr(kntrr) =  - x*denom
                  biomrr(kntrr) = b
                  bjomrr(kntrr) = b
                endif
                if (kod2 .eq. 'D') mkodrr(kntrr) = 2
                if (kod2 .eq. 'R') mkodrr(kntrr) = 3
              endif
            endif
          elseif (kod2 .eq. 'B') then
            mkodrr(kntrr) = 4
            read (work80(i), 10060) name1c, base1, name2c, base2, drp1,
     &       drp2
10060       format (bz, 34x, a8, f4.0, 2x, a8, f4.0, 2x, 2f6.5)
            buffer = work80(i)
            if (name1c .ne. blnk8) then
              kb1 = nambas(base1)
              j1 = inam(name1c, kb1)
              if (j1 .eq. 0) then
                write (outbuf, 10010) (work(iy, i), iy = 1, 8)
                call mpost('RRINP')
                call prtout(1)
                kntrr = kntrr - 1
                goto 100
              else
                ibs2rr(kntrr) = j1
                shl1rr(kntrr) = drp1
              endif
            endif
            if (name2c .ne. blnk8) then
C           
C             DECODE REMOTE RELAY FOR GENERATOR DROPPING
C           
              kb2 = nambas(base2)
              j2 = inam(name2c, kb2)
              if (j2 .eq. 0) then
                write (outbuf, 10010) (work(iy, i), iy = 1, 8)
                call mpost('RRINP')
                call prtout(1)
                kntrr = kntrr - 1
              else
                jbs2rr(kntrr) = j2
                shl2rr(kntrr) = drp2
              endif
            endif
          elseif (kod2 .eq. 'G') then
            mkodrr(kntrr) = 5
            read (work80(i), 10070) name1c, base1, idc, name2c, base2,
     &       idc1, drp1, drp2
10070       format (bz, 34x, a8, f4.0, a1, 1x, a8, f4.0, a1, 1x, 2f6.5)
            buffer = work80(i)
            if (name1c .ne. blnk8) then
              kb1 = nambas(base1)
              j1 = inam(name1c, kb1)
              if (j1 .eq. 0) then
                write (outbuf, 10010) (work(iy, i), iy = 1, 8)
                call mpost('RRINP')
                call prtout(1)
                kntrr = kntrr - 1
                goto 100
              else
                ibs2rr(kntrr) = j1
                ipr2rr(kntrr) = idc
                gdr1rr(kntrr) = drp1
              endif
            endif
            if (name2c .ne. blnk8) then
              kb2 = nambas(base2)
              j2 = inam(name2c, kb2)
              if (j2 .eq. 0) then
                write (outbuf, 10010) (work(iy, i), iy = 1, 8)
                call mpost('RRINP')
                call prtout(1)
                kntrr = kntrr - 1
              else
                jbs2rr(kntrr) = j2
                jpr2rr(kntrr) = idc1
                gdr2rr(kntrr) = drp2
              endif
            endif
          else
            write (errbuf(1), 10080) kod2
10080       format (' THE CODE IN COLUMN 33 IS INCORRECT. CODE = ', a1)
            call mpost('RRINP')
            call prterr('E', 1)
            write (outbuf, 10010) (work(iy, i), iy = 1, 8)
            call prtout(1)
            kntrr = kntrr - 1
            iabort = 1
          endif
        endif
  100   continue
      enddo
      goto 120
  110 write (errbuf(1), 10090) MAXRR
10090 format (' NUMBER OF REMOTE RELAYS "RR" CARDS EXCEEDS ',
     & ' THE MAXIMUM OF ', i3)
      call mpost('RRINP')
      call prterr('E', 1)
      iabort = 1
  120 return
      end
