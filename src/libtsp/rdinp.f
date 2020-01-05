C    %W% %G%
      subroutine rdinp
C     
C     This subroutine decodes the distance relay data card
c     and forms initial tables.  It is called by INPUT2.
C     
      include 'tspinc/params.inc'
      include 'tspinc/rdcom.inc'
      include 'tspinc/kntrly.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/workc.inc'
      include 'tspinc/bypass.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/prt.inc'

      character*8 name1c, name2c
      character*1 idc, itrp
      data altorn/0.0174532/

      read (buffer, 10000) name1c, base1, itrp, name2c, base2, idc,
     & ksect, tcentr, tangle, tdia, ttrip, bcentr, bangle, bdia, btrip,
     & ttrp
10000 format (bz, 6x, a8, f4.0, a1, a8, f4.0, a1, i1, 4(f5.5, f5.2),
     & f5.2)
      kb1 = nambas(base1)
      kb2 = nambas(base2)
      j1 = inam(name1c, kb1)
      j2 = inam(name2c, kb2)
      if (j1 .eq. 0 .or. j2 .eq. 0) then
        write (outbuf, 10010) buffer
10010   format ('0', a80)
        call prtout(1)
      else
        kntrd = kntrd + 1
        if (kntrd .gt. MAXRD) then
          write (errbuf(1), 10020) MAXRD
10020     format (5x, ' YOU HAVE ENTERED MORE THAN ', i3,
     &     ' DISTANCE RELAY ', 'CARDS.  THE STUDY WILL ABORT.')
          call prterr('E', 1)
          iabort = 1
        else
          if (iswbps .ne. 0) then
C           
C           STORE THIS LINE IN THE DEFAULT DISTANCE RELAY BYPASS T
C           
            nbypas = nbypas + 1
            kbpass(1, nbypas) = j1
            kbpass(2, nbypas) = j2
          endif
          if (idc .eq. '0') idc = ' '
          if (idc .eq. '0') idc = ' '
          icd3rd(kntrd) = 1
          if (itrp .eq. 'C') icd3rd(kntrd) = 2
          ibusrd(kntrd) = j1
          jbusrd(kntrd) = j2
          iparrd(kntrd) = idc
          icd1rd(kntrd) = 1
          icd2rd(kntrd) = 1
          icd4rd(kntrd) = 0
          tangle = tangle*altorn
          trcrd(kntrd) = tcentr*cos(tangle)
          txcrd(kntrd) = tcentr*sin(tangle)
          trad = tdia/2.
          trd2rd(kntrd) = trad*trad
          bangle = bangle*altorn
          brcrd(kntrd) = bcentr*cos(bangle)
          bxcrd(kntrd) = bcentr*sin(bangle)
          brad = bdia/2.
          brd2rd(kntrd) = brad*brad
          bzdyrd(kntrd) =  - 1.0
          tzdyrd(kntrd) =  - 1.0
          triprd(kntrd) = ttrp
          ttrprd(kntrd) = ttrip
          btrprd(kntrd) = btrip
          ttimrd(kntrd) = 0.0
C         
C         CALL BRNCHY TO OBTAIN ORGINAL ADMITTANCES RELAYED LINE
C         
          call brnchy(j1, j2, idc, ierr, gkm, bkm, gmk, bmk, gk1, bk1, 
     &                gk2, bk2)
C         
C         IF IERR = -1, THEN THE LINE COULD NOT BE FOUND IN THE BR
C         DATA TABLES FROM THE POWER FLOW.
C         
          if (ierr .eq. -1) then
            iabort = 1
            write (errbuf(1), 10030) name1c, base1, name2c, base2, idc,
     &       ksect
            call prterr('E', 1)
10030       format ('NO BRANCH DATA CAN BE FOUND FOR DISTANCE RELAY ',
     &       'CARD.', a8, f5.1, 2x, a8, f5.1, 1x, a1, 1x, i1)
          else
            gijrd(kntrd) = gkm
            bijrd(kntrd) = bkm
            giord(kntrd) = gk1
            biord(kntrd) = bk1
            gjord(kntrd) = gk2
            bjord(kntrd) = bk2
          endif
        endif
      endif
      return
      end
