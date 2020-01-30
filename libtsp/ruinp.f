C    %W% %G%
      subroutine ruinp
C     
C     This subroutine decodes the under frequency relay data card
c     and forms initial tables.  It is called by INPUT2.
C     
      include 'tspinc/params.inc'
      include 'tspinc/rucom.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/kntrly.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/workc.inc'
      include 'tspinc/bypass.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/param.inc'

      character*8 name1c, name2c
      character*1 idc, itrp
      data altorn /0.0174532/

C     -     begin     begin     begin     begin     begin     begin
      read (buffer, 10000) name1c, base1, itrp, name2c, base2, idc,
     & ftrip, trelyc, tripc, ttrp
10000 format (bz, 6x, a8, f4.0, a1, a8, f4.0, a1, 1x, f5.1, f5.2, f5.5,
     & 25x, f5.2)
      kb1 = nambas(base1)
      kb2 = nambas(base2)
      j1 = inam(name1c, kb1)
      j2 = inam(name2c, kb2)
      if (j1 .eq. 0 .or. j2 .eq. 0) then
        write (outbuf, 10010) buffer
10010   format ('0', a80)
        call prtout(1)
      else
        kntru = kntru + 1
        if (kntru .gt. MAXRU) then
          write (errbuf(1), 10020) MAXRU
10020     format (5x, ' YOU HAVE ENTERED MORE THAN ', i3,
     &     ' OUT OF STEP ', 'RELAY CARDS.  THE STUDY WILL ABORT.')
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
          icd3ru(kntru) = 1
          if (itrp .eq. 'C') icd3ru(kntru) = 2
          ibusru(kntru) = j1
          jbusru(kntru) = j2
          iparru(kntru) = idc
          icd1ru(kntru) = 1
          icd4ru(kntru) = 0
          ftrpru(kntru) =  - 6.283185*(1.-ftrip/frqbse)
          trlyru(kntru) = trelyc
          trpcru(kntru) = tripc
          timeru(kntru) =  - 1.0
          tripru(kntru) = ttrp
          ttimru(kntru) = 0.0
          wnowru(kntru) = 0.0
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
            write (errbuf(1), 10030) name1c, base1, name2c, base2, idc
10030       format (
     &       'NO BRANCH DATA CAN BE FOUND FOR UNDERFREQUENCY RELAY ',
     &       'CARD ', a8, f5.1, 2x, a8, f5.1, 1x, a1, 1x, i1)
            call prterr('E', 1)
          else
            gijru(kntru) = gkm
            bijru(kntru) = bkm
            gioru(kntru) = gk1
            bioru(kntru) = bk1
            gjoru(kntru) = gk2
            bjoru(kntru) = bk2
          endif
        endif
      endif
      return
      end
