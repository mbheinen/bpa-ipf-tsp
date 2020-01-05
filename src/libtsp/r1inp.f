C    %W% %G%
      subroutine r1inp
C     
C     THIS SUBROUTINE DECODES THE RATE OF CHANGE OF POWER RELAY
C     DATA CARD AND FORMS INITIAL TABLES.  IT IS CALLED BY INPUT2.
C     
      include 'tspinc/params.inc'
      include 'tspinc/r1com.inc'
      include 'tspinc/param.inc'
      include 'tspinc/kntrly.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/workc.inc'
      include 'tspinc/bypass.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/prt.inc'

      character*8 name1c, name2c
      character*1 idc, itrp, subtyp

C     -     begin     begin     begin     begin     begin     begin
      read (buffer, 10000) subtyp, name1c, base1, itrp, name2c, base2,
     & idc
10000 format (bz, 1x, a1, 4x, a8, f4.0, a1, a8, f4.0, a1)
      kb1 = nambas(base1)
      kb2 = nambas(base2)
      j1 = inam(name1c, kb1)
      j2 = inam(name2c, kb2)
      if (j1 .eq. 0 .or. j2 .eq. 0) then
        write (outbuf, 10010) buffer
10010   format ('0', a80)
        call prtout(1)
      else
        kntr1 = kntr1 + 1
        if (kntr1 .gt. MAXR1) then
          write (errbuf(1), 10020) MAXR1
10020     format (5x, ' YOU HAVE ENTERED MORE THAN ', i3,
     &     ' POWER RATE ', 'RELAY CARDS.  THE STUDY WILL ABORT.')
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
          icd3r1(kntr1) = 1
          if (itrp .eq. 'C') icd3r1(kntr1) = 2
          ibusr1(kntr1) = j1
          jbusr1(kntr1) = j2
          iparr1(kntr1) = idc
          icd1r1(kntr1) = 1
          if (subtyp .eq. '1') icd2r1(kntr1) = 5
          if (subtyp .eq. '2') icd2r1(kntr1) = 6
          if (subtyp .eq. '3') icd2r1(kntr1) = 7
          icd4r1(kntr1) = 0
C         
C         DECODE R3 TYPE RELAY
C         
          if (subtyp .eq. '3') then
            read (buffer, 10030) rmax, rmin, rs, ps, tc, te, th, ttrp
10030       format (bz, 33x, 3f5.0, 5x, f5.0, 4f5.2)
            tc = tc*frqbse
            ps = ps/bmva
            denom = 1./(frqbse*bmva)
            rmax = rmax*denom
            rmin = rmin*denom
            rs = rs*denom
            rmaxr1(kntr1) = rmax
            rminr1(kntr1) = rmin
            tcr1(kntr1) = tc
            psr1(kntr1) = ps
            rsr1(kntr1) = rs
            ter1(kntr1) = te
            thr1(kntr1) = th
            ttrpr1(kntr1) = ttrp
            ttimr1(kntr1) = 0.0
          endif
C         
C         DECODE R1 AND R2 CARDS
C         
          if (subtyp .eq. '2' .or. subtyp .eq. '1') then
            read (buffer, 10040) rb, rmax, rs, bias, ps, tc, td, ttrp
10040       format (bz, 33x, 5f5.0, 2f5.2, 5x, f5.2)
            denom = 1./(frqbse*bmva)
            rb = rb*denom
            rs = rs*denom
            rmax = rmax*denom
            tc = tc*frqbse
            ps = ps/bmva
            bias = bias/bmva
            rbr1(kntr1) = rb
            rmaxr1(kntr1) = rmax
            rsr1(kntr1) = rs
            biasr1(kntr1) = bias
            psr1(kntr1) = ps
            tcr1(kntr1) = tc
            tdr1(kntr1) = td
            t1r1(kntr1) = td
            ttrpr1(kntr1) = ttrp
            ttimr1(kntr1) = 0.0
          endif
C         
C         CALL BRNCHY TO OBTAIN ORGINAL ADMITTANCES RELAYED LINE
C         
          call brnchy(j1, j2, idc, ierr, gkm, bkm, gmk, bmk, gk1, bk1, 
     &                gk2, bk2)
C         
C          IF IERR = -1, THEN THE LINE COULD NOT BE FOUND IN THE BR
C          DATA TABLES FROM THE POWER FLOW.
C         
          if (ierr .eq. -1) then
            iabort = 1
            ksect = 0
            write (errbuf(1), 10050) name1c, base1, name2c, base2, idc,
     &       ksect
            call prterr('E', 1)
10050       format ('NO BRANCH DATA CAN BE FOUND FOR POWER RATE RELAY '
     &       , 'CARD.', a8, f5.1, 2x, a8, f5.1, 1x, a1, 1x, i1)
          else
            gijr1(kntr1) = gkm
            bijr1(kntr1) = bkm
            gior1(kntr1) = gk1
            bior1(kntr1) = bk1
            gjor1(kntr1) = gk2
            bjor1(kntr1) = bk2
          endif
        endif
      endif
      return
      end
