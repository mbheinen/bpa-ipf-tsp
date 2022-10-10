C    %W% %G%
      subroutine indrop
C     
C     THIS SUBROUTINE CHECKS ALL INDUCTION MOTORS IN THE
C     STUDY FOR LOW VOLTAGE TRIPPING.  IT IS CALLED BY'
C     RELAY AND IT CALL GENDRP.
C     
      include 'tspinc/params.inc'
      include 'tspinc/lnk12.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
      include 'tspinc/param.inc'
      include 'tspinc/prt.inc'
      dimension tblim(4), itblim(4)
      equivalence (tblim, itblim), (ltrip, itblim(1)), (timerm, tblim
     &            (2)), (voltlo, tblim(3)), (tdly, tblim(4))
      character*8 name1c
      character*1 imid
      if (keybrd(16) .ne. 0) then
        write (outbuf, 10000) imlv
10000   format (' 0 RELAY S6090 , IMLV = ', i6)
        call prtout(1)
        do jjj = 1, imlv, 10
          kkk = min0(jjj+9, imlv)
          write (outbuf, 10010) imlv, (imlvi(i), i = jjj, kkk)
10010     format (t24, 10i6)
          call prtout(1)
        enddo
      endif
      do im = 1, imlv
        imi = imlvi(im)
        igm = igentn(1, imi)
        iecs = igentn(2, imi)
        imid = igentc(imi)
        call redecs(tblim, iecs+22, 4)
        if (ltrip .ne. 2) then
          er = eyr(igm)
          ei = eyi(igm)
          vlt = sqrt(er**2+ei**2)
C         
C         RESET TIMER IF VOLTAGE IS ABOVE TRIP LEVEL
C         
          if (vlt .le. voltlo) then
            if (timerm .eq. 0.0) then
C             
C             SET UNDER VOLTAGE TRIP TIMER
C             
              name1c = bname(igm)
              bkv1 = buskv(igm)
              write (outbuf, 10020) name1c, bkv1, imid, tsim
              call prtout(1)
10020         format ('0', 5x, 'INDUCTION MOTOR LOW VOLTAGE TIMER FOR '
     &         , 5x, a8, 1x, f5.1, 1x, a1, ' SET AT ', f6.1, ' CYCLES')
              if (tdly .ne. 0.0) then
C             
C               CHECK TIMER FOR INITIATING TRIP
C             
                timerm = to + tdly
                if (timerm .lt. dnxrly) dnxrly = timerm
                goto 100
              endif
            elseif (to .lt. timerm) then
              goto 110
            endif
C           
C           INITIATE TRIP
C           
            ivpc = 2
            ltrip = 2
C           
C           SET TDLY = -2.0 TO BE USED AS CODE FOR BLOCKED ROTOR R
C           
            tdly =  - 2.0
            if (igm .lt. lfrst) lfrst = igm
            name1c = bname(igm)
            bkv1 = buskv(igm)
            write (outbuf, 10030) name1c, bkv1, imid, tsim
            call prtout(1)
10030       format ('0', 5x, 'INDUCTION MTR DROP DUE TO LOW VOLTS FOR',
     &       5x, a8, 1x, f5.1, 1x, a1, ' AT ', f6.1, ' CYCLES')
            gndrop = 0.0
            call gendrop(imi, gndrop, igm, iecs, imid)
          elseif (timerm .ne. 0.0) then
            timerm = 0.0
            name1c = bname(igm)
            bkv1 = buskv(igm)
            write (outbuf, 10040) name1c, bkv1, imid, tsim
            call prtout(1)
10040       format ('0', 5x, 'INDUCTION MOTOR LOW VOLTAGE TIMER FOR ',
     &       5x, a8, 1x, f5.1, 1x, a1, ' RESET AT ', f6.1, ' CYCLES')
          endif
  100     call ritecs(tblim, iecs+22, 4)
        endif
  110   continue
      enddo
      if (keybrd(16) .ne. 0) then
        write (outbuf, 10050)
10050   format ('0 **** INDUCTION MTR TRIP DEBUG RELAY S7050 ')
        call prtout(1)
        write (outbuf, 10060) igm, ltrip, timerm, er, ei, vlt
10060   format (2i10, 4e18.6)
        call prtout(1)
      endif
      return
      end
