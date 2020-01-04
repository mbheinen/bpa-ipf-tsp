C    @(#)proend.f	20.3 2/13/96
      subroutine proend
C ***                                                                  *
C *** CASE-END                                                         *
C ***   (1)                                                            *
 
      include 'ipfinc/blank.inc'
      include 'ipfinc/errmsg.inc'
      include 'ipfinc/errorx.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/timmsg.inc'
 
      dimension word(40)
      character word * 30, capital *30
 
      nocase = nocase + 1
      if ( nobus .lt. ntot ) nobus = ntot
C ***                                                                  *
      lprtsv = lprtsw
      lcrtsw = crtsw
      lfchsv =fichsw
      lprtsw = 1
      crtsw = 1
      if (kspare(16).ge.0) fichsw = 1
C ***                                                                  *
      call forbtm
      write (outbuf,100)
  100 format ('**** CASE PROCESSING SUMMARY ****')
      call rpnlod
      call fortop
C ***                                                                  *
      call space(2)
      write (outbuf,110) errcnt(1)
  110 format (' THERE WERE', i4, ' TYPE "I" (INFORMATIONAL) ERRORS')
      call prtout(1)
C ***                                                                  *
      write (outbuf,120) errcnt(2)
  120 format (11x, i4, ' TYPE "W" (WARNING) ERRORS')
      call prtout(1)
C ***                                                                  *
      write (outbuf,130) errcnt(3)
  130 format (11x, i4, ' TYPE "E" ERROR')
      call prtout(1)
C ***                                                                  *
      write (outbuf,140) errcnt(4)
  140 format (11x, i4, ' TYPE "F" (FATAL) ERRORS')
      call prtout(1)
 
      write (outbuf,150) errcnt(5)
  150 format (8x, 'AND', i4, ' TYPE "A" (ABORT) ERRORS')
      call prtout(1)
      call space(2)
C ***                                                                  *
      crtsw = 0
C ***                                                                  *
      if (numerr .ne. 0) then
         errcnt(1) = 0
         errcnt(2) = 0
         errcnt(3) = 0
         errcnt(4) = 0
         errcnt(5) = 0
C ***                                                                  *
         write (outbuf,160)
  160    format ('0  * * * ERROR MESSAGES ENCOUNTERED * * *')
         call prtout(1)
         call space(2)
C ***                                                                  *
         do 170 i = 1, numerr
            outbuf = errm(i)
            call prtout(1)
            errm(i) = ' '
  170    continue
C ***                                                                  *
         call forbtm
         call fortop
         numerr = 0
C ***                                                                  *
      else
         write (outbuf,180)
  180    format ('0  * * * NO ERROR MESSAGES ENCOUNTERED * * *')
         call prtout(1)
         call space(2)
      endif
C ***                                                                  *
      write (outbuf,190)
  190 format ('0 * * * CASE TIMING SUMMARY * * * *')
      call shdlod(1)
      outbuf= '0'
      call shdlod(2)
      outbuf = '0         MODULE NAME       MODULE CPU TIME        ' //
     & 'TOTAL CPU TIME       TIME OF DAY'
c*** & 'TOTAL CPU TIME       TIME OF DAY          PAGE FAULTS'
      call shdlod(3)
      outbuf = '          ------ ----       ------ --- ----        ' //
     & '----- --- ----       ---- -- ---'
c*** & '----- --- ----       ---- -- ---          ---- ------'
      call shdlod(4)
      outbuf = ' '
      call shdlod(5)
      call shdprt
 
      do 200 i = 1, msgnum
         outbuf = tmsg(i)
         call prtout(1)
  200 continue
      msgnum = 0
C ***                                                                  *
      lprtsw = lprtsv
      crtsw = lcrtsw
      fichsw = lfchsv
      inrcd = buf
C ***                                                                  *
      if ( .not. endjob ) then
C ***                                                                  *
  210    outbuf = ' ' / /inrcd
         ityp = index('(H',inrcd(1:1))
C ***                                                                  *
         if (ityp .eq. 1) then
            call scan(inrcd,word,nwrd)
            jobreq(1) = capital ( word(1) )
         else
            call prtout(1)
         endif
C ***                                                                  *
      endif
C ***                                                                  *
      if ( jobreq(1) .eq. 'STOP' .or. jobreq(1) .eq. 'END') then
         endjob = .true.
      endif
C ***                                                                  *
      call forbtm
      if ( .not. endjob ) then
 
C ***    FICHE_BREAK                                                   *
 
         outbuf = '$MFFB '
         call pfomf(2)
 
C ***    CLEAR SUBHEADINGS ETC.                                        *
 
         outbuf = ' '
         call shdlod(1)
         call shdlod(2)
         call shdlod(3)
         call shdlod(4)
         call rpnlod
         call comlod(1)
         call fortop
      endif
 
      return
      end
