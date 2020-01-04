C    @(#)prterx.f	20.4 2/13/96
      subroutine prterx (errtyp, nbl)
 
C *** PRTERX prints the error message in a uniform format and          *
C *** maintains a count in ERRCNT of the numbers of errors at          *
C *** each severity level. It reformats the error text and calls       *
C *** the print routine for actual printing of the messages.           *
 
C *** The error message is also saved in common /ERRMSG/ERRM(400)      *
C *** for summarizing at the end of the powerflow output.              *
 
C *** ERRTYP - a character designating the severity level:             *
 
C *** I - informative                                                  *
C *** W - warning                                                      *
C *** E - error                                                        *
C *** F - fatal error                                                  *
C *** A - abort                                                        *
 
C *** NBL - the number of lines encoded in ERRBUF                      *
C ***                                                                  *
 
      include 'ipfinc/errmsg.inc'
      include 'ipfinc/errorx.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/prtdbg.inc'

      common /is_batch / is_batch   ! 0 = interactive, 1 = batch
 
      character errtyp * 1, errflg(5) * 12, crgctl * 1, capital * 1
      integer type, savcrt, svlprt, savfch
 
      data errflg /'*INFORMATIVE', '*** WARNING ',
     1             '*** ERROR   ', '*** FATAL   ', '*** ABORT   ' /
C ***                                                                  *
C *** Save original LPRT and CRT Switches and temporarily turn         *
C *** them on.                                                         *
C ***                                                                  *
      savfch = fichsw
      svlprt = lprtsw
      savcrt = crtsw
      if (.not. batch) crtsw = 1
      lprtsw = 1
      oldbsw = dbsw
      call dbgprt(1)
 
      type = index('IWEFA',capital(errtyp))
      if (type .ne. 0) then
 
         errcnt(type) = errcnt(type) + 1
 
C ***    Don't print error level 1 on CRT or LPT or FICHE              *
C ***         (defer to end of pf reports)                             *
C ***                                                                  *
         if (type .eq. 1) then
            lprtsw = 0
            crtsw = 0
            fichsw = 0
         else 
            if (batch) then
              crtsw = 1
            endif
         endif
 
      else
 
         write (outbuf,1000) capital(errtyp)
 1000    format ('0 ILLEGAL ERROR TYPE ', a, ' SET TO "I"')
         type = 1
         errcnt(type) = errcnt(type) + 1
         outbuf(122:) = errflg(type)
         call prtout (1)
         if (numerr .lt. MAXERRMSG) then
            numerr = numerr + 1
            errm(numerr) = outbuf
         endif
      endif
 
C *** Print lines of error message                                     *
 
      do 1200 i = 1, nbl
         if (i .eq. 1) then
            if (errbuf(1)(1:1) .eq. '0') then
               crgctl = '0'
               errbuf(1)(1:1) = ' '
            else
               crgctl = ' '
            endif
         else
            crgctl = ' '
         endif
         write (outbuf,1100) crgctl, errflg(type), errbuf(i)(1:106),
     1      errflg(type)
 1100    format (a1, a12, 1x, a, 1x, a12)
         call prtout (1)
C
C ***    Store only first MAXERRMSG errors
C
         if (numerr .lt. MAXERRMSG) then
            numerr = numerr + 1
            errm(numerr) = outbuf
         endif
 1200 continue
 
C     Stop if more than 100 "E" or "F" errors.
 
      if ( errcnt(3)+errcnt(4)+errcnt(5) .gt. 100  .and. 
     &     is_batch .eq. 1 ) then
         crgctl = ' '
         errbuf(1) = ' More than  100  level "E" or "F" errors.'
     &               // '  Program aborted.'
         errcnt(5) = errcnt(5) + 1
         write (outbuf,1100) crgctl, errflg(5), errbuf(1)(1:106),
     &                       errflg(5)
         call prtout(1)
         if (numerr .lt. MAXERRMSG) then
            numerr = numerr + 1
            errm(numerr) = outbuf
         endif
         call erquit
      endif
 
C *** Restore LPRT and CRT switches to original value                  *
 
      crtsw = savcrt
      lprtsw = svlprt
      fichsw = savfch
 
      call dbgprt(oldbsw)
 
      return
      end
