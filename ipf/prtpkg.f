C    @(#)prtpkg.f	20.3 2/13/96
      subroutine prtpkg
 
C *** This is a standardized print package for Powerflow               *
C *** and Transient Stability Programs. This routine will print on the *
C *** crt, lineprinter and/or fiche units as requested using the same  *
C *** output buffer.                                                   *
 
C *** This package contains the following entries:                     *
 
C *** 1.      PRTOUT(NUM)     Print the detail line on all             *
C ***                         selected devices                         *
 
C *** 1a.     PFOMF(NUM)      Print the detail line on the             *
C ***                         FICHE unit only for control of FICHE.    *
C ***                         line not counted!                        *
 
C *** 2.      HEDLOD          Load header message buffer               *
 
C *** 3.      RPNLOD          Load report name buffer                  *
 
C *** 4.      SHDLOD(NUM)     Load SUB_HEADING(NUM), 1 <= NUM <= 5     *
 
C *** 5.      COMLOD(NUM)     Load COMMENT(NUM), 1 <=  NUM <= 2        *
 
C *** 6.      SHDPRT(NUM)     Print SUB_HEADING                        *
 
C *** 7.      FORBTM          Force bottom footer for both LPRT and    *
C ***                         FICHE                                    *
 
C *** 8.      FORTOP          Force top header for both LPRT and FICHE.*
C ***                         Any coments and subheadings will         *
C ***                         be printed automatically.                *
 
C *** 9.      NEWPG           Jump to new page.  Print footer and      *
C ***                         header on LPRT and FICHE                 *
 
C *** 10.     SKIPLN(NUM)     Skip "NUM" blank lines on both LPRT      *
C ***                         and FICHE                                *
 
C *** 11.     SPACE(NUM)      Skip "NUM" blank lines on both LPRT      *
C ***                         and FICHE (same function as SKIPLN)      *
 
C *** 12.      DBGPRT(NUM)    Turns ON/OFF printing of all records     *
C ***                         for the DEBUG file.                      *
C ***                         (NUM = 0 (OFF) / 1 (ON))                 *
 
C *** 13.      CHKBTM(NUM)    Force a new page if less than NUM lines  *
C ***                         remain                                   *
 
C *** HEADER is the header and footer line. It will appear             *
C ***        on all pages of the report.                               *
 
C *** COMENT is two lines of user-supplied commentory, and             *
C ***        will appear below the header on all pages of              *
C ***        the report.                                               *
 
C *** REPNAM is the name of the section of the main report,            *
C ***        and will be printed on the same line with header          *
C ***        at the head and foot of each page.                        *
 
C *** SUBHED is 5 lines of subheader information provided by           *
C ***        the calling module.  It will be printed when              *
C ***        requested by the calling module and below the             *
C ***        header of each new page.                                  *
 
 
      include 'ipfinc/header.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/pageno.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/topbot.inc'
 
      common /prtdbg/ dbsw
      integer dbsw
      save
 
      character cch * 1, ccl * 1, ccf * 1
      integer first
 
C ***                                                                  *
 
      entry dbgprt(num)
      dbsw = num
      return
 
C ***                                                                  *
 
      entry prtout(num)
 
      cch = outbuf(1:1)
      ccl = cch
      ccf = cch
 
C *** Write on DEBUG file if switch is on                              *
 
      if ( dbsw .gt. 0) write ( dbug,1000) outbuf
 
      if (lstdnl .eq. 0) then
         ccl = '1'
      endif
 
      if (lstdnf .eq. 0) then
         ccf = '1'
      endif
 
      if ( crtsw .gt. 0 ) then
         outbuf(1:1) = ' '
         write (*,1000) outbuf
         outbuf(1:1) = cch
      endif
 
      if ( ccl .eq. '0' ) then
         if ( lprtsw .gt. 0 ) lineno = lineno + 1
 
      else if( ccl .eq. '1' ) then
 
C *** Print at top of next page                                        *
 
         if (lprtsw .gt. 0) then
            if (lstdnl .ne. 0) then
               call prtbtm(lprt,pageno,lineno,maxlin)
            endif
 
            call prttop(lprt,pageno,lineno,maxlin)
            ccl = ' '
            lstdnl = 1
         endif
 
      else if ( ccl .eq. '+' ) then
         if ( lprtsw .gt. 0 ) lineno = lineno - 1
      endif
 
      if ( ccf .eq. '0' ) then
         if ( fichsw .gt. 0 ) fichln = fichln + 1
 
      else if( ccf .eq. '1' ) then
         if (fichsw .gt. 0) then
            if (lstdnf .ne. 0) then
               call prtbtm(mfich,fichpg,fichln,fichmx)
            endif
 
            call prttop(mfich,fichpg,fichln,fichmx)
            lstdnf = 1
            ccf = ' '
         endif
 
      else if ( ccf .eq. '+' ) then
         if ( fichsw .gt. 0 ) fichln = fichln - 1
      endif
 
C *** Print only if not too close to page foot                         *
 
      if (lstdnl .ne. 0) then
         if ( lprtsw .gt. 0 ) then
            if (lineno .ge. (maxlin-2)) then
               call prtbtm(lprt,pageno,lineno,maxlin)
               call prttop(lprt,pageno,lineno,maxlin)
               ccl = ' '
            endif
 
            outbuf(1:1) = ccl
            write (lprt,1000) outbuf
 1000       format(a)
            lineno = lineno + 1
         endif
 
      endif
 
      if (lstdnf .ne. 0) then
         if ( fichsw .gt. 0 ) then
 
            if (fichln .ge. (fichmx-2)) then
               call prtbtm(mfich,fichpg,fichln,fichmx)
               call prttop(mfich,fichpg,fichln,fichmx)
               ccf = ' '
            endif
 
            outbuf(1:1) = ccf
            write (mfich,1000) outbuf
            fichln = fichln + 1
         endif
      endif
 
      return
 
C *** Print special microfiche control on FICHE file only              *
 
      entry pfomf(num)
      write (mfich,1000) outbuf
      return
 
C ***                                                                  *
 
      entry hedlod
 
C *** Load header message with new text....                            *
 
      header(1:50) = outbuf(1:50)
      return
 
C ***                                                                  *
 
      entry rpnlod
 
C *** Load report name with new text...                                *
 
      repnam(1:60) = outbuf(1:60)
      return
 
C ***                                                                  *
 
      entry shdlod(num)
 
C *** Load SUBHEAD(I) with new text....                                *
 
      if (num .gt. 0.and.num .lt. 6) then
         if (index('0 #',outbuf(1:1)) .eq. 0 ) outbuf(1:1) = ' '
         subhed(num) = outbuf
C ***                                                                  *
      else
         write (*,1100) num
 1100    format(' CALLING ERROR TO PRTOUT : ILLEGAL VALUE OF SUBHEADER I
     1NDEX (',i4,') MUST BE BETWEEN 0 TO 6.')
      endif
 
      return
 
C ***                                                                  *
 
      entry comlod(num)
C ***                                                                  *
C *** Load COMENT(NUM) with new text....                               *
C ***                                                                  *
      if (num .eq. 1) then
         coment(1) = ' '
         coment(2) = ' '
C ***                                                                  *
      else if (coment(1) .eq. ' ') then
         coment(1) = outbuf
C ***                                                                  *
      else if (coment(2) .eq. ' ') then
         coment(2) = outbuf
      endif
C ***                                                                  *
      return
C ***                                                                  *
      entry forbtm
 
C *** Force bottom on both LPT and FICH units.                         *
 
      if (lstdnl .ne. 0) then
         if (lprtsw .gt. 0) call prtbtm(lprt,pageno,lineno,maxlin)
         lstdnl = 0
      endif
 
      if (lstdnf .ne. 0) then
         if (fichsw .gt. 0) call prtbtm(mfich,fichpg,fichln,fichmx)
         lstdnf = 0
      endif
C ***                                                                  *
      return
C ***                                                                  *
      entry fortop
 
C *** Force top on both LPT and FICH units.                            *
 
      if (lstdnl .eq. 0) then
         if (lprtsw .gt. 0) call prttop(lprt,pageno,lineno,maxlin)
         lstdnl = 1
      else
         if (lprtsw .gt. 0) call prtbtm(lprt,pageno,lineno,maxlin)
      endif
 
      if (lstdnf .eq. 0) then
         if (fichsw .gt. 0) call prttop(mfich,fichpg,fichln,fichmx)
         lstdnf = 1
      else
         if (fichsw .gt. 0) call prtbtm(mfich,fichpg,fichln,fichmx)
      endif
 
      return
C ***                                                                  *
      entry skipln(num)
      entry space(num)
 
C *** Skip "NUM" blank lines or to bottom of page                      *
 
      outbuf = ' '
 
      if ( crtsw .gt. 0 ) then
         do 1300 i = 1, num
         write (*,1200)
 1200    format(1x)
 1300    continue
      endif
C ***                                                                  *
      if ( lprtsw .gt. 0 ) then
         first = lineno
         jlast = maxlin - 3
         last = first + num
         if ( last .gt. jlast ) last = jlast
         do 1400 i = first + 1, last
         write (lprt,1200)
         lineno = lineno + 1
 1400    continue
      endif
C ***                                                                  *
      if ( fichsw .gt. 0 ) then
         first = fichln
         jlast = fichmx - 3
         last = first + num
         if ( last .gt. jlast ) last = jlast
         do 1500 i = first + 1, last
         write (mfich,1200)
         fichln = fichln + 1
 1500    continue
      endif
C ***                                                                  *
      return
C ***                                                                  *
      entry shdprt
C ***                                                                  *
C *** Subheader print at the current line on the page.                 *
C *** If you're too close to the foot, start a new page anyway         *
C ***                                                                  *
      if (crtsw .gt. 0) then
         do 1600 i = 1, 5
         if (subhed(i) .ne. ' ') write (*,1000) subhed(i)
 1600    continue
      endif
C ***                                                                  *
      if (lstdnl .eq. 0) then
         call prttop(lprt,pageno,lineno,maxlin)
         lstdnl = 1
      endif
 
      if (lstdnf .eq. 0) then
         call prttop(mfich,fichpg,fichln,fichmx)
         lstdnf = 1
      endif
C ***                                                                  *
      if ( lprtsw .gt. 0 ) then
         j = maxlin - 9
         if (lineno .ge. j) then
            call prtbtm(lprt,pageno,lineno,maxlin)
            call prttop(lprt,pageno,lineno,maxlin)
 
C *** Otherwise write the subheader                                    *
 
         endif
C ***                                                                  *
         do 1700 i = 1, 5
         if (subhed(i) .ne. ' ') then
            if (subhed(i)(1:1) .eq. '0') then
               lineno = lineno + 1
*----------------------------------------------------------------------*
*            ELSE
*               SUBHED(I)(1:1)= ' '
*----------------------------------------------------------------------*
            endif
 
            write (lprt,1000) subhed(i)
            lineno = lineno + 1
         endif
 
 1700    continue
      endif
C ***                                                                  *
      if ( fichsw .gt. 0 ) then
         j = fichmx - 9
 
         if ( fichln .ge. j) then
            call prtbtm(mfich,fichpg,fichln,fichmx)
            call prttop(mfich,fichpg,fichln,fichmx)
 
C *** Otherwise write the subheader                                    *
 
         endif
 
         do 1800 i = 1, 5
         if (subhed(i) .ne. ' ') then
            if (subhed(i)(1:1) .eq.'0') then
               fichln = fichln + 1
            else
               subhed(i)(1:1)= ' '
            endif
            write (mfich,1000) subhed(i)
            fichln = fichln + 1
         endif
 1800    continue
C ***                                                                  *
      endif
C ***                                                                  *
      return
 
C ***                                                                  *
 
      entry newpg
 
C *** Finish old page and start a new page                             *
 
      if (lprtsw .gt. 0) then
         if (lstdnl .ne. 0) then
            call prtbtm(lprt,pageno,lineno,maxlin)
         endif
         call prttop(lprt,pageno,lineno,maxlin)
         lstdnl = 1
      endif
C ***                                                                  *
      if (fichsw .gt. 0) then
         if (lstdnf .ne. 0) then
            call prtbtm(mfich,fichpg,fichln,fichmx)
         endif
         call prttop(mfich,fichpg,fichln,fichmx)
         lstdnf = 1
      endif
C ***                                                                  *
      return
 
C ***                                                                  *
 
      entry chkbtm(num)
 
C *** Force new page/frame if not more than NUM lines remain           *
 
      if ( lineno .ge. (maxlin - num) ) then
         if (lprtsw .gt. 0) then
            if (lstdnl .ne. 0) then
               call prtbtm(lprt,pageno,lineno,maxlin)
            endif
            call prttop(lprt,pageno,lineno,maxlin)
            lstdnl = 1
         endif
      endif
 
      if ( fichln .ge. (fichmx - num) ) then
         if (fichsw .gt. 0) then
            if (lstdnf .ne. 0) then
               call prtbtm(mfich,fichpg,fichln,fichmx)
            endif
            call prttop(mfich,fichpg,fichln,fichmx)
            lstdnf = 1
         endif
      endif
 
      return
C ***                                                                  *
      end
