C    @(#)p_plot.f	20.5 1/7/99
C****************************************************************
C
C   File: p_plot.f
C   Purpose: IPF shell program to process /PLOTS commands
C
C   Author:  Dave Stefonek Date: 20 February 1992
C   Modified: Jay Coleman   6 AUG 93
C
C   Called by:
C
C****************************************************************
C
	integer function p_plot (in_buffer, out_buffer)
        character in_buffer*(*), out_buffer*(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/errorsw.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'
        include 'ipfinc/pasprm.inc'

        character plt_buffer*(MAXBUFFER)
        character null*1, linefeed*1, capital*40, temp_buf*40,
     1            text*80, word(100)*16, stext*50, basecase_dir*60,
     2            pfmstrfnd *15, tempname*60
        character multfil(121)*80    !  one .gt. multmax
        character mstrchk*6          !  used to check for master coord file
        character newnam*80, tnam1*80, tnam2*80
        character longfil*80 
        integer apdoutbuf, o2, psplot, fndfil
        integer open_file, status
        logical exst, file_exists

        max_buf = len( out_buffer ) - 60
        p_plot = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        multmax = 120     ! Max allowable coord files
        multplt = 0      ! Number of coord files to be processed 
        multprc = 0      ! Number of coord files processed 
        null = char(0)
        linefeed = char(10)
        pfmstrfnd = 'pfmaster.post'// null
        out_buffer(1:1) = null
        i1 = index (in_buffer, linefeed) + 1
        i2 = index (in_buffer, null)
        plt_buffer(1:i2) = in_buffer(1:i2)


        num_word = 0
c
c	Get file name from in_buffer
c
        do while (i1 .lt. i2)
           next = nxt_term(in_buffer(i1+1:)) + i1
           text = in_buffer(i1:next-1)
           lentxt = next - i1
******************** debug stuff **********************************
*          print 777, i1, next-1, text
* 777	   format (' p_plot.f : in_buffer = ', 2i4, 1x, a)
******************** debug stuff **********************************
*** don't use characters that are part of VMS, UNIX, or DOS file names
***        call uscan (text, word(num_word+1), numwrd,'=', ',/ ')
           call uscan (text, word(num_word+1), numwrd,'=', ', ')
           num_word = num_word + numwrd
           i1 = next
           if (in_buffer(i1:i1) .eq. linefeed) i1 = i2 + 1
        enddo
*       do i = 1, num_word
*          word(i) = capital(word(i))
*       enddo

*       temp_buf = in_buffer
*       next = index(temp_buf, LINEFEED)
*       if (next .eq. 0) next = index (temp_buf, NULL)
*       if (next .gt. 0) temp_buf(next:) = ' '
*       temp_buf = capital (temp_buf)

        if (ostates .le. 4) then
           p_plot = 1
           write (errbuf(1), 144)
  144      format(' Unsolved base data in residence')
           call prterx ('W', 1)

****** this return can be removed when drawing a diagram w/o pfdata is ok
*          go to 900 
****** this return can be removed when drawing a diagram w/o pfdata is ok
 
        endif


****** inquire to determine if a pfmaster.post file is available 
********************* debug stuff *****************************
*     print 9921, kerr, pfmstrfnd, postmstr 
*9921 format(' kerr, pfmstrfnd, postmstr ' i5,' :', a,':',a,':')
********************* debug stuff *****************************
       kerr = fndfil(pfmstrfnd, postmstr)
c**************************************************************
c  make sure the string is blank filled - JGC
       i1 = index( postmstr, null )
       if ( i1 .gt. 0 ) postmstr(i1:) = ' '
c**************************************************************
       inquire(file=postmstr, exist=exst)
       if (.not. exst) then
           write (errbuf(1), 151) postmstr
  151      format(' Missing pfmaster.post file   "', a, '"' )
           call prterx ('W', 1)
           go to 900 
       endif

********* begin - look for multiple coordinate files to be plotted
      lcord = 24           ! lcord is a scratch file here
      close (unit = lcord, err = 170)
  170 continue
      longfil = text(1:lentxt)
      call opnfila(lcord, longfil, status)
      if (status .ne. 0) then
         write (errbuf(1), 181) text
  181    format(' Missing Coordinate file:', a)
         call prterx ('W',1)
         go to 900 
      endif
      read(lcord, 191) multfil(multplt+1)  
  191 format (a)
      mstrchk = capital (multfil(multplt+1)(1:6))
      if (mstrchk .eq. 'MASTER') then
  200     read(lcord, 191, end = 220) multfil(multplt+1)  
          if ( multplt .lt. multmax) then 
             if (multfil(multplt+1)(1:1) .ne. '!' .and.
     1      multfil(multplt+1)(1:1) .ne. '#') multplt = multplt + 1
          else
             write (errbuf(1), 211)     
  211        format(' Coordinate File Table Overflow - Yell HELP!')
             call prterx ('W',1)
          endif
          go to 200
  220     multprc = multprc + 1
          text = multfil(multprc) 
  
      endif
      close (unit = lcord, err = 250)
********* end   - look for multiple coordinate files to be plotted

****** inquire to determine if coordinate file exists  
  250  newnam = text
************************** debug stuff *********************
*     print 9931, multprc, multplt, newnam
*9931 format(' validate multiplot files, multprc, multplt ',
*    1 'newnam', 2i4,/1x, a)
************************** debug stuff *********************

********** special logic for searching paths on BPA VMS systems*********
********** 'newnam' will be the full path name *************************

           if ( is_it_vms() .eq. 1 ) then
              inquire ( file=newnam, exist=file_exists )
              if ( .not. file_exists ) then
                 i = lastch( newnam )
                 write(*,261) newnam(1:i), 
     &             'not found, searching  BASECASE_DIR'
  261            format (' File  "', a, '"  ', a )
                 tnam2 = newnam
                 i = index (tnam2, ':')
                 tnam1 = tnam2(i+1:)
                 tnam2 = tnam1
                 i = index (tnam2, ']')
                 tnam1 = tnam2(i+1:)
                 tnam2 = tnam1
                 newnam = 'basecase_dir:' // tnam2
                 inquire ( file=newnam, exist=file_exists )
              endif
              if ( .not. file_exists ) then
                 il = lastch( tnam2 )
                 write(*,261) tnam2(1:il),
     &             'not found in basecase_dir, searching  IPF_TDAT'
                 newnam = 'ipf_tdat:' // tnam2
                 inquire ( file=newnam, exist=file_exists )
              endif

              if ( .not. file_exists ) then
                 il = lastch( tnam2 )
                 write(*,261) tnam2(1:il), 
     &             'not found in ipf_tdat, searching  WSCCEOFC_DIR'
                 newnam = 'wscceofc_dir:' // tnam2
                 inquire ( file=newnam, exist=file_exists )
              endif
              if ( .not. file_exists ) then
                 write(*,261) tnam2(1:il), 
     &             'not found in wscceofc_dir, searching  WSCCBASE_DIR'
                 newnam = 'wsccbase_dir:' // tnam2
*                inquire ( file=newnam, exist=file_exists )
              endif
           else
              inquire ( file=newnam, exist=file_exists )
              if ( .not. file_exists ) then
                 basecase_dir = ' '
                 tempname = 'BASECASE_DIR' // char(0)
                 call getenvir (tempname, basecase_dir)
                 if (basecase_dir .ne. ' ') then
                    tnam2 = newnam
                    do while (index (tnam2, '/') .ne. 0) 
                       last1 = index (tnam2, '/')
                       tnam1 = tnam2(last1+1:)
                       tnam2 = tnam1
                    enddo
                    last1 = lastch( tnam2 )
                    last2 = lastch (basecase_dir)
                    write(*, 261) tnam2(1:last1), 
     &                 'not found, searching BASECASE_DIR'
                    newnam = basecase_dir(1:last2) // tnam2(1:last1)
                    inquire ( file=newnam, exist=file_exists )
                 endif
              endif
           endif
********** special logic for searching paths on BPA VMS systems*********

************** debug stuff *************************    
*     print 9941, newnam
*9941 format(' after alteration newnam= :',a,':')
************** debug stuff *************************    

       inquire(file=newnam, exist=file_exists)
       if (.not. file_exists) then
           write (errbuf(1), 181) text
           call prterx ('W', 1)
*******  check for more plots when plotting from a master*********
           if (multprc .lt. multplt) then
              multprc = multprc + 1
              text = multfil(multprc)
              in_buffer(1:i2) = plt_buffer(1:i2)
              go to 250
           endif

           go to 900 
       endif

       call prtime('POSTSCRIPT PLOT')
       inrcd = buf
 5555 continue

************************** debug stuff *********************
*     print 9901, multprc, multplt
*9901 format(' calling psplot, multprc, multplt', 2i4)
*     print 9903, plt_buffer(1:i2-1)
*9903 format (1x,a)
************************** debug stuff *********************
        p_plot = psplot(newnam, in_buffer)
************************** debug stuff *********************
*     print 9911
*9911 format(' returned from psplot')
************************** debug stuff *********************
******* begin - check for more plots *********
      if (multprc .lt. multplt) then 
         multprc = multprc + 1
         text = multfil(multprc)
         in_buffer(1:i2) = plt_buffer(1:i2)
         go to 250
      endif

******* end   - check for more plots *********

c       Append error messages to buffer
c
  900   continue
        j = 1
        length = 1
        o2 = index (out_buffer,null)
        do while (j .le. numerr .and. length .gt. 0)
           length = apdoutbuf(o2, errm(j), out_buffer(o2:))
           o2 = o2 + length
           j = j + 1
        enddo
c
c 	Append summary
c
        if ( o2 .gt. max_buf ) then
           o2 = max_buf
           do while ( o2 .gt. 1  .and.
     &                out_buffer(o2:o2) .ne. linefeed )
              o2 = o2 - 1
           enddo
           out_buffer(o2:o2) = null
        endif
        write (stext, 340) 'p_plot.f', p_plot, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, stext, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end
