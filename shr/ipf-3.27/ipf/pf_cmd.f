C    @(#)pf_cmd.f	20.4 1/7/99
C****************************************************************
C
C       File: pf_cmd.f
C
C       Purpose: Routine to process interactively PF commands
C
C       Author: Walt Powell  Date: 12 November 1992
C       Called by: ipf_main 
C
C****************************************************************
C
	subroutine pf_cmd

	include 'ipfinc/parametr.inc'

	include 'ipfinc/jobctl.inc'
	include 'ipfinc/lfiles.inc'
	include 'ipfinc/prt.inc'

        character in_buffer * (MAXBUFFER)
        character out_buffer * (MAXBUFFER)
	character text * 120, null * 1, linefeed * 1 
        logical eof, eoj, endloop
	integer status, o1, o2

	character capital * 10
	integer  open_file, srv_cmdprs_cw

        call initlz(status)
        call pfinit

        null = char(0)
        linefeed = char(10)
        status = open_file( 50, 'ipf_test.log', 'F', 'W', iostat )
        if ( status .ne. 0 ) then
           write (*,11) status, iostat
   11      format(' error opening log file, status = ', i2,
     &            '     iostat = ', i4 )
           call exit
        endif
        eoj = .false.
        do while (.not. eoj)
c
c	   Type in powerflow command, terminate with (END)
c
   90      write (*,100)
  100	   format(' Enter powerflow command > ',$)
      	   read (*,110) text
  110	   format (a)
           if ( text .eq. ' ' ) then
              print *, 'use  /QUIT  or  /EXIT  to exit'
              goto 90
           endif
           if ( capital( text(2:5) ) .eq. 'QUIT'  .or.
     &          capital( text(2:5) ) .eq. 'EXIT' ) goto 900

           write(50,110) ' '
           write(50,110)
     &        '#######################################################'
           write(50,110) ' #######  in_buffer text:  #################'
           write(50,110) ' '

           in_buffer(1:1) = null
           out_buffer(1:1) = null
           i1 = 1
           eof = .false.
           endloop = .false.
           do while (.not. endloop)
              len1 = lastch(text)
              len2 = len(in_buffer)
              if (len1 + i1 .lt. len2) then
                 if (i1 .gt. 1) then
                    in_buffer(i1:i1) = linefeed
                    i1 = i1 + 1    
                 endif
                 in_buffer(i1:i1+len1) = text(1:len1) // null
                 write(50,110) text(1:len1)
                 i1 = i1 + len1
              endif
              if (eof) then
                 endloop = .true.
              else
                 write (*,100)
                 read (*,110, end=130) text
                 if ( capital( text(1:5) ) .eq. '(END)' ) then
                    eof = .true.
                    text = '*[EOM]'
                 endif
              endif
           enddo
  130      continue

           status = srv_cmdprs_cw( in_buffer, out_buffer )

           write(50,110) ' '
           write(50,110) ' #######  out_buffer text:  ################'
           write(50,110) ' '

           o1 = 1
           last = index (out_buffer, null)
           if ( last .eq. 0 ) then
              write (*, 139)
  139         format(' ***** out_buffer had no null terminator *****')
              write (*, 140) out_buffer(1:120)
  140         format(' pf_cmd (', a, ')'  )
              write (50, 139)
              write (50, 110) out_buffer(1:120)
           endif
           i = 0
           do while (o1 .lt. last)
              i = i + 1
              o2 = index (out_buffer(o1:), linefeed)
              if (o2 .eq. 0) then
                 o2 = last
              else
                 o2 = o1 + o2 - 2
              endif
              if ( o2-o1 .lt. 1 ) then
                 if ( i .lt. 16 ) write (*, 140) ' '
                 write (50, 110) ' '
              else if ( o2-o1 .lt. 120 ) then
                 if ( i .lt. 16 ) write (*, 140) out_buffer(o1:o2)
                 write (50, 110) out_buffer(o1:o2)
              else
                 if ( i .lt. 16 ) write (*, 140) out_buffer(o1:o1+120)
                 write (50, 110) out_buffer(o1:o1+120)
              endif
              o1 = o2 + 2
           enddo
        enddo
  900   continue
        call close_file(50)
        return
        end
