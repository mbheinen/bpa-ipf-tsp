C    @(#)p_loaddef.f	20.5 8/20/98
C****************************************************************
C
C       File: p_loaddef.f
C       Purpose: IPF shell program to process
C                                                                      
C       This subroutine processes the following commands
c       
c         / GET_DATA, TYPE = LOAD_DEFINE
C         > DEFINE ...
C         C ...
C
C       Author: Walt Powell  Date: 21 July 1992 
C                            Modified: 21 July 1992
C       Called by: 
C
C****************************************************************
C
	integer function p_loaddef (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/blank.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/usranl.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/errmsg.inc'

        character null*1, linefeed*1, text*120
        integer first, last, apdoutbuf, o2
        logical opened

C       LUN is logical unit number of auxiliary output.                  
C       NUMREC is count of records written to auxiliary output file.     
C                                                                      
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null

        p_loaddef = 0  ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
c
c       Position in_buffer to next record
c
        first = 1
        last = index (in_buffer, null)
        if (last .eq. 0) last = len (in_buffer)

        numusr = 1
        numrcd = 0

        lunusr = 25
        inquire (unit=lunusr, opened=opened)
        if (.not. opened) then
           write (errbuf(1), 104) lunusr
  104      format (' /USER_DEF scratch file is not opened on logical uni
     &t', i3)
           call prterx ('W',1)
           p_loaddef = 1
           go to 330
        endif
 
        do while (first .lt. last)
           next = nxt_term(in_buffer(first+1:)) + first
           if (first .lt. next) then
              text = in_buffer(first:next-1)
C
C             Skip "." comment text.
C
              if (text(1:1) .eq. '.') then
              else if (index ('> ', text(1:1)) .gt. 0) then
                 numrcd = numrcd + 1
 
                 if (numdef(1) .gt. 500) then
                    write (errbuf(1),10237) 500
10237               format ('More than ',i3, ' > DEFINE records.')
                    call prterx ('E', 1)
                    p_loaddef = 1
                 else
                    numdef(1) = numdef(1) + 1
                    usrdef(numdef(1)) = text
                 endif
              else if (index ('HSC', text(1:1)) .ne. 0) then
                 if (numtxt(1) .gt. 500) then
                    write (errbuf(1),10238) 500
10238               format ('More than ',i3, 'C comment records.')
                    call prterx ('E', 1)
                    p_loaddef = 1
                 else
                    numtxt(1) = numtxt(1) + 1
                    usrtxt(numtxt(1)) = text
                 endif
              endif
           endif
           first = next + 1
        enddo

        rewind lunusr

        ndef = numdef(1)
        do first  = 1, ndef, 200
           last = min0 (first+199,ndef)
           write (lunusr) (usrdef(i),i=first,last)
        enddo
        ntex = numtxt(1)
        do first = 1, ntex, 200
           last = min0 (first+199,ntex)
           write (lunusr) (usrtxt(i),i=first,last)
        enddo

        o2 = index (out_buffer,null)
c
c       Append error messages to buffer
c
  330   j = 1 
        length = 1
        do while (j .le. numerr .and. length .gt. 0)
           length = apdoutbuf(o2, errm(j), out_buffer(o2:))
           o2 = o2 + length
           j = j + 1
        enddo
c
c 	Append summary
c
        write (text, 340) 'p_loaddef.f', p_loaddef, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
  900   call setercnt (0, ' ')
        return
	end
