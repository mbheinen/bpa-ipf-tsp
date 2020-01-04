C    @(#)deleterpt.f	20.6 7/18/96
C****************************************************************
C
C   	File: deleterpt.f
C
C   	Purpose: Generates a report of all system deletions.
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: p_report.f
C
C****************************************************************
C
      	integer function deleterpt (in_buffer, out_buffer, scrfil) 
        character in_buffer *(*), out_buffer *(*)
        integer scrfil

      	include 'ipfinc/parametr.inc'

      	include 'ipfinc/blank.inc'
      	include 'ipfinc/changr.inc'
      	include 'ipfinc/lfiles.inc'
      	include 'ipfinc/delete.inc'

	character null * 1, linefeed * 1, header(4) * 80
        integer o2, apdoutbuf

        maxbuf_out = len( out_buffer ) - 400
        null = char(0)
        linefeed = char(10)
        deleterpt = 0

        write (header(1), 100) ndelete, cspare(30), dte

  100   format (' List of system deletions - Total ', i4, ' Case ', a, 
     &     ' date ', a)
        header(2) = ' '

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        do i = 1, 2
           if (scrfil .gt. 0) write (scrfil, '(a)') header(i)
           length = apdoutbuf(o2, header(i), out_buffer(o2:))
           o2 = o2 + length
        enddo
        ic = 1
        do while (ic .le. ndelete .and. o2 .le. maxbuf_out)
           if (delete(ic)(3:3) .ne. '*') then
              if (scrfil .gt. 0) write (scrfil, '(a)') delete(ic)(1:92)
              length = apdoutbuf(o2, delete(ic)(1:92), out_buffer(o2:))
              o2 = o2 + length
           endif
           ic = ic + 1
        enddo

        if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+8), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end
