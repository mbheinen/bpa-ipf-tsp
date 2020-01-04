C    @(#)chglisrpt.f	20.7 5/27/98
C****************************************************************
C
C   	File: chglilsrpt.f
C
C   	Purpose: Generates a report of all changes and comment records 
c                in /oldchg/
C
C                This subroutine is re-entrant to complete list
C                of change records. Array lastloop() flags last valid
C                encoded entity from previous call. Rentry is
C                invoked with the IPF command
C
C                /REPORTS SELECT CHANGES CONTINUE
C
C                Premature termination occurs whenever the encoded
C                output fills out_buffer. The last record is flagged
C                with the terminator
C
C                *[MORE]
C
C                It is the GUI's task to utilize this information
C                to request continuation calls to retrieve the entire
C                data set for the specified filter.
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: p_report.f
C
C****************************************************************
C
      	integer function chglisrpt (in_buffer, out_buffer, scrfil) 
        character in_buffer *(*), out_buffer *(*)
        integer scrfil

      	include 'ipfinc/parametr.inc'

      	include 'ipfinc/blank.inc'
      	include 'ipfinc/changr.inc'
      	include 'ipfinc/lfiles.inc'
      	include 'ipfinc/oldchg.inc'

	character null * 1, linefeed * 1, header(4) * 80
        integer o2, apdoutbuf, loop, lastloop, findstr, ptr, o1, count

        save 

        maxbuf_out = len( out_buffer ) - 400
        null = char(0)
        linefeed = char(10)
        chglisrpt = 0

        last = index (in_buffer, null)
C
C       Check for re-entry and continue
c
        if (findstr (in_buffer(1:last), 'CONTINUE') .ne. 0) then
           lastloop = loop
        else
           lastloop = 0
        endif

        loop = 0

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)

        if (lastloop .eq. 0) then
           write (header(1), 100) numold, cspare(30), dte
  100      format (' List of system changes - Total ', i4, ' Case ', a, 
     &        ' date ', a)
           header(2) = ' '

           do i = 1, 2
              if (scrfil .gt. 0) write (scrfil, '(a)') header(i)
              length = apdoutbuf(o2, header(i), out_buffer(o2:))
              o2 = o2 + length
           enddo
        endif

        ic = lastloop
        if (ic .eq. 0) ic = 1
        loop = ic
        do while (ic .le. numold .and. o2 .le. maxbuf_out)
           if (oldchg(ic)(121:121) .ne. 'U' .and.
     &         oldchg(ic)(121:121) .ne. 'P') then
              if (scrfil .gt. 0) write (scrfil, '(a)') oldchg(ic)(1:120)
              length = apdoutbuf(o2, oldchg(ic)(1:120), out_buffer(o2:))
              if (length .eq. 0) go to 800
              o2 = o2 + length
              if (o2 .lt. maxbuf_out) loop = ic
           endif
           ic = ic + 1
        enddo

c*** remember maxbuf_out is really 400 less than the real buffer size
  800   if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+8), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end
