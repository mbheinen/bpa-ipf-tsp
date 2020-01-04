C    @(#)p_ptdata.f	20.6 1/7/99
C****************************************************************
C
C   File: p_ptdata.f
C   Purpose: IPF shell program to process /GET_DATA commands
C
C   Author: Walt Powell  Date: 20 February 1992 
C                        Modified: 20 February 1992
C   Called by: 
C
C****************************************************************
C
	integer function p_ptdata (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/delete.inc'
        include 'ipfinc/epridc.inc'
        include 'ipfinc/errorsw.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'
        include 'ipfinc/alt_case.inc'

        character null * 1, linefeed * 1, text * 120, word(100) * 30,
     &            capital * 30
        integer o1, o2, apdoutbuf, findstr, bufsize

        null = char(0)
        linefeed = char(10)
        bufsize = len (out_buffer)

        p_ptdata = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        out_buffer(1:1) = null
        o2 = 1

c*************** Now done by command parser ****************************
c***c
c***c       Replace "*[EOM]" with "(END)"
c***c
c***        last = index (in_buffer, null)
c***        ix = findstr (in_buffer(1:last), '*[EOM]')
c***        if (ix .gt. 0) then
c***           in_buffer(ix:ix+5) = '(END)' // null
c***        endif
c***
c**********************************************************************

        next = nxt_term(in_buffer) 
        inrcd = in_buffer(1:next-1)
        call uscan(in_buffer(2:next-1), word, nwrd, '=',' ,'//linefeed)
c
c       Capitalize all word() except <filename>
c
c        iflag = 0
c        do i = 1, nwrd
c           if (iflag .ne. 1 .and. word(i) .ne. '=') then
c              word(i) = capital(word(i))
c           endif
c           if (iflag .eq. 0 .and. word(i) .eq. 'FILE') then
c              iflag = 1
c           else if (iflag .eq. 1 .and. word(i) .ne. '=') then
c              iflag = 2
c           endif
c        enddo
        i = 1
        do while ( i .le. nwrd )
           if ( word(i) .ne. '=' ) word(i) = capital(word(i))
           if ( word(i) .eq. 'FILE' ) i = i + 2
           i = i + 1
        end do
c
c       Reset error flag
c
        call setercnt (0, ' ')
        miscell = 0

        if (word(2) .eq. 'TYPE' .and.
     1      word(4) .eq. 'COMMENTS') then

c          set "itext" to beginning of line after "COMMENTS" keyword
           i = findstr (in_buffer, 'COMMENTS')
           itext = i + index( in_buffer(i+1:), linefeed ) + 1

           call setercnt (0, ' ')
           call ptcommnt (in_buffer(itext:), out_buffer)
        endif
c
c 	Append summary
c
        write (text, 340) 'p_ptdata.f', p_ptdata, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
c
c       Debug printout invoked with IPF command /TRACE, CHANGE = ON
c
        if (kase1(27) .ne. 0) then
           o1 = 1
           do while (o1 .lt. bufsize .and. 
     &              (out_buffer(o1:o1) .ne. null))
              next = nxt_term (out_buffer(o1+1:)) + o1
              o2 = min (o1+131, next-1)
              write (dbug, 830) out_buffer(o1:o2)             
  830         format (1x, a)
              o1 = next
              if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
           enddo
        endif
        return
	end
