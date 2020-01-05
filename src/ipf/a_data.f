C    @(#)a_data.f	20.4 2/13/96
C****************************************************************
C
C   File: a_data.f
C   Purpose: Routine to obtain BCD image of INPUT Area "A"
C            data 
C
C   Author: Walt Powell  Date: 22 July 1992
C                        Modified: 22 July 1992
C   Called by:
C
C****************************************************************
C
        subroutine a_data (in_buffer, out_buffer)
        character in_buffer * (*), out_buffer * (*)
c
c       This subroutine returns WSCC-formated area input data records.
c       Output parameter:
c
c       out_buffer - a character string for storing data
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/lfiles.inc'
c	Global varables used:
c		dbug
        include 'ipfinc/blank.inc'
c	Global varables used:
c		ntotc, kase1
        include 'ipfinc/arcntl.inc'
c	Global varables used:
c		arcnam, arczns
        include 'ipfinc/area.inc'
c	Global varables used:
c		None
        include 'ipfinc/prt.inc'
c	Global varables used:
c		errbuf
 
        character rec120 * 120, rec80 * 80
        character * 1  null, linefeed
        integer   apdoutbuf, o1
        logical   go

        null = char(0)
        linefeed = char(10)

        out_buffer(1:1) = null
        max_buf = len(out_buffer) - 580
        o1 = 1
        do ia = 1, ntotc
           call bcdarc( ia, rec120 )
           rec80 = rec120
           length = apdoutbuf( o1, rec80, out_buffer(o1:) )
           o1 = o1 + length
           if ( o1 .gt. max_buf  .or.  length .eq. 0 ) goto 500
           do iac = 1, MAXCAZ/MAXCAZR - 1
              if ( arczns(iac*MAXCAZR+1,ia)  .eq. ' ' ) goto 200
              call bcdarc2( ia, iac, rec120 )
              rec80 = rec120
              length = apdoutbuf( o1, rec80, out_buffer(o1:) )
              o1 = o1 + length
              if ( o1 .gt. max_buf  .or.  length .eq. 0 ) goto 500
           enddo
  200   continue
        enddo

        ia = ntotc
        if (ia .lt. ntotc) then
           o1 = o1 - 22
           write (out_buffer(o1:o1+9), 320) linefeed, null
  320      format (a, '*[MORE]', a)
        else
           if (out_buffer(o1:o1) .ne. linefeed) o1 = o1 + 1
           out_buffer(o1:o1) = null
        endif

        goto 600

  500   continue
        write (errbuf(1), 511) arcnam(ia)
  511   format (' Output buffer overflowed at area ', a10) 
        call prterx ('W', 1)

  600   continue

c**** save until positive its OK to remove
c**** This is done by the calling routine
c*
c*       Debug printout invoked with IPF command /TRACE, CHANGE = ON
c*
c*        if (kase1(27) .ne. 0) then
c*           o1 = 1
c*           do while (o1 .lt. max_buf .and. 
c*     &              (out_buffer(o1:o1) .ne. null))
c*              next = nxt_term (out_buffer(o1+1:)) + o1
c*              write (dbug, 830) out_buffer(o1:next-1)             
c*  830         format (1x, a)
c*              o1 = next
c*              if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
c*           enddo
c*        endif
c******************************************

        return 
        end
