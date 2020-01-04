C    @(#)gtarofzn.f	20.3 2/13/96
C****************************************************************
C
C   File: gtarofzn.f
C   Purpose: Routine to obtain BCD image of area associated with 
C            named zone.
C
C   Author: Walt Powell  Date: 05 April 1994
C                        Modified: 
C   Called by:
C
C****************************************************************
C
        subroutine gtarofzn (in_buffer, out_buffer)

        character out_buffer * (*)
        character in_buffer * (*)
c
c       This subroutine returns the area name associated with the
C       input zone name.
c       Output parameter:
c
c       out_buffer - a character string for storing data
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/lfiles.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/prt.inc'
 
        character rec120 * 120, rec80 * 80, word(50) * 10, zname * 2
        character * 1  null, linefeed
        integer   apdoutbuf, o1, find_zon, findstr
        logical   go

        null = char(0)
        linefeed = char(10)

        out_buffer(1:1) = null
        max_buf = len(out_buffer) - 580
        o1 = 1

        last = index (in_buffer, null)
        ix = findstr (in_buffer(1:last), 'AREA_OF_ZONE')
        if (ix .gt. 0) then
           ix = ix + len ('AREA_OF_ZONE')
           call uscan (in_buffer(ix:last), word, numwrd, '/=' ,
     &              ', ' // linefeed // null )

           iwrd = 2
           if (word(iwrd) .eq. '=') iwrd = iwrd + 1
           zname = word(iwrd)

c          Find area number zone.                        
                                                               
           k = find_zon(zname) 
           if (k .gt. 0) then
              rec80 = arcnam(acznum(k))
              length = apdoutbuf( o1, arcnam(acznum(k)), 
     &                            out_buffer(o1:) )
              o1 = o1 + length
           endif
        endif

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
