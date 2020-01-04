C    @(#)get_orpt13.f	20.4 1/7/99
C****************************************************************
C
C       File: get_orpt13.f
C       Purpose: Routine to generate phase shifter output 
C                reports using IPF command
C
C                / REPORTS, SELECT PHASE_SHIFT
C                  WHERE BUS = "<busname>"
C                (END)
C
C       Author: Walt Powell  Date: 14 July 1994
C                            Modified: 14 July 1994
C       Called by:
C
C****************************************************************
C
	subroutine get_orpt13 (scrfil)
        integer scrfil

	include 'ipfinc/parametr.inc'	
	include 'ipfinc/lfiles.inc'
	include 'ipfinc/blank.inc'
        include 'ipfinc/sortuvov.inc'

        character in_buffer*(MAXBUFFER), out_buffer*(MAXBUFFER), 
     &            null*1, linefeed*1
	integer apdoutbuf, o1, o2, findstr, status, phshftrpt
        logical change_v, change_f, get_defv, load_fltr

        null = char(0)
        linefeed = char(10)

        change_f = .true.      ! Change filters

        do while (change_f)
           in_buffer(1:1) = null
           i1 = index (in_buffer, null)
           length = apdoutbuf(i1, '/REPORTS, SELECT PHASE_SHIFT ',
     &                        in_buffer(i1:))
           i1 = i1 + length
           change_f = load_fltr(in_buffer, out_buffer, i1)
           if (change_f) then
              out_buffer(1:1) = null
              ix = findstr (in_buffer, 'PHASE_SHIFT')
     &           + lastch ('PHASE_SHIFT')
              status = phshftrpt(in_buffer(ix:), out_buffer,
     &                           scrfil)
              o1 = 1
              do while (o1 .lt. MAXBUFFER .and. 
     &                  out_buffer(o1:o1) .ne. null)
                 o2 = nxt_term(out_buffer(o1+1:)) + o1
                 if (out_buffer(o1:o1) .eq. '/') then
                    o2 = MAXBUFFER + 1
                 else
                    write (*, 110) out_buffer(o1:o2-1)
  110               format (1x, a)
                 endif
                 o1 = o2
                 if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
              enddo
           endif
        enddo
        return
        end
