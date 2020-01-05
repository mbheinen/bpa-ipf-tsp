C    @(#)i_data.f	20.4 2/13/96
C****************************************************************
C
C   File: i_data.f
C   Purpose: Routine to obtain BCD image of INPUT Intertie "I"
C            data 
C
C   Author: Walt Powell  Date: 22 July 1992
C                        Modified: 22 July 1992
C   Called by:
C
C****************************************************************
C
        subroutine i_data (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)
c
c       This subroutine returns WSCC-formated intertie input data records.
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
 
        character record * 120
        character null * 1, linefeed * 1
        integer o2, apdoutbuf

        max_buf = len( out_buffer ) - 150
        null = char(0)
        linefeed = char(10)

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        nb = 1
        do while (nb .le. ntotic .and. o2 .lt. max_buf)
           komp = kompr (arcint(1,nb), arcint(2,nb), junk)
           if (komp .lt. 0) then
              call bcdari (nb, record)
              length = apdoutbuf(o2, record, 
     1                           out_buffer(o2:))
              o2 = o2 + length
              if (length .eq. 0) then
                 write (errbuf(1), 112) arcint(1,nb), arcint(2,nb)
  112            format (' Output buffer overflowed at intertie ', 
     1              a10, 1x, a10) 
                 call prterx ('W', 1)
                 go to 900
              endif
           endif
           nb = nb + 1
        enddo
        if (nb .lt. ntotic) then
           o2 = o2 - 22
           write (out_buffer(o2:o2+9), 320) linefeed, null
  320      format (a, '*[MORE]', a)
        else
           if (out_buffer(o2:o2) .ne. linefeed) o2 = o2 + 1
           out_buffer(o2:o2) = null
        endif

  900   continue
        return 
        end
