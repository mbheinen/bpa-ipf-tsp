C    @(#)gtbsvolt.f	20.5 1/7/99

C****************************************************************
C
C   File: gtbsvolt.for
C
C   Purpose: Routine to obtain bus solution voltages and angles
C
C   Author: Walt Powell  Date: 4 November 1992
C   Called by: p_gtdata.f
C
C****************************************************************
C
        subroutine gtbsvolt (in_buffer, out_buffer, error)
        character in_buffer *(*), out_buffer *(*)
        integer error
c
c       This subroutine returns the list of bus name, kv, voltage, and
c       angle in the following format:
c
c       (1:8) bus name
c       (9:12) bus kv
c       (14:17) voltage, kv (in I4) 
c       (19:22) angle, degrees (in i4)
c
c       Output parameter:
c
c       in_buffer - a character string specifying desired data
c       out_buffer - a character string for storing data
c       error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
c
        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'

        character  null * 1, linefeed * 1, code * 4, base_c * 4
        integer    o2, bufsize
 
        null = char(0)
        linefeed = char(10)

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)

        nb = 1
	bufsize = len (out_buffer)
        do while (nb .le. ntot .and. o2 + 22 .lt. bufsize)
           kt = inp2opt(nb)
           ivolt = dsqrt (e(kt) ** 2 + f(kt) ** 2) * base(nb)
           iangle = 57.2957795 * datan2 (f(kt), e(kt))
           base_c = code(base(nb), 4, 0)
           write (out_buffer(o2:o2+22), 310) 
     1        bus(nb), base_c, ivolt, iangle, linefeed
  310      format (a8, a4, 1x, i4, 1x, i4, a)
           o2 = o2 + 23
           nb = nb + 1
        enddo
        if (nb .le. ntot) then
           o2 = o2 - 23
           write (out_buffer(o2:o2+9), 320) null
  320      format ('*[MORE]', a)
        endif 

        o2 = o2 + 1
        out_buffer(o2:o2) = null
        return
        end
