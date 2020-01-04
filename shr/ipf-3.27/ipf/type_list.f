C    @(#)type_list.f	20.5 1/7/99
C****************************************************************
C
C   File: type_list.f
C
C   Purpose: Routine to obtain an record type list.
C
C   Author: Walt Powell  Date: 18 May 1992
C   Called by: p_gtdata.f
C
C****************************************************************
C
        subroutine type_list (in_buffer, out_buffer, error)
        integer error

        include 'ipfinc/parametr.inc'

        character in_buffer * (MAXBUFFER), out_buffer * (MAXBUFFER)
c
c       This subroutine returns the list of system buses in out_buffer.
c       Output parameter:
c
c       in_buffer - a character string specifying desired data
c       out_buffer - a character string for storing data
c       error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
 
        character  null * 1, linefeed * 1, record_type(37)*2
        integer o2
 
        data record_type / 
     &   '* ', 'A*', 'A?', 'I ', 'B*', 'L*', 'B?', 'B ', 'BE', 'BS', 
     &   'BC', 'BD', 'BV', 'BQ', 'BG', 'BT', 'BX', 'BM', 'BF', '+ ', 
     &   'X ', 'Q ', 'LD', 'LM', 'E ', 'T ', 'TP', 'R ', 'RZ',
     &   '+*', '+ ', '+N', '+I', '+F', '+S', '+P', '+A' /
        null = char(0)
        linefeed = char(10)

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)

        do nb = 1, 29
           write (out_buffer(o2:o2+3), 310) record_type(nb), 
     1        linefeed
  310      format (a2, a)
           o2 = o2 + 3
        enddo
        out_buffer(o2:o2) = null
        return
        end
