C    @(#)nxt_term.f	20.3 2/13/96
	integer function nxt_term (string)
        character string *(*)
c
c       This function determines the next NULL or LINEFEED terminator
c       in the c-compatible STRING.
c
        character null * 1, linefeed * 1
      
        null = char(0)
        linefeed = char(10)

        ln = len (string)
        inull = index (string, null)
        ilf   = index (string, linefeed)
        if (inull .eq. 0) inull = ln + 1
        if (ilf   .eq. 0) ilf   = ln + 1

        nxt_term = min (ilf, inull)
        return
        end
