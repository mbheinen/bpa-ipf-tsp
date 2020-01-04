C    @(#)cpyinbfil.f	20.3 2/13/96
C****************************************************************
C
C       File: cpyinbfil
C
C       Purpose: Routine to copy inbuf to file
C
C       Author: Walt Powell  Date: 12 November 1992
C       Called by: ipf_main 
C
C****************************************************************
C
	subroutine cpyinbfil (inbuf, file)
        character inbuf * (*)
        integer file

        include 'ipfinc/blank.inc'
        include 'ipfinc/jobctl.inc'

	character text * 132

        rewind file

        ix = 0
        istat = init_bufln( inbuf, ix, text, lc )
        do while ( istat .eq. 0 )
           istat = nxt_bufln( inbuf, ix, text, lc )
           write ( file, '(a)' ) text(1:lc)
        enddo

        write (file, '(a)' ) '(END)'
        rewind file
        read (file, '(a)' , end=900) inrcd
        buf = inrcd

  900   return
        end
