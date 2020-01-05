C    @(#)gtcommnt.f	20.3 2/13/96
C****************************************************************
C
C       File: gtcommnt.f
C
C       Purpose: Subroutine to obtain headers and comments.
C
C       Author: Walt Powell  Date: 1 May 1993
C       Called by: p_gtdata.f
C
C****************************************************************
C
        subroutine gtcommnt (in_buffer, out_buffer)

        character in_buffer * (*), out_buffer * (*)
c
c       This subroutine returns WSCC-formated input data records.
c       Output parameter:
c
c       in_buffer - a character string specifying desired data
c       out_buffer - a character string for storing data
c       error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
c
C       This routine obtains and applies a filter for entire input 
C       network data.
 
        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/basval.inc'
        include 'ipfinc/header.inc'
        include 'ipfinc/coment.inc'

        character  text * 140, null * 1, linefeed * 1
        integer o2, apdoutbuf

        save

        null = char(0)
        linefeed = char(10)
        last = index (in_buffer, null)

        out_buffer(1:1) = null
        o2 = 1
c
c       Process case records
c
        text = 'CASE_ID = ' // basval(4)
        last = lastch(text)
        length = apdoutbuf( o2, text(1:last), out_buffer(o2:) )
        o2 = o2 + length

        text = 'CASE_DS = ' // basval(7)
        last = lastch (text)
        length = apdoutbuf( o2, text(1:last), out_buffer(o2:) )
        o2 = o2 + length

        text = 'H Powerflow version: ' // prgvsn // '     CaseID: ' //
     &         basval(4)(1:10) // '     Case Description: ' //
     &         basval(7) // '      Date: ' // basval(5)(1:10)
        last = lastch (text)
        length = apdoutbuf( o2, text(1:last), out_buffer(o2:) )
        o2 = o2 + length

        do i = 1, 2
           text = 'H' // coment(i)
           last = lastch (text)
           length = apdoutbuf( o2, text(1:last), out_buffer(o2:) )
           o2 = o2 + length
        enddo

        do i = 1, ncom
           text = 'C' // com(i)
           last = lastch (text)
           length = apdoutbuf( o2, text(1:last), out_buffer(o2:) )
           o2 = o2 + length
        enddo

        return
        end
