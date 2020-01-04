C    @(#)ptcommnt.f	20.3 2/13/96
C****************************************************************
C
C     File: ptcommnt.f
C
C     Purpose: Subroutine to write new headers and comments.
C
C     Author: Walt Powell  Date: 1 May 1993
C     Called by: p_ptdata.f
C
C****************************************************************
C
      subroutine ptcommnt (in_buffer, out_buffer)

      character in_buffer * (*), out_buffer * (*)
c
c     This subroutine returns WSCC-formated input data records.
c     Output parameter:
c
c     in_buffer - a character string specifying desired data
c     out_buffer - a character string for storing data
c     error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
c
 
      include 'ipfinc/parametr.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/basval.inc'
      include 'ipfinc/header.inc'
      include 'ipfinc/coment.inc'

      character * 10  capital

      character  text * 140, null * 1, linefeed * 1, word(10) * 60
      integer o2, apdoutbuf

      save

      null = char(0)
      linefeed = char(10)
      last = index (in_buffer, null)

      out_buffer(1:1) = null
      o2 = 1
c
c     Process case records
c
      i1 = 1
      numhdr = 0
      ncom = 0
      do while ( i1 .lt. last )
         next_i1 = nxt_term(in_buffer(i1+1:)) + i1
         text = in_buffer(i1:next_i1-1)
         if ( capital( text(1:7) ) .eq. 'CASE_ID' .or. 
     &        capital( text(1:6) ) .eq. 'CASEID' ) then
            call uscan( text, word, nwrd, '=', ' ,' )
            basval(4) = word(3)
            chase1(1) = word(3)
         else if ( capital( text(1:7) ) .eq. 'CASE_DS' .or.
     &             capital( text(1:6) ) .eq. 'CASEDS' ) then
            call uscan( text, word, nwrd, '=', ' ,' )
            basval(7) = word(3)
            chase1(34) = word(3)(1:10)
            chase1(35) = word(3)(11:20)
         else if ( text(1:1) .eq. 'H'  .and.  numhdr .lt. 2 ) then
            numhdr = numhdr + 1
            coment(numhdr) = text(2:)
         else if ( text(1:1) .eq. 'C'  .and.  ncom .lt. MAXCMT ) then
            ncom = ncom + 1
            com(ncom) = text(2:)
         endif

         i1 = next_i1
         if (in_buffer(i1:i1) .eq. linefeed) i1 = i1 + 1

      enddo

      return
      end
