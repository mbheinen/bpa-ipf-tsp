C    @(#)uscan.f	20.3 2/13/96
C****************************************************************
C   File: uscan.f
C   Purpose: This is a universal scanning routine to parse a 
C            character string into separate words:
C****************************************************************
C
       subroutine uscan (ain, aout, nwrd, limit1, limit2)
       character aout (*)*(*), ain *(*), limit1 *(*), limit2 *(*)
C
C       Input parameters:
C
C          ain :    character string
C          limit1 : character string denoting parsed delimiters 
C                   (returns single character token)
C          limit2 : character string denoting unparsed delimiters
C
C       Output parameters:
C
C          aout() : a character array of parsed tokens
C          nwrd   : counter of number of tokens in aout()
C
C       Special considerations:
C
C       1. A blank character can be inserted in the word and not be
C          interpreted as a blank by using the "#".  USCAN will replace
C          in the parsed output the "#" with a blank.
C
C       2. A string enclosed with "..." parses as a separate entity
C          irrespective of any imbedded delimiters.
C
c
c***********************************************************************
c*** Modified for DOS and UNIX: "\" and "/" are directory path characters
c***   and should not be used in the parse strings for any line to be
c***   parsed that may contain a file path/name
c***
c***            SLASH  BACKSLASH  "/"  "\"
c***            LEFT AND RIGHT SQUARE BRACKET  "["  "]"
c***            LEFT AND RIGHT PREN  "("  ")"
c***            GREATER THAN AND LESS THAN  ">"  "<"
c***
c***  Now the above characters are stripped off of the beginning and end,
c***  but are allowed otherwise.
c***********************************************************************
c
      logical found
      character * 1   back_slash
c
c some compilers use '\' as a "C" style escape character
c
      back_slash = char(92)

      last = len( ain )
      do while ( last .gt. 1  .and.
     &           ( ain(last:last) .eq. ' '  .or.
     &             ain(last:last) .eq. ')'  .or.
     &             ain(last:last) .eq. '<'  .or.
     &             ain(last:last) .eq. ']'  .or.
     &             ain(last:last) .eq. back_slash  ) )
         last = last - 1
      end do
      ifirst = 1
      do while ( ifirst .lt. last  .and.
     &           ( ain(ifirst:ifirst) .eq. ' '  .or.
     &             ain(ifirst:ifirst) .eq. '('  .or.
     &             ain(ifirst:ifirst) .eq. '>'  .or.
     &             ain(ifirst:ifirst) .eq. '['  .or.
     &             ain(ifirst:ifirst) .eq. '/'       ) )
         ifirst = ifirst + 1
      end do
c*************************************************************

C
C     Clear the output array before starting
C
      lenout = len(aout(1))
      nwrd = 1
      aout(1) = ' '
      nchr = 0
      i = ifirst
      last1 = last + 1
      do while ( i .lt. last1 )
c
c        Check for "..." string
c
         if ( ain(i:i) .eq. '"' ) then
            if ( nchr .ne. 0 ) nwrd = nwrd + 1
            nchr = 0
            aout(nwrd) = ' '
            i = i + 1   ! skip the quote
            do while ( ain(i:i) .ne. '"'  .and.  i .lt. last1 )
               if ( nchr .lt. lenout ) then
                  nchr = nchr + 1
                  aout(nwrd)(nchr:nchr) = ain(i:i)
               endif
               i = i + 1
            enddo
            i = i + 1   ! skip the quote
            if ( nchr .ne. 0 ) then
               nwrd = nwrd + 1
               aout(nwrd) = ' '
               nchr = 0
            endif
C
C        Check for parsed limiters
C
         else if ( index( limit1, ain(i:i) ) .ne. 0 ) then
            if ( nchr .ne. 0 ) nwrd=nwrd+1
            aout(nwrd) = ain(i:i)
            nchr = 0
            nwrd = nwrd + 1
            aout(nwrd) = ' '
            i = i + 1
C
C        Check for unparsed limiters
C
         else if ( index( limit2, ain(i:i) ) .ne. 0 ) then
            if ( nchr .ne. 0 ) then
               nwrd = nwrd + 1
               aout(nwrd) = ' '
               nchr = 0
            endif
            i = i + 1
         else
C
C        Default: append to current string.
C
            nchr=nchr+1
            if (nchr .le. lenout ) then
               if (ain(i:i).eq.'#') then
                  aout(nwrd)(nchr:nchr) = ' '
               else
                  aout(nwrd)(nchr:nchr) = ain(i:i)
               endif
            endif
            i = i + 1
         endif
      enddo    
      if (nchr .eq. 0) nwrd = nwrd - 1
      return
      end
