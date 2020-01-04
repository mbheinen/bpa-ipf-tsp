C    @(#)xscan.f	20.3 2/13/96
        subroutine xscan (ain, aout, nwrd)
C
C       This routine will parse a FREE FIELD CHARACTER STRING BUFFER
C       "AIN", into a character array "AOUT".
C
C       Limiters are blanks. Distinct words are continuous strings
C       and the following arithmetic operators:
C
C          "+" (if not part of an exponent)
C          "-" (if not part of an exponent)
C          "**"
C          "*"
C          "/"
C          ">"
C          "<"
C          "("
C          ")"
C
C       A blank character can be inserted in the word and not be
C       interpreted as a blank by using the "#".  XSCAN will insert
C       the blank in the same position as the word is built.
C
        character    aout (*) * (*)
        character    ain * (*)
C
C       Clear the output array before starting
C
        nwrd=1
        nchr=0
        lenout = len(aout(1))
        aout(nwrd) = ' '
        do 100 i=1,len(ain)
           ityp = index (' ()*/+-<>', ain(i:i))
           if (ityp .eq. 0) then
              nchr=nchr+1
              if (nchr .le. lenout ) then
                 if (ain(i:i).eq.'#') then
                    aout(nwrd)(nchr:nchr) = ' '
                 else
                    aout(nwrd)(nchr:nchr) = ain(i:i)
                 endif
              endif
C
C          Blank delimits words.
C
           else if (ityp .eq. 1) then
              if (nchr.ne.0) then
                 nwrd=nwrd+1
                 aout(nwrd) = ' '
                 nchr=0
              endif
C
C          "+" or "-" delimits word if it is not part of a numerical
C          exponential expression or it is a arithmetic suffix.
C
           else if (ityp .eq. 6 .or. ityp .eq. 7) then
              if (nchr .gt. 0) then
                 if (aout(nwrd)(nchr:nchr) .eq. 'E') then
                    nchr=nchr+1
                    if (nchr .le. lenout ) then
                       aout(nwrd)(nchr:nchr) = ain(i:i)
                    endif
                 else if (nwrd .eq. 1 .and. nchr .eq. 0) then
                    nchr=nchr+1
                    if (nchr .le. lenout ) then
                       aout(nwrd)(nchr:nchr) = ain(i:i)
                    endif
                 else
                    if (nchr.ne.0) then
                       nwrd=nwrd+1
                    endif
                    aout(nwrd) = ain(i:i)
                    nchr=0
                    nwrd=nwrd+1
                    aout(nwrd) = ' '
                 endif
              else
                 if (nwrd .gt. 1) then
                    if (aout(nwrd-1) .eq. '(' ) then
                       nchr=nchr+1
                       if (nchr .le. lenout ) then
                          aout(nwrd)(nchr:nchr) = ain(i:i)
                       endif
                    else
                       aout(nwrd) = ain(i:i)
                       nchr=0
                       nwrd=nwrd+1
                       aout(nwrd) = ' '
                    endif
                 else
                    aout(nwrd) = ain(i:i)
                    nchr=0
                    nwrd=nwrd+1
                    aout(nwrd) = ' '
                 endif
              endif
C
C          Look back for exponentiation.
C
           else if (ityp .eq. 4) then
              if (nchr.ne.0) then
                 nwrd=nwrd+1
                 aout(nwrd) = ain(i:i)
                 nchr=0
                 nwrd=nwrd+1
                 aout(nwrd) = ' '
              else
                 if (nwrd .gt. 1) then
                    if (aout(nwrd-1) .eq. '*') then
                       aout(nwrd-1) = '**'
                    else
                       aout(nwrd) = ain(i:i)
                       nchr=0
                       nwrd=nwrd+1
                       aout(nwrd) = ' '
                    endif
                 else
                    aout(nwrd) = ain(i:i)
                    nchr=0
                    nwrd=nwrd+1
                    aout(nwrd) = ' '
                 endif
              endif
C
C          All remaining operators are delimiters.
C
           else
              if (nchr.ne.0) then
                 nwrd=nwrd+1
              endif
              aout(nwrd) = ain(i:i)
              nchr=0
              nwrd=nwrd+1
              aout(nwrd) = ' '
           endif
 100    continue
 150    if (nchr .eq. 0) nwrd = nwrd - 1
        return
        end
