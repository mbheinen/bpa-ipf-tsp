C    @(#)ge_scan.f	20.2 3/29/99
C****************************************************************  
C   File: ge_scan.f  
C   Purpose: This is a GE-specific scanning routine to parse a   
C            character string into separate words:  
C****************************************************************  
C  
       subroutine ge_scan (ain, aout, nwrd, limit1, limit2)  
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
C       1. A string enclosed with "..." parses as a separate entity  
C          irrespective of any imbedded delimiters.  
C  
c  
c***********************************************************************  
c  
      character * 1   back_slash  
      integer zfirst, zlast  
c  
c     some compilers use '\' as a "C" style escape character  
c  
      back_slash = char(92)  
C  
C     Clear the output array before starting  
C  
      lenout = len(aout(1))  
      nwrd = 1  
      aout(1) = ' '  
      nchr = 0  
      i = 1
      last = len( ain )  
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
            zfirst = i  
            zlast = last1 - 1  
            do while (zlast .gt. zfirst .and.   
     &                ain(zlast:zlast) .ne. '"')  
              zlast = zlast - 1  
            enddo  
            do while (i .lt. zlast)  
               if ( nchr .lt. lenout ) then  
                  nchr = nchr + 1  
                  aout(nwrd)(nchr:nchr) = ain(i:i)  
               endif  
               i = i + 1  
            enddo  
            if (ain(i:i) .eq. '"') i = i + 1   ! skip the quote  
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
               aout(nwrd)(nchr:nchr) = ain(i:i)  
            endif  
            i = i + 1  
         endif  
      enddo      
      if (nchr .eq. 0) nwrd = nwrd - 1  
      return  
      end  
