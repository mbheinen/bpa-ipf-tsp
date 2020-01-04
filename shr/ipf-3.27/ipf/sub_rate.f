C    @(#)sub_rate.f	20.4 7/18/96
C****************************************************************
C
C   	File: sub_rate.f
C
C   	Purpose: Substitute extended ratings into nominal field
C                                                                      *
C       Input parameters:
C
C             xbuf     - the encoded character branch record
c             ratcod   - a character string denoting extended 
c                        line ratings used:
C
C                        M: Transformers = min(TEB), Lines = min(EB)
C                        T: Transformers = T, Lines = T
C                        E: Transformers = E, Lines = T
C                        B: Transformers = B, Lines = B
C                        ET: Transformers = E, Lines = T
C                        EB: Transformers = E, Lines = B
C
C   	Author: Hanford Van Ness    Date: circa 1980
C   	Called by: ext_brn.f
C
C****************************************************************
C
      subroutine sub_rate ( xbuf, ratcod )
      character*(*) xbuf, ratcod


      if ( index('LTE ', xbuf(1:1)) .ne. 0 ) then

         read(xbuf(34:37), '(bz, f4.0)') rate0
         read(xbuf(81:84), '(bz, f4.0)') rate1
         read(xbuf(85:88), '(bz, f4.0)') rate2
         read(xbuf(89:92), '(bz, f4.0)') rate3
 
         if ( rate1+rate2+rate3 .gt. 0.0 ) then

c           There is at least one extended rating on this branch

            if ( rate1 .eq. 0.0 ) rate1 = 10000.0
            if ( rate2 .eq. 0.0 ) rate2 = 10000.0
            if ( rate3 .eq. 0.0 ) rate3 = 10000.0
 
            if (ratcod .eq. 'M' ) then

c              Replace nominal rating with minimum extended rating

               xbuf(34:37) = xbuf(81:84)
               rate0 = rate1
               if ( rate2 .lt. rate0 ) then
                  xbuf(34:37) = xbuf(85:88)
                  rate0 = rate2
               endif
               if ( rate3 .lt. rate0 ) then
                  xbuf(34:37) = xbuf(89:92)
               endif
            else if (ratcod .eq. 'T' ) then

c              Replace nominal rating with thermal rating

               if (rate1 .gt. 0.0) xbuf(34:37) = xbuf(81:84)
            else if (ratcod .eq. 'E' .or. ratcod .eq. 'ET' ) then

c              Replace nominal rating with EMERGENCY/thermal rating

               if (xbuf(1:1) .eq. 'T') then
                 if (rate2 .gt. 0.0) xbuf(34:37) = xbuf(85:88)
               else
                 if (rate1 .gt. 0.0) xbuf(34:37) = xbuf(81:84)
               endif
            else if (ratcod .eq. 'B' ) then

c              Replace nominal rating with bottleneck rating

               if (xbuf(1:1) .eq. 'T') then
                 if (rate3 .gt. 0.0) xbuf(34:37) = xbuf(89:92)
               else
                 if (rate2 .gt. 0.0) xbuf(34:37) = xbuf(85:88)
               endif
            else if (ratcod .eq. 'EB' ) then

c              Replace nominal rating with emergency/thermal rating

               if (xbuf(1:1) .eq. 'T') then
                 if (rate2 .gt. 0.0) xbuf(34:37) = xbuf(85:88)
               else
                 if (rate1 .gt. 0.0) xbuf(34:37) = xbuf(81:84)
               endif
            endif
 
         endif
 
      endif
 
      return
      end
