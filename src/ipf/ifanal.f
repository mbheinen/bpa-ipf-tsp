C    @(#)ifanal.f	20.3 2/13/96
      subroutine ifanal (form, num, check, onoff, tag, status)
      character * 1 form, tag
      integer check, status, onoff
C
C     This function returns a character value designating the
C     status of analysis report "NUM".
C
C     Input parameters:
C
C        FORM   : 'P' - Print report;
C                 'F' - Fiche report.
C        NUM    : Analysis listing report number.
C        CHECK  : 0 - No zone or ownership restriction;
C                 1 - Zone and/or ownership restriction.
C        ONOFF  : 0 - Unconditionally no output for this media.
C                 1 - Potential output for this media.
C
C     Output value:
C
C        TAG :    ' ' - No report;
C                 'F' - Full report;
C                 'Z' - Report restricted by zones;
C                 'O' - Report restricted by ownerships;
C                 'B' - Report restricted by both zones and ownerships.
C
C        STATUS:  0/1 - Do not/do print (of fiche).
C
 
 
      include 'ipfinc/parametr.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/zonlst.inc'
 
      if (form .eq. 'P') then
         if (aplist(num) .eq. 1 .and. onoff .gt. 0) then
C
C           Test for Zone-Ownership restriction
C
            if (check .eq. 1) then
               if (npzanl .eq. 0) then
                  if (npoanl .eq. 0) then
                     tag = 'F'
                  else
                     tag = 'O'
                  endif
               else
                  if (npoanl .eq. 0) then
                     tag = 'Z'
                  else
                     tag = 'B'
                  endif
               endif
            else
               tag = 'F'
            endif
         else
            tag = ' '
         endif
         if (tag .eq. ' ') then
            status = 0
         else
            status = 1
         endif
      else
         if (aflist(num) .eq. 1 .and. onoff .gt. 0) then
C
C           Test for Zone-Ownership restriction
C
            if (check .eq. 1) then
               if (nfzanl .eq. 0) then
                  if (nfoanl .eq. 0) then
                     tag = 'F'
                  else
                     tag = 'O'
                  endif
               else
                  if (nfoanl .eq. 0) then
                     tag = 'Z'
                  else
                     tag = 'B'
                  endif
               endif
            else
               tag = 'F'
            endif
         else
            tag = ' '
         endif
         if (tag .eq. ' ') then
            status = 0
         else
            status = 1
         endif
      endif
      return
      end
