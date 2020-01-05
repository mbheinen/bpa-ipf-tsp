C    @(#)oermisbus.f	20.3 2/13/96
C****************************************************************
C
C   File: oermisbus.f
C   Purpose: Routine to print out missing bus message together with
C            the three nearest candidates using the alternate or
C            base case data.
C
C   Author: Walt Powell  Date: 18 August 1993
C   Reference:  See also ermisbus.f
C   Called by:
C
C****************************************************************
C
	subroutine oermisbus (busname, basekv, text)
        character busname * 8, text *(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/alt_case.inc'
        include 'ipfinc/prt.inc'

        logical found

        write (errbuf(1), 194) busname, basekv, text(1:33)
  194   format ('Bus ', a8, f6.1, ' on record is not in system (', 
     1     a, ')')
        j1 = 1
        j2 = ontot_alf
        found = .false.
        do while (j1 .le. j2 .and. .not. found)
           i = (j1 + j2) / 2
           j = oalf2inp(i)
           komp = kompr (oldbus(j), busname, junk) 
           if (komp .eq. 0) komp = 100.0 * (oldbase(j) - basekv)
           if (komp .lt. 0) then
              j1 = i + 1
           else if (komp .gt. 0) then
              j2 = i - 1
           else
              found = .true.
              j1 = i
              j2 = i
           endif
        enddo
        ierr = 1
        do i = min(j1,j2)-1, max(j1,j2)+1
           if (i .gt. 0 .and. i .le. ontot_alf) then
              j = oalf2inp(i)
              ierr = ierr + 1
              write (errbuf(ierr), 142) oldbus(j),oldbase(j)
  142         format (' Adjacent bus names > ', a8, f6.1)
           endif
        enddo
        call prterx ('W', ierr)
        return
        end
