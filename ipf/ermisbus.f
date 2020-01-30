C    @(#)ermisbus.f	20.3 2/13/96
C****************************************************************
C
C   File: ermisbus.f
C   Purpose: Routine to print out missing bus message together with
C            the three nearest candidates.
C
C   Author: Walt Powell  Date: 2 June 1993
C   Called by:
C
C****************************************************************
C
	subroutine ermisbus (busname, basekv, text)
        character busname * 8, text *(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/prt.inc'

        logical found

        write (errbuf(1), 194) busname, basekv, text(1:33)
  194   format ('Bus ', a8, f6.1, ' on record is not in system (', 
     1     a, ')')
        j1 = 1
        j2 = ntot_alf
        found = .false.
        do while (j1 .le. j2 .and. .not. found)
           i = (j1 + j2) / 2
           j = alf2inp(i)
           komp = kompr (bus(j), busname, junk) 
           if (komp .eq. 0) komp = 100.0 * (base(j) - basekv)
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
           if (i .gt. 0 .and. i .le. ntot_alf) then
              j = alf2inp(i)
              ierr = ierr + 1
              write (errbuf(ierr), 142) bus(j),base(j)
  142         format (' Adjacent bus names > ', a8, f6.1)
           endif
        enddo
        call prterx ('W', ierr)
        return
        end
