C    @(#)ermisare.f	20.4 6/27/97
C****************************************************************
C
C   File: ermisare.f
C   Purpose: Routine to print out missing area message together with
C            the three nearest candidates.
C
C   Author: Walt Powell  Date: 2 June 1993
C   Called by:
C
C****************************************************************
C
	subroutine ermisare (areaname, text)
        character areaname *(*), text *(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/prt.inc'

        logical found

        write (errbuf(1), 194) areaname, text(1:33)
  194   format ('Area ', a10, ' on record is not in system (', 
     1     a, ')')
        j1 = 1
        j2 = ntotc
        found = .false.
        do while (j1 .le. j2 .and. .not. found)
           i = (j1 + j2) / 2
           komp = kompr (arcnam(i), areaname, junk) 
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
           if (i .gt. 0 .and. i .le. ntotc) then
              ierr = ierr + 1
              write (errbuf(ierr), 142) arcnam(i)
  142         format (' Adjacent area names > ', a10)
           endif
        enddo
        call prterx ('W', ierr)
        return
        end
