C    @(#)nx_date.f	20.2 2/28/00
      subroutine nx_date( month, day, year, weekday )
      integer month, day, year, weekday

C     This is the "root level" routine that should be changed
c     if you don't have a "c" compiler and your system supports
c     a Fortran callable date routine.

C     This routine gets the current date from the system in
c     integer format

C**********************************************************************
C***  Use one of the following - comment out the others  **************
C**********************************************************************

C**********************************************************************
C     The following code is the standard portable code that calls
c     a "C" routine that calls a standard "C" library routine.

      integer tm(9)

C *** CALTME is a "C" routine that is portable

      call caltme( tm )
      day   = tm(4)
      month = tm(5) + 1
      year  = mod (tm(6), 100)
      weekday = tm(7) + 1
C**********************************************************************
C**********************************************************************
C     VAX/VMS VERSION THAT AVOIDS CALLS TO "C" USING THE VMS
C     FORTRAN LIBRARY ROUTINE "IDATE"

C     call idate(  month, day, year )

C**********************************************************************
C**********************************************************************

C     Your system's Fortran date routine and any code to return
C     month, day, year

C**********************************************************************

      return
      end
