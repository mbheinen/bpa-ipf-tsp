C    @(#)n_time.f	20.3 2/13/96
      subroutine n_time( hour, minute, second )
      integer  hour, minute, second

c   This is the "root level" routine that should be changed
c      if you don't have a "C" compiler and your system supports
c      a fortran callable time routine.

c   This routine gets the current time from the system in
c      integer format

c**********************************************************************
c***  use one of the following - comment out the others  **************
c**********************************************************************

c**********************************************************************
c**********************************************************************
c   the following code is the standard portable code that calls
c   a "c" routine that calls a standard "c" library routine.

      integer tm(9)
c *** caltme is a "c" routine that is portable
      call caltme( tm )
      hour   = tm(3)
      minute = tm(2)
      second = tm(1)

c**********************************************************************
c**********************************************************************

c   VAX/VMS version that avoids calls to "C" using the VMS
c   fortran library routine "time"

c      character  time_str * 8
c      call time(  time_str )
c      read( time_str, 11 ) hour, minute, second
c   11 format( i2, 1x, i2, 1x, i2 )

c**********************************************************************
c**********************************************************************

c  your system's fortran time routine and any code to return
c   hour, minute, second

c**********************************************************************
c**********************************************************************

      return
      end
