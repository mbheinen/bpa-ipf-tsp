C    @(#)n_date.f	20.3 2/13/96
      subroutine n_date( month, day, year )
      integer month, day, year

C   THIS IS THE "ROOT LEVEL" ROUTINE THAT SHOULD BE CHANGED
C   IF YOU DON'T HAVE A "C" COMPILER AND YOUR SYSTEM SUPPORTS
C   A FORTRAN CALLABLE DATE ROUTINE.

C   THIS ROUTINE GETS THE CURRENT DATE FROM THE SYSTEM IN
C   INTEGER FORMAT

C**********************************************************************
C***  USE ONE OF THE FOLLOWING - COMMENT OUT THE OTHERS  **************
C**********************************************************************

C**********************************************************************
C**********************************************************************
C   THE FOLLOWING CODE IS THE STANDARD PORTABLE CODE THAT CALLS
C   A "C" ROUTINE THAT CALLS A STANDARD "C" LIBRARY ROUTINE.

      integer tm(9)
C *** CALTME IS A "C" ROUTINE THAT IS PORTABLE
      call caltme( tm )
      day   = tm(4)
      month = tm(5) + 1
      year  = mod( tm(6), 100 )

C**********************************************************************
C**********************************************************************
C   VAX/VMS VERSION THAT AVOIDS CALLS TO "C" USING THE VMS
C   FORTRAN LIBRARY ROUTINE "IDATE"

C      CALL IDATE(  MONTH, DAY, YEAR )

C**********************************************************************
C**********************************************************************

C  YOUR SYSTEM'S FORTRAN DATE ROUTINE AND ANY CODE TO RETURN
C   MONTH, DAY, YEAR

C**********************************************************************
C**********************************************************************

      return
      end
