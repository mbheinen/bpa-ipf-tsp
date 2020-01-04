C    @(#)timoda.f	20.3 2/13/96
      subroutine timoda(time_str)
      character  time_str * (*)
 
c     return time of day in   hh:mm:ss   character format
 
      integer  hour, minute, second
      call n_time( hour, minute, second )
      write( time_str, 11 ) hour, minute, second
   11 format( i2, ':', i2, ':', i2 )
      return
      end
