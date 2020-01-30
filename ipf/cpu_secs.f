C    @(#)cpu_secs.f	20.3 2/13/96
      real function cpu_secs( cput0 )
      real cput0

c     This routine returns a delta value, to get cpu time in seconds 
c     from the beginning of the process, pass in a value of 0.0

c     To time a section of code do the following:
c        cpu_init = cpu_secs( 0.0 )
c           ...   code to be timed
c        cpu_used = cpu_secs( cpu_init )

c     This is a base level routine that calls a "C" routine for 
c     portability. If you do not have a "C" compiler and your system 
c     supports a FORTRAN call to get the cpu seconds, replace the 
c     following code with your system's equivalent call.

      real cput1

c*** this is the "C" routine
      call cputmr( cput1 )
c***********************************

      cpu_secs = cput1 - cput0

      return
      end
