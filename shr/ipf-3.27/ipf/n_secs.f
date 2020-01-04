C    @(#)n_secs.f	20.3 2/13/96
      integer function n_secs( s0 )
      integer s0

c     This routine returns a delta value. To get time, in seconds, since
c     midnight, pass in a value of zero.

c     To time a section of code do the following:
c        isec_init = n_secs( 0 )
c           ...   code to be timed
c        isec_used = n_secs( isec_init )

c     The delta time logic works even if the delta is over midnight,
c     however, the interval must be less that 24 hours.

c     Daylight savings time changes are not accounted for, so if you 
c     time during the interval that time is being changed, your time 
c     will be way off.

      integer  h, m, s, s1

      call n_time( h, m, s )

      s1 = h * 3600  +  m * 60  +  s
      if( s1 .ge. s0 ) then
         n_secs = s1-s0
      else
         n_secs = s1  +  (24*3600 - s0)
      endif

      return
      end
