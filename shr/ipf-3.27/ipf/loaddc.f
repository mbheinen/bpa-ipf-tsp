C    @(#)loaddc.f	20.3 2/13/96
      subroutine loaddc (iver,histin,rcflag,jtape,ldflow,stab)
      integer iver, histin, jtape
      logical stab, rcflag, ldflow
      
*     this is a dummy routine called from RDDTAI.FOR for EPRI dc
      logical done
      done = .true.

      if (done)  stop 'LOADDC FOR EPRI'

      return
      end
