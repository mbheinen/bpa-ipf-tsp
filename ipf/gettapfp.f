C    @(#)gettapfp.f	20.1 1/4/99
C****************************************************************
C
C        File: gettapfp.f (GeT tapfp)
C
C        Purpose: Routine to deterimine tapfp such that tstart is
C                 a discrete tap.  
C
C        Input parameters:
C
C             tstart:   Starting primary tap
C             tmin:     Minimum Per unit primary tap 
C             tmax:     Maximum Per unit primary tap 
C             numtaps:  Number of taps
C
C        Output parameters:
c
C             numtaps:  Number of taps (if original value was zero)
C             stepsize: Step size Per Unit (with f12.8 resolution)
C             tappf:    Ancilliary tap to assure tstart is discrete
C
C        Author: Walt Powell  Date: 21 May 1996
C        Called by: ext_gel.f
C
C****************************************************************
	integer function gettapfp (tstart, tmin, tmax, numtaps, 
     &                             stepsize, tapfp)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/prt.inc'

        character tempc*12

        gettapfp = 0

        if (numtaps .gt. 1) then
          stepsize = (tmax - tmin) / (float(numtaps) - 1.0)
        else
          stepsize = 0.00025
          steptot = (tmax - tmin) / stepsize
          numtaps = steptot + 1.5
        endif
        write (tempc, fmt='(f12.6)') stepsize
        stepsize = ftn_atof (tempc)

        tmax = tmin + (float(numtaps) - 1.0) * stepsize + 0.00001
c
c       Test if tstart is at a discrete tap
c
        step = (tstart - tmin) / stepsize
        if (step .ge. 0.0) then
          istep = step + 0.99
        else
          istep = step - 0.99
        endif
        tapfp = tmin + float (istep) * stepsize
        return
        end
 
