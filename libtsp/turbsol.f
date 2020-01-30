C    %W% %G%
      subroutine turbsol(ign, dt, pdt, lppwr, pgv, pm1, pm2)
c     solves turbine equations

      implicit none

      include 'tspinc/params.inc'
      include 'tspinc/turb.inc'

c     local variables
c     ign       -  generator number
c     itb       -  index to turbdat array
c     TURBSLOTS -  parameter from params.inc
c     dt        -  time step (in seconds)
c     pdt       -  past time step (in seconds)
c     lppwr     -  iteration counter
c     mturb     -  index to turbine type
c     pgv       -  output of governor
c     pm1       -  output of turbine (high pressure)
c     pm2       -  output of turbine (low pressure)

      real dt, pdt, pgv, pm1, pm2, x1, x2, x3, x4, x5, x6, x7, x8, x9
      integer ign, itb, mturb, lppwr

      itb = turbindx(ign)

      mturb = turbtyp(ign)

      if (mturb.eq.1) then
c ********************* TA turbine *******************************

c       assign states
        x2 = (turbdat(itb+2) + pgv)/turbdat(itb+1)

        if (lppwr.eq.0) then

          if (abs(dt-pdt).gt.1.0e-6) then
c           update time constant because time step changed
            turbdat(itb+1) = (turbdat(itb+1) - 1.0)*pdt/dt + 1.0
          endif

c         update history value
          turbdat(itb+2) =  x2*(turbdat(itb+1)-2.0) + pgv

        endif

        pm1 = x2
        pm2 = 0.0

      else if (mturb.eq.2) then
c ********************* TB turbine *******************************

c       assign states
        x2 = (turbdat(itb+4) + pgv)/turbdat(itb+1)
        x3 = (turbdat(itb+5) +  x2)/turbdat(itb+2)
        x4 = (turbdat(itb+6) +  x3)/turbdat(itb+3)

        if (lppwr.eq.0) then

          if (abs(dt-pdt).gt.1.0e-6) then
c           update time constant because time step changed
            turbdat(itb+1) = (turbdat(itb+1) - 1.0)*pdt/dt + 1.0
            turbdat(itb+2) = (turbdat(itb+2) - 1.0)*pdt/dt + 1.0
            turbdat(itb+3) = (turbdat(itb+3) - 1.0)*pdt/dt + 1.0
          endif

c         update history value
          turbdat(itb+4) =  x2*(turbdat(itb+1)-2.0) + pgv
          turbdat(itb+5) =  x3*(turbdat(itb+2)-2.0) + x2
          turbdat(itb+6) =  x4*(turbdat(itb+3)-2.0) + x3

        endif

        pm1 = turbdat(itb+7)*x2 + turbdat(itb+8)*x3 + turbdat(itb+9)*x4
        pm2 = 0.0

      else if (mturb.eq.3) then
c ********************* TC turbine *******************************
 
c       assign states
        x2 = (turbdat(itb+5) + pgv)/turbdat(itb+1)
        x3 = (turbdat(itb+6) +  x2)/turbdat(itb+2)
        x4 = (turbdat(itb+7) +  x3)/turbdat(itb+3)
        x5 = (turbdat(itb+8) +  x4)/turbdat(itb+4)

        if (lppwr.eq.0) then

          if (abs(dt-pdt).gt.1.0e-6) then
c           update time constant because time step changed
            turbdat(itb+1) = (turbdat(itb+1) - 1.0)*pdt/dt + 1.0
            turbdat(itb+2) = (turbdat(itb+2) - 1.0)*pdt/dt + 1.0
            turbdat(itb+3) = (turbdat(itb+3) - 1.0)*pdt/dt + 1.0
            turbdat(itb+4) = (turbdat(itb+4) - 1.0)*pdt/dt + 1.0
          endif

c         update history value
          turbdat(itb+5) =  x2*(turbdat(itb+1)-2.0) + pgv
          turbdat(itb+6) =  x3*(turbdat(itb+2)-2.0) + x2
          turbdat(itb+7) =  x4*(turbdat(itb+3)-2.0) + x3
          turbdat(itb+8) =  x5*(turbdat(itb+4)-2.0) + x4
        endif

        pm1 =   turbdat(itb+ 9)*x2 + turbdat(itb+10)*x3 
     &        + turbdat(itb+11)*x4 + turbdat(itb+12)*x5
        pm2 = 0.0

      else if (mturb.eq.4) then
c ********************* TD turbine *******************************

c       assign states
        x2 = (turbdat(itb+4) + pgv)/turbdat(itb+1)
        x3 = (turbdat(itb+5) +  x2)/turbdat(itb+2)
        x4 = (turbdat(itb+6) +  x3)/turbdat(itb+3)

        if (lppwr.eq.0) then

          if (abs(dt-pdt).gt.1.0e-6) then
c           update time constant because time step changed
            turbdat(itb+1) = (turbdat(itb+1) - 1.0)*pdt/dt + 1.0
            turbdat(itb+2) = (turbdat(itb+2) - 1.0)*pdt/dt + 1.0
            turbdat(itb+3) = (turbdat(itb+3) - 1.0)*pdt/dt + 1.0
          endif

c         update history value
          turbdat(itb+4) =  x2*(turbdat(itb+1)-2.0) + pgv
          turbdat(itb+5) =  x3*(turbdat(itb+2)-2.0) + x2
          turbdat(itb+6) =  x4*(turbdat(itb+3)-2.0) + x3
        endif

        pm1 = turbdat(itb+7)*x2 + turbdat(itb+ 9)*x4
        pm2 = turbdat(itb+8)*x3 + turbdat(itb+10)*x4


      else if (mturb.eq.5) then
c ********************* TE turbine *******************************

c       assign states

        x2 = (turbdat(itb+4) + pgv)/turbdat(itb+1)
        x3 = (turbdat(itb+5) +  x2)/turbdat(itb+2)
        x4 = (turbdat(itb+6) +  x3)/turbdat(itb+3)

        if (lppwr.eq.0) then

          if (abs(dt-pdt).gt.1.0e-6) then
c           update time constant because time step changed
            turbdat(itb+1) = (turbdat(itb+1) - 1.0)*pdt/dt + 1.0
            turbdat(itb+2) = (turbdat(itb+2) - 1.0)*pdt/dt + 1.0
            turbdat(itb+3) = (turbdat(itb+3) - 1.0)*pdt/dt + 1.0
          endif

c         update history value
          turbdat(itb+4) =  x2*(turbdat(itb+1)-2.0) + pgv
          turbdat(itb+5) =  x3*(turbdat(itb+2)-2.0) + x2
          turbdat(itb+6) =  x4*(turbdat(itb+3)-2.0) + x3

        endif

c       assign pm1 & pm2
        pm1 = turbdat(itb+7)*x2 + turbdat(itb+8)*x3
        pm2 = turbdat(itb+9)*x4

      else if (mturb.eq.6) then
c ********************* TF turbine *******************************

c       assign states
        x2 = (turbdat(itb+5) + pgv)/turbdat(itb+1)
        x3 = (turbdat(itb+6) +  x2)/turbdat(itb+2)
        x4 = (turbdat(itb+7) +  x3)/turbdat(itb+3)
        x5 = (turbdat(itb+8) +  x4)/turbdat(itb+4)

        if (lppwr.eq.0) then

          if (abs(dt-pdt).gt.1.0e-6) then
c           update time constant because time step changed
            turbdat(itb+1) = (turbdat(itb+1) - 1.0)*pdt/dt + 1.0
            turbdat(itb+2) = (turbdat(itb+2) - 1.0)*pdt/dt + 1.0
            turbdat(itb+3) = (turbdat(itb+3) - 1.0)*pdt/dt + 1.0
            turbdat(itb+4) = (turbdat(itb+4) - 1.0)*pdt/dt + 1.0

          endif

c         update history value
          turbdat(itb+5) =  x2*(turbdat(itb+1)-2.0) + pgv
          turbdat(itb+6) =  x3*(turbdat(itb+2)-2.0) + x2
          turbdat(itb+7) =  x4*(turbdat(itb+3)-2.0) + x3
          turbdat(itb+8) =  x5*(turbdat(itb+4)-2.0) + x4

        endif

c       assign pm1 & pm2
        pm1 =   turbdat(itb+ 9)*x2 + turbdat(itb+11)*x4
     &        + turbdat(itb+13)*x5
        pm2 =   turbdat(itb+10)*x3 + turbdat(itb+12)*x4
     &        + turbdat(itb+14)*x5

      else if (mturb.eq.7) then
c ********************* TG turbine *******************************

c       assign states
        x2 = (turbdat(itb+5) + pgv)/turbdat(itb+1)
        x3 = (turbdat(itb+6) +  x2)/turbdat(itb+2)
        x4 = (turbdat(itb+7) +  x3)/turbdat(itb+3)
        x5 = (turbdat(itb+8) +  x4)/turbdat(itb+4)

        if (lppwr.eq.0) then

          if (abs(dt-pdt).gt.1.0e-6) then
c           update time constant because time step changed
            turbdat(itb+1) = (turbdat(itb+1) - 1.0)*pdt/dt + 1.0
            turbdat(itb+2) = (turbdat(itb+2) - 1.0)*pdt/dt + 1.0
            turbdat(itb+3) = (turbdat(itb+3) - 1.0)*pdt/dt + 1.0
            turbdat(itb+4) = (turbdat(itb+4) - 1.0)*pdt/dt + 1.0

          endif

c         update history value
          turbdat(itb+5) =  x2*(turbdat(itb+1)-2.0) + pgv
          turbdat(itb+6) =  x3*(turbdat(itb+2)-2.0) + x2
          turbdat(itb+7) =  x4*(turbdat(itb+3)-2.0) + x3
          turbdat(itb+8) =  x5*(turbdat(itb+4)-2.0) + x4

        endif

c       assign pm1 & pm2
        pm1 =   turbdat(itb+ 9)*x2 + turbdat(itb+11)*x3
     &        + turbdat(itb+13)*x4 + turbdat(itb+15)*x5
        pm2 =   turbdat(itb+10)*x2 + turbdat(itb+12)*x3
     &        + turbdat(itb+14)*x4 + turbdat(itb+16)*x5

      else if (mturb.eq.8) then
c ********************* TW turbine *******************************
    
c       assign states
        x1 = pgv
        x2 = (turbdat(itb+2) + x1)/turbdat(itb+1)

        if (lppwr.eq.0) then

          if (abs(dt-pdt).gt.1.0e-6) then
c           update time constant because time step changed
            turbdat(itb+1) = (turbdat(itb+1) - 1.0)*pdt/dt + 1.0
          endif

c         update history value
          turbdat(itb+2) =  x2*(turbdat(itb+1)-2.0) + pgv

        endif

        pm1 = 3.0*x2 - 2.0*x1
        pm2 = 0.0

c       assign pm1 & pm2

      endif

      return
      end 
