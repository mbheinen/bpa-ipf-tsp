C    %W% %G%
      subroutine govsol (ign, edt, ddt2, wnew, wnow, lppwr, t)
c     solves the governor
       
      implicit none 

      include 'tspinc/params.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'
      include 'tspinc/deltfq.inc'
      include 'tspinc/spare1.inc'
      include 'tspinc/gov.inc'
      include 'tspinc/turb.inc'


c     local variables:
c     ign    -  generator number. i and j are local counting variables.
c     igv    -  govdat offset for this governor
c     mgov   -  governor type
c     mturb  -  turbine type
c     edt    -  present time step in cycles
c     dt     -  present time step in seconds
c     ddt2   -  past time step in cycles
c     pdt    -  past time step in seconds
c     wnow   -  freq deviation at converged time (off by 2pi)
c     wnew   -  freq deviation at time being calculated (off by 2pi)
c     dfreqo -  freq deviation at converged time in radians/sec
c     dfreqn -  freq deviation at time being calculated in radians/sec
c     pi     -  parameter pi
c     lppwr  -  dynamic equation iteration counter
c     x1, x2, x3p, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14 states
c     pm1    -  output of turbine (or HP output if there are two)
c     pm2    -  LP output of turbine
c     to     -  time in cycles
c     time   -  time in seconds
c     atemp  -  temporary storage
c     id     -  idatat(4).  Used if governor is cross compound.

      integer ign, i, j, mgov, mturb, igv, lppwr
      real    edt, ddt2, dt, pdt, wnow, wnew, dfreqo, dfreqn, PI,
     &        x1, x2, x3p, x3, x4, x5, x6, pm1, pm2, x7, x8, x9, 
     &        x10, x11, x12, x13, x14, t, time, atemp


      parameter (PI = 3.141592654)

c     convert time to seconds
      time = t/60.0

c     convert present and past time step into seconds
      dt  = edt/60.0
      pdt = ddt2/60.0

c     wnow and wnew are off by 2*pi.  Devide by 2*pi to fix.
      dfreqo = wnow/2.0/PI
      dfreqn = wnew/2.0/PI

      igv = govindx(ign)

      mgov = govtyp(ign)
      mturb = turbtyp(ign)

c     return if no governor on this machine
      if (mgov.eq.0) return

      if (mgov.eq.1) then
c ********************* GS governor *******************************

        if (lppwr.eq.0) then
c         system has converged, update h values

c         process first deadband
          if (govdat(igv+12).gt.1.0e-6) then
            call govdb1(govdat(igv+12), govdat(igv+13),
     &                  govdat(igv+24), dfreqo,
     &                  govdat(igv+25), x1)
          else
            x1 = dfreqo
          endif

c         process t1-t2 block
          x2 = ( govdat(igv+18) + govdat(igv+5)*x1 )  / 
     &           (govdat(igv+3)*govdat(igv+4) )

c         calculate x6 from x4 value 
          if (govmode(ign).eq.2) then
            x6 = (govdat(igv+20) + govdat(igv+23))/govdat(igv+16)
          else
            x6 = govdat(igv+21)
          endif
         
c         calculate x3p
          x3p = (govdat(igv+17) - x2 - x6)*govdat(igv+6)

c         limit gate velocity
          x3p = amax1( amin1(x3p,govdat(igv+7)) ,govdat(igv+8))

c         integrate x3p to get x3
          x3 = (govdat(igv+19) + x3p)*pdt/2.0

c         limit pmax/pmin
          x3 = amax1( amin1(x3, govdat(igv+1)), govdat(igv+2))
c         process second deadband.
          if (govdat(igv+14).gt.1.e-6) then
            call govdb2(govdat(igv+14), govdat(igv+26), x3,
     &                  govdat(igv+27), x4)
          else
            x4 = x3
          endif

c         calculate nonlinear gain
          if (govdat(igv+15).ne.0.0) then
            call cvsol(govcv(ign), govdat(igv+1), x4, x5)
          else
            x5 = x4
          endif

c         solve turbine equations and returns mech power in pm1 & pm2
          call turbsol(ign, dt, pdt, 0, x5, pm1, pm2)

c         update past values for use w/ deadband routines
          govdat(igv+24) = dfreqo
          govdat(igv+25) = x1
          govdat(igv+26) = x3
          govdat(igv+27) = x4

c         update time constant variables if time step has changed
          if (abs(dt-pdt).gt.1.0e-6) then
            govdat(igv+ 4) = (govdat(igv+ 4) - 1.0)*pdt/dt + 1.0
            govdat(igv+ 5) = (govdat(igv+ 5) - 1.0)*pdt/dt + 1.0
            govdat(igv+16) = (govdat(igv+16) - 1.0)*pdt/dt + 1.0
          endif
          
c         assign history values (only assign g20 if govmode=2)
          govdat(igv+18) =   govdat(igv+3)*(govdat(igv+4) - 2.0)*x2
     &                                    -(govdat(igv+5) - 2.0)*x1
          govdat(igv+19) = x3*2.0/dt + x3p
          if (govmode(ign).eq.2) then
            govdat(igv+20) = x6*(govdat(igv+16)-2.0) + govdat(igv+23)
          endif

c         calculate govmx as smaller of Pmax and integral of Velopen 
c         and govmin as larger of Pmin and integral of Velclose.
          govmx(ign) = amin1( govdat(igv+1), 
     &                        (govdat(igv+19) + govdat(igv+7))*dt/2.0)
          govmn(ign) = amax1( govdat(igv+2), 
     &                        (govdat(igv+19) + govdat(igv+8))*dt/2.0)
          

c         end of lppwr eq 0 case
        endif

c       start calculation of next solution

c       process first deadband
        if (govdat(igv+12).gt.1.0e-6) then
          call govdb1(govdat(igv+12), govdat(igv+13),
     &                govdat(igv+24), dfreqn, govdat(igv+25), x1)
        else
          x1 = dfreqn
        endif

c       process t1-t2 block
        x2 = ( govdat(igv+18) + govdat(igv+5)*x1 )  / 
     &         (govdat(igv+3)*govdat(igv+4) )

c       calculate x3 using transducer value if govmode.eq.2 and using
c       algebraic closure of feedback loop if govmode.eq.1
        if (govmode(ign).eq.2) then

c         calculate transducer value
          x6 = (govdat(igv+20) + genp(ign))/govdat(igv+16)

c         calculate x3p
          x3p = (govdat(igv+17) - x2 - x6)*govdat(igv+6)

c         limit gate velocity
          x3p = amax1( amin1(x3p,govdat(igv+7)) ,govdat(igv+8))

c         integrate x3p to get x3
          x3 = (govdat(igv+19) + x3p)*pdt/2.0

c         limit pmax/pmin
          x3 = amax1( amin1(x3, govdat(igv+1)), govdat(igv+2))

        else 
c         loop is algebraically closed to avoid iteration 
          x3 = (govdat(igv+19) + govdat(igv+6)*(govdat(igv+17) - x2)) 
     &        /       (2.0/dt + govdat(igv+6))

c         limit x3 using govmn/govmx (NDY!)
          x3 = amax1( amin1(x3, govmx(ign)) , govmn(ign) )

        endif

c       process second deadband.
        if (govdat(igv+14).gt.1.e-6) then
          call govdb2(govdat(igv+14), govdat(igv+26), x3,
     &                govdat(igv+27), x4)
        else
          x4 = x3
        endif

c       calculate nonlinear gain
        if (govdat(igv+15).ne.0.0) then
          call cvsol(govcv(ign), govdat(igv+1), x4, x5)
        else
          x5 = x4
        endif

c       solve turbine equations
        call turbsol(ign, dt, pdt, 1, x5, pm1, pm2)

c       store states that need to be stored
        govdat(igv+21) = x4
        govdat(igv+22) = x5
        govdat(igv+23) = genp(ign)

      else if (mgov.eq.2) then
c ********************* GH governor *******************************

        if (lppwr.eq.0) then
c         system has converged, update h values

c         retrieve stored states
          x5 = govdat(igv+24)
          x8 = govdat(igv+25)

c         process first deadband
          if (govdat(igv+12).gt.1.0e-6) then
            call govdb1(govdat(igv+12), govdat(igv+13),
     &                  govdat(igv+29), dfreqo,
     &                  govdat(igv+30), x1)
          else
            x1 = dfreqo
          endif

c         process transducer if switch is in position 2
          if (govmode(ign).eq.2) then
            x6 = (govdat(igv+23) + govdat(igv+26))/govdat(igv+16)
          else
            x6 = x5
          endif

c         process Td block
          x7 = ( govdat(igv+22) + govdat(igv+19)*x5 )/govdat(igv+6)

c         sum signals
          x2 = govdat(igv+17) - x1 - x7 - govdat(igv+3)*x6

c         process Tp block
          x3 = (govdat(igv+20) + x2)/(govdat(igv+4)*govdat(igv+5))

c         limit output of block
          x3 = amax1( amin1(x3, govdat(igv+7)), govdat(igv+8))

c         process integrator
          x4 = (govdat(igv+21) + x3)*pdt/2.0

c         limit output of integrator
          x4 = amax1( amin1(x4, govdat(igv+1)), govdat(igv+2))

c         process second deadband.
          if (govdat(igv+14).gt.1.e-6) then
            call govdb2(govdat(igv+14), govdat(igv+31), x4,
     &                  govdat(igv+32), x5)
          else
            x5 = x4
          endif

c         calculate nonlinear gain
          if (govdat(igv+15).ne.0.0) then
            call cvsol(govcv(ign), govdat(igv+1), x5, x8)
          else
            x8 = x5
          endif

c         solve turbine equations and return mech power in pm1 & pm2
          call turbsol(ign, dt, pdt, 0, x8, pm1, pm2)

c         update past values for use w/ deadband routines
          govdat(igv+29) = dfreqo
          govdat(igv+30) = x1
          govdat(igv+31) = x4
          govdat(igv+32) = x5

c         update time constant variables if time step has changed
          if (abs(dt-pdt).gt.1.0e-6) then
            govdat(igv+ 5) = (govdat(igv+ 5) - 1.0)*pdt/dt + 1.0
            govdat(igv+ 6) = (govdat(igv+ 6) - 1.0)*pdt/dt + 1.0
            govdat(igv+16) = (govdat(igv+16) - 1.0)*pdt/dt + 1.0
            govdat(igv+19) =  govdat(igv+19)*pdt/dt
          endif
          
c         assign history values (only assign g23 if govmode=2)
          govdat(igv+20) = govdat(igv+4)*(govdat(igv+5)-2.0)*x3 + x2
          govdat(igv+21) = x4*2.0/dt + x3
          govdat(igv+22) = (govdat(igv+6)-2.0)*x7 - govdat(igv+19)*x5
          if (govmode(ign).eq.2) then
            govdat(igv+23) = x6*(govdat(igv+16)-2.0) + govdat(igv+26)
          endif

c         calculate govmx as smaller of Pmax and integral of Velopen 
c         and govmin as larger of Pmin and integral of Velclose.
          govmx(ign) = amin1( govdat(igv+1), 
     &                        (govdat(igv+21) + govdat(igv+7))*dt/2.0)
          govmn(ign) = amax1( govdat(igv+2), 
     &                        (govdat(igv+21) + govdat(igv+8))*dt/2.0)

c         calculate linear coefficients to close loop (x5 = A1*wnew + A2)
          if (govmode(ign).eq.2) then 
            govdat(igv+27) = - 1.0/(govdat(igv+4)*govdat(igv+5)*2.0/dt   
     &                            + govdat(igv+19)/govdat(igv+6))

            govdat(igv+28) = -( govdat(igv+21)*govdat(igv+4) 
     &                        * govdat(igv+5) + govdat(igv+20)   
     &                        - govdat(igv+3)*x6 + govdat(igv+17)
     &                        - govdat(igv+22)/govdat(igv+6) )
     &                        * govdat(igv+27)
          else
            govdat(igv+27) = - 1.0/(govdat(igv+4)*govdat(igv+5)*2.0/dt   
     &                            + govdat(igv+19)/govdat(igv+6)
     &                            + govdat(igv+3))

            govdat(igv+28) = -( govdat(igv+21)*govdat(igv+4) 
     &                        * govdat(igv+5) + govdat(igv+20)   
     &                        + govdat(igv+17)   
     &                        - govdat(igv+22)/govdat(igv+6) )
     &                        * govdat(igv+27)
          endif
                               

c         end of lppwr eq 0 case
        endif

c       start calculation of next solution

c       process first deadband
        if (govdat(igv+12).gt.1.0e-6) then
          call govdb1(govdat(igv+12), govdat(igv+13),
     &                govdat(igv+29), dfreqo,
     &                govdat(igv+30), x1)
        else
          x1 = dfreqn
        endif

c       solve algebraically closed loop
        x4 = govdat(igv+27)*x1 + govdat(igv+28)

c       limit x5 using govmn/govmx (NDY!)
        x4 = amax1( amin1(x4, govmx(ign)) , govmn(ign) )

c       process second deadband.
        if (govdat(igv+14).gt.1.e-6) then
          call govdb2(govdat(igv+14), govdat(igv+31), x4,
     &                govdat(igv+32), x5)
        else
          x5 = x4
        endif

c       calculate nonlinear gain
        if (govdat(igv+15).ne.0.0) then
          call cvsol(govcv(ign), govdat(igv+1), x5, x8)
        else
          x8 = x5
        endif

c       solve turbine equations
        call turbsol(ign, dt, pdt, 1, x8, pm1, pm2)

c       store states that need to be stored
        govdat(igv+24) = x5
        govdat(igv+25) = x8
        govdat(igv+26) = genp(ign)

      else if (mgov.eq.3) then
c ********************* GP governor *******************************

        if (lppwr.eq.0) then
c         system has converged, update h values

c         retrieve stored states
          x6  = govdat(igv+32)
          x11 = govdat(igv+33)
          x12 = govdat(igv+34)

c         process first deadband
          if (govdat(igv+12).gt.1.0e-6) then
            call govdb1(govdat(igv+12), govdat(igv+13),
     &                  govdat(igv+36), dfreqo,
     &                  govdat(igv+37), x1)
          else
            x1 = dfreqo
          endif

c         process transducer if switch is in position 2
          if (govmode(ign).eq.2) then
            x7 = (govdat(igv+35) + govdat(igv+26))/govdat(igv+16)
          else
            x7 = x6
          endif

c         sum signals
          x2 = govdat(igv+17) - x1 - govdat(igv+3)*x7

c         process Td block
          x3 = (govdat(igv+22) + x2)/govdat(igv+4)

c         process PID
          x4 = (govdat(igv+23) + govdat(igv+20)*x3)*pdt/2.0
          x5 = (govdat(igv+24) + govdat(igv+19)*2./pdt*x3)/govdat(igv+5)
          x6 = govdat(igv+18)*x3 + x4 + x5

c         sum signals
          x8 = x6 - x11

c         process Kg block
          x9 = (govdat(igv+25) + govdat(igv+21)*x8)/govdat(igv+6)

c         limit output
          x9 = amax1( amin1(x9, govdat(igv+7)), govdat(igv+8))

c         process integrator block
          x10 = (govdat(igv+27) + x9)*pdt/2.0

c         limit output of integrator
          x10 = amax1( amin1(x10, govdat(igv+1)), govdat(igv+2))

c         process second deadband.
          if (govdat(igv+14).gt.1.e-6) then
            call govdb2(govdat(igv+14), govdat(igv+38), x10,
     &                  govdat(igv+39), x11)
          else
            x11 = x10
          endif

c         calculate nonlinear gain
          if (govdat(igv+15).ne.0.0) then
            call cvsol(govcv(ign), govdat(igv+1), x11, x12)
          else
            x12 = x11
          endif

c         solve turbine equations and returns mech power in pm1 & pm2
          call turbsol(ign, dt, pdt, 0, x12, pm1, pm2)
          
csw temp write 
        if (ign.eq.42) then
          write(14,1000) time, x11, x12
1000      format(8f10.7)
        endif 

c         update past values for use w/ deadband routines
          govdat(igv+36) = dfreqo
          govdat(igv+37) = x1
          govdat(igv+38) = x10
          govdat(igv+39) = x11

c         update time constant variables if time step has changed
          if (abs(dt-pdt).gt.1.0e-6) then
            govdat(igv+ 4) = (govdat(igv+ 4) - 1.0)*pdt/dt + 1.0
            govdat(igv+ 5) = (govdat(igv+ 5) - 1.0)*pdt/dt + 1.0
            govdat(igv+ 6) = (govdat(igv+ 6) - 1.0)*pdt/dt + 1.0
            govdat(igv+16) = (govdat(igv+16) - 1.0)*pdt/dt + 1.0
          endif
          

c         assign history values (only assign g35 if govmode=2)
          govdat(igv+22) = x2 + x3*(govdat(igv+4)-2.0)
          govdat(igv+23) = x3*govdat(igv+20) + x4*2.0/dt
          govdat(igv+24) = x5*(govdat(igv+5)-2.)-x3*2.*govdat(igv+19)/dt
          govdat(igv+25) = x8*govdat(igv+21) + x9*(govdat(igv+6)-2.)
          govdat(igv+27) = x9 + x10*2.0/dt
          if (govmode(ign).eq.2) then
            govdat(igv+35) = x7*(govdat(igv+16)-2.0) + govdat(igv+26)
          endif

c         calculate govmx as smaller of Pmax and integral of Velopen 
c         and govmin as larger of Pmin and integral of Velclose.
          govmx(ign) = amin1( govdat(igv+1), 
     &                        (govdat(igv+27) + govdat(igv+7))*dt/2.0)
          govmn(ign) = amax1( govdat(igv+2), 
     &                        (govdat(igv+27) + govdat(igv+8))*dt/2.0)


c         calculate linear coefficients to close loops 
c                 x6  = a1*x1 + a2
c                 x11 = a3*x6 + a4
      

          atemp =   2.*govdat(igv+19)/(govdat(igv+5)*dt) 
     &            + govdat(igv+20)*dt/2. + govdat(igv+18)
          if (govmode(ign).eq.2) then 
            govdat(igv+28) = - atemp/govdat(igv+4)
            govdat(igv+29) =   govdat(igv+23)*dt/2.   
     &                       + govdat(igv+24)/govdat(igv+5)
     &                       + atemp*( govdat(igv+22) + govdat(igv+17) 
     &                                 - govdat(igv+3)/govdat(igv+16) 
     &                                   *(govdat(igv+35)+genp(ign)) 
     &                               )/govdat(igv+4)
          else
            govdat(igv+28) = - atemp
     &                         /(govdat(igv+4) + govdat(igv+3)*atemp)
            govdat(igv+29) = (( govdat(igv+23)*dt/2. 
     &                          + govdat(igv+24)/govdat(igv+5) 
     &                        )*govdat(igv+4)
     &                        + atemp*(govdat(igv+22) + govdat(igv+17))
     &                       )/(govdat(igv+4) + govdat(igv+3)*atemp)
          endif

          atemp = 1./( 2.*govdat(igv+6) + govdat(igv+21)*dt )
          govdat(igv+30) = govdat(igv+21)*dt*atemp   
          govdat(igv+31) = (  govdat(igv+27)*govdat(igv+6) 
     &                      + govdat(igv+25)
     &                     )*dt*atemp 

c         end of lppwr eq 0 case
        endif

c       start calculation of next solution

c       process first deadband
        if (govdat(igv+12).gt.1.0e-6) then
          call govdb1(govdat(igv+12), govdat(igv+13),
     &                govdat(igv+36), dfreqn,
     &                govdat(igv+37), x1)
        else
          x1 = dfreqn
        endif

c       solve first loop
        x6 = govdat(igv+28)*x1 + govdat(igv+29)

c       solve second loop
        x10 = govdat(igv+30)*x6 + govdat(igv+31)

c       limit x6 using govmn/govmx
        x10 = amax1( amin1(x10, govmx(ign)) , govmn(ign) )

c       process second deadband.
        if (govdat(igv+14).gt.1.e-6) then
          call govdb2(govdat(igv+14), govdat(igv+38), x10,
     &                govdat(igv+39), x11)
        else
          x11 = x10
        endif

c       calculate nonlinear gain
        if (govdat(igv+15).ne.0.0) then
          call cvsol(govcv(ign), govdat(igv+1), x11, x12)
        else
          x12 = x11
        endif

c       solve turbine equations
        call turbsol(ign, dt, pdt, 1, x12, pm1, pm2)

c       store states that need to be stored
        govdat(igv+32) = x6
        govdat(igv+33) = x11
        govdat(igv+34) = x12
        govdat(igv+26) = genp(ign)

      else if (mgov.eq.4) then
c ********************* G2 governor *******************************

        if (lppwr.eq.0) then
c         system has converged, update h values

c         retrieve stored states
          x8  = govdat(igv+35)
          x13 = govdat(igv+36)
          x14 = govdat(igv+37)

c         process first deadband
          if (govdat(igv+12).gt.1.0e-6) then
            call govdb1(govdat(igv+12), govdat(igv+13),
     &                  govdat(igv+39), dfreqo,
     &                  govdat(igv+40), x1)
          else
            x1 = dfreqo
          endif

c         process Td block
          x2 = (govdat(igv+22) + x1)/govdat(igv+4)

c         process double derivitive blocks
          x3 = (govdat(igv+23) + 2./pdt*govdat(igv+18)*x2)/govdat(igv+5)
          x4 = (govdat(igv+24) + 2./pdt*govdat(igv+19)*x2)/govdat(igv+5)
          x5 = (govdat(igv+25) + 2./pdt*x4)/govdat(igv+5)

c         sum signals
          x6 = x2 + x3 + x5

c         process transducer if switch is in position 2
          if (govmode(ign).eq.2) then
            x9 = (govdat(igv+38) + govdat(igv+26))/govdat(igv+16)
          else
            x9 = x8
          endif

c         sum signals
          x7 = govdat(igv+17) - x6 - govdat(igv+3)*x9 

c         process Ki block
          x8 = (govdat(igv+27) + govdat(igv+20)*x7)*pdt/2.0

c         sum signals
          x10 = x8 - x13

c         process Kg block and limit output
          x11 = (govdat(igv+28) + govdat(igv+21)*x10)/govdat(igv+6)
          x11 = amax1( amin1(x11, govdat(igv+7)), govdat(igv+8))

c         process integrator block and limit output 
          x12 = (govdat(igv+29) + x11)*pdt/2.0
          x12 = amax1( amin1(x12, govdat(igv+1)), govdat(igv+2))

c         process second deadband.
          if (govdat(igv+14).gt.1.e-6) then
            call govdb2(govdat(igv+14), govdat(igv+41), x12,
     &                  govdat(igv+42), x13)
          else
            x13 = x12
          endif

c         calculate nonlinear gain
          if (govdat(igv+15).ne.0.0) then
            call cvsol(govcv(ign), govdat(igv+1), x13, x14)
          else
            x14 = x13
          endif

c         solve turbine equations and returns mech power in pm1 & pm2
          call turbsol(ign, dt, pdt, 0, x14, pm1, pm2)

c         update past values for use w/ deadband routines
          govdat(igv+39) = dfreqo
          govdat(igv+40) = x1
          govdat(igv+41) = x12
          govdat(igv+42) = x13




c         update time constant variables if time step has changed
          if (abs(dt-pdt).gt.1.0e-6) then
            govdat(igv+ 4) = (govdat(igv+ 4) - 1.0)*pdt/dt + 1.0
            govdat(igv+ 5) = (govdat(igv+ 5) - 1.0)*pdt/dt + 1.0
            govdat(igv+ 6) = (govdat(igv+ 6) - 1.0)*pdt/dt + 1.0
            govdat(igv+16) = (govdat(igv+16) - 1.0)*pdt/dt + 1.0
          endif

c         assign history values (only assign g38 if govmode=2)
          govdat(igv+22) = x2*(govdat(igv+4)-2.0) + x1
          govdat(igv+23) = x3*(govdat(igv+5)-2.0)  
     &                     - 2.0/dt*govdat(igv+18)*x2
          govdat(igv+24) = x4*(govdat(igv+5)-2.0)
     &                     - 2.0/dt*govdat(igv+19)*x2
          govdat(igv+25) = x5*(govdat(igv+5)-2.0)
     &                     - 2.0/dt*x4
          govdat(igv+27) = 2.0/dt*x8 + govdat(igv+20)*x7
          govdat(igv+28) = x11*(govdat(igv+6)-2.0) + govdat(igv+21)*x10
          govdat(igv+29) = 2.0/dt*x12 + x11
          if (govmode(ign).eq.2) then
            govdat(igv+38) = x9*(govdat(igv+16)-2.0) + govdat(igv+26)
          endif

c         calculate govmx as smaller of Pmax and integral of Velopen 
c         and govmin as larger of Pmin and integral of Velclose.
          govmx(ign) = amin1( govdat(igv+1),
     &                        (govdat(igv+29) + govdat(igv+7))*dt/2.0)
          govmn(ign) = amax1( govdat(igv+2),
     &                        (govdat(igv+29) + govdat(igv+8))*dt/2.0)

c         calculate linear coefficients to close loops (NDY)
          if (govmode(ign).eq.2) then
            govdat(igv+30) =  - govdat(igv+20)*dt/2.0
            govdat(igv+31) =    govdat(igv+27)*dt/2.0 
     &                        + govdat(igv+20)*dt/2.0
     &                          *(govdat(igv+17) - 
     &                            govdat(igv+3)/govdat(igv+16)
     &                            *(govdat(igv+38) + genp(ign)))
          else
            atemp = 1.0/(2.0/dt + govdat(igv+20)*govdat(igv+3))
            govdat(igv+30) = - govdat(igv+20)*atemp
            govdat(igv+31) =   (govdat(igv+20)*govdat(igv+17)
     &                          + govdat(igv+27) )*atemp
          end if
          atemp = 1.0/(2.0/dt*govdat(igv+6) + govdat(igv+21))
          govdat(igv+32) = govdat(igv+21)*atemp
          govdat(igv+33) = (govdat(igv+29)*govdat(igv+6) 
     &                      + govdat(igv+28))*atemp


c         end of lppwr eq 0 case
        endif

c       start calculation of next solution

c       process first deadband
        if (govdat(igv+12).gt.1.0e-6) then
          call govdb1(govdat(igv+12), govdat(igv+13),
     &                govdat(igv+39), dfreqn,
     &                govdat(igv+40), x1)
        else
          x1 = dfreqn
        endif

c       process Td block
        x2 = (govdat(igv+22) + x1)/govdat(igv+4)

c       process double derivitive blocks
        x3 = (govdat(igv+23) + 2./pdt*govdat(igv+18)*x2)/govdat(igv+5)
        x4 = (govdat(igv+24) + 2./pdt*govdat(igv+19)*x2)/govdat(igv+5)
        x5 = (govdat(igv+25) + 2./pdt*x4)/govdat(igv+5)

c       sum signals
        x6 = x2 + x3 + x5

c       solve first loop
        x8 = govdat(igv+30)*x6 + govdat(igv+31)

c       solve second loop
        x12 = govdat(igv+32)*x8 + govdat(igv+33)

c       limit x12 using govmn/govmx
        x12 = amax1( amin1(x12, govmx(ign)) , govmn(ign) )

c       process second deadband.
        if (govdat(igv+14).gt.1.e-6) then
          call govdb2(govdat(igv+14), govdat(igv+41), x12,
     &                govdat(igv+42), x13)
        else
          x13 = x12
        endif

c       calculate nonlinear gain
        if (govdat(igv+15).ne.0.0) then
          call cvsol(govcv(ign), govdat(igv+1), x13, x14)
          else
            x14 = x13
        endif

c       solve turbine equations
        call turbsol(ign, dt, pdt, 1, x14, pm1, pm2)

c       store states that need to be stored
        govdat(igv+35) = x8
        govdat(igv+36) = x13
        govdat(igv+37) = x14
        govdat(igv+26) = genp(ign)

      else if (mgov.eq.5) then
c ********************* GW governor *******************************

        if (lppwr.eq.0) then

c         calculate previous states
          x1 =  dfreqo/govdat(igv+3)
          x2 = (govdat(igv+8) + x1*govdat(igv+5))/govdat(igv+4)
          x3 = (govdat(igv+9) + x2)/govdat(igv+6)
          x4 =  govdat(igv+7) - x3

c         limit x4
          x4 = amax1(amin1(x4, govdat(igv+1)),govdat(igv+2))

c         solve turbine equations
          call turbsol(ign, dt, pdt, 0, x4, pm1, pm2)


c         update time constant variables if time step has changed
          if (abs(dt-pdt).gt.1.0e-6) then
            govdat(igv+4) = (govdat(igv+4)-1.0)*pdt/dt + 1.0
            govdat(igv+5) = (govdat(igv+5)-1.0)*pdt/dt + 1.0
            govdat(igv+6) = (govdat(igv+6)-1.0)*pdt/dt + 1.0
          endif

c         assign history values
          govdat(igv+8) = x2*(govdat(igv+4)-2.0) 
     &                  - x1*(govdat(igv+5)-2.0)
          govdat(igv+9) = x3*(govdat(igv+6)-2.0) + x2

c         end of lppwr.eq.0 case
        endif

c       calculate states
        x1 =  dfreqn/govdat(igv+3)
        x2 = (govdat(igv+8) + x1*govdat(igv+5))/govdat(igv+4)
        x3 = (govdat(igv+9) + x2)/govdat(igv+6)
        x4 =  govdat(igv+7) - x3

c       limit x4
        x4 = amax1(amin1(x4, govdat(igv+1)),govdat(igv+2))

c       solve turbine equations
        call turbsol(ign, dt, pdt, 1, x4, pm1, pm2)

      endif

c     assign turbine output
      govpwr(ign) = pm1

c     assign low power turbine output for cross compound turbines
      if (mturb .ge. 4 .and. mturb .lt. 8) then
        govpwr(govlpi(ign)) = pm2
      endif


      return
      end
