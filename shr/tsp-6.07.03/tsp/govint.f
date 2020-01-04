C    %W% %G%
      subroutine govint(ign, edt, iabort)
c     this routine initializes the governors

      implicit none

      include 'tspinc/params.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/gov.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/spare1.inc'

c     local variables
c     ign    -  generator number. i and j are local counting variables.
c     mgov   -  governor type, igv is the govdat offset
c     gvpwr  -  power out of governor (govpwr contains PM1 for cc govs)  
c     iabort -  flag to abort run
c     x1, x2, x3p, x3 -> x14       states


C     Local variables

      integer ign, i, j, mgov, igv, iabort
      real    edt, dt, x1, x2, x3p, x3, x4, x5, x6, x7, x8,
     &        x9, x10, x11, x12, x13, x14, gvpwr, xtemp

      dt = edt/60.0

c     assign offset to govdat array for local use
      igv = govindx(ign)

      mgov = govtyp(ign)

C     assign large value to govmx
      govmx(ign) = 1.0e6

c     zero govpwr if very small
      if (abs(govpwr(ign)) .lt. 1.0e-6) govpwr(ign) = 0.0

c     determine position of switch in feedback path and save
      if (govdat(igv+16).lt.1.0e-6) then
        govmode(ign) = 1
      else
        govmode(ign) = 2
      endif

c     initialize turbine
      call turbint(ign, govpwr(ign), gvpwr, dt)

c     initialize nonlinear curve
      if (govdat(igv+15).ne.0.0.and.govtyp(ign).ne.5) then
        call cvint( govdat(igv+15), govcv(ign), gvpwr, xtemp,
     &              govdat(igv+ 1) )
        gvpwr = xtemp
      endif

c     put govpwr into Phigh if Phigh is zero and into Plow if Plow is zero
      if (govdat(igv+ 9).lt.1.0e-6) govdat(igv+9) = gvpwr
      if (govdat(igv+10).lt.1.0e-6) govdat(igv+10) = gvpwr

      if (mgov.eq.1) then
c ********************* GS governor *******************************

c       store initial governor power
        govdat(igv+17) = gvpwr

c       store time constants in convenient format
        govdat(igv+ 4) = govdat(igv+ 4)*2.0/dt + 1.0
        govdat(igv+ 5) = govdat(igv+ 5)*2.0/dt + 1.0
        govdat(igv+ 6) = 1.0/govdat(igv+ 6)
        govdat(igv+16) = govdat(igv+16)*2.0/dt + 1.0

c       initialize states
        x1  = 0.0
        x2  = 0.0
        x3p = 0.0
        x3  = govdat(igv+17)
        x4  = x3
        x5  = x4
        x6  = x5

c       store states that need to be stored
        govdat(igv+21) = x4
        govdat(igv+22) = x5
        govdat(igv+24) = 0.0
        govdat(igv+25) = x1
        govdat(igv+26) = x3
        govdat(igv+27) = x4

c       assign history values
        govdat(igv+18) =   govdat(igv+3)*(govdat(igv+4) - 2.0)*x2
     &                                  -(govdat(igv+5) - 2.0)*x1
        govdat(igv+19) = x3*2.0/dt + x3p
        govdat(igv+20) = x6*(govdat(igv+16)-2.0) + gvpwr

      else if (mgov.eq.2) then
c ********************* GH governor *******************************

c       store initial governor power * steady state droop
        govdat(igv+17) = gvpwr*govdat(igv+3)

c       store time constants in convenient format
        govdat(igv+ 5) = govdat(igv+ 5)*2.0/dt + 1.0
        govdat(igv+19) = 2.0*govdat(igv+ 6)*govdat(igv+18)/dt
        govdat(igv+ 6) = govdat(igv+ 6)*2.0/dt + 1.0
        govdat(igv+16) = govdat(igv+16)*2.0/dt + 1.0

c       initialize states
        x1  = 0.0
        x2  = 0.0
        x3  = 0.0
        x4  = gvpwr
        x5  = x4
        x6  = x5
        x7  = 0.0
        x8  = x5

c       store states that need to be stored
        govdat(igv+24) = x5
        govdat(igv+25) = x8
        govdat(igv+30) = x1
        govdat(igv+31) = x4
        govdat(igv+32) = x5

c       assign history values
        govdat(igv+20) = govdat(igv+4)*(govdat(igv+5)-1.0)*x3 + x2
        govdat(igv+21) = x4*2.0/dt + x3
        govdat(igv+22) = (govdat(igv+6)-1.0)*x7 - govdat(igv+19)*x5
        govdat(igv+23) = x6*(govdat(igv+16)-2.0) + gvpwr



      else if (mgov.eq.3) then
c ********************* GP governor *******************************

c       store initial governor power * steady state droop
        govdat(igv+17) = gvpwr*govdat(igv+3)

c       store time constants in convenient format
        govdat(igv+ 4) = govdat(igv+ 4)*2./dt + 1.0
        govdat(igv+ 5) = govdat(igv+ 5)*2./dt + 1.0
        govdat(igv+ 6) = govdat(igv+ 6)*2./dt + 1.0
        govdat(igv+16) = govdat(igv+16)*2./dt + 1.0

c       initialize states
        x1  = 0.0
        x2  = 0.0
        x3  = 0.0
        x4  = gvpwr
        x5  = 0.0
        x6  = gvpwr
        x7  = gvpwr
        x8  = 0.0
        x9  = 0.0
        x10 = gvpwr
        x11 = gvpwr
        x12 = gvpwr

c       store states that need to be stored
        govdat(igv+32) = x6
        govdat(igv+33) = x11
        govdat(igv+34) = x12
        govdat(igv+37) = x1 
        govdat(igv+38) = x10
        govdat(igv+39) = x11

c       assign history values
        govdat(igv+22) = x2 + x3*(govdat(igv+4)-2.0)
        govdat(igv+23) = x3*govdat(igv+20) + x4*2.0/dt
        govdat(igv+24) = x5*(govdat(igv+5)-2.) - x3*2.*govdat(igv+19)/dt 
        govdat(igv+25) = x8*govdat(igv+21) + x9*(govdat(igv+6)-2.)
        govdat(igv+27) = x9 + x10*2.0/dt
        govdat(igv+35) = x7*(govdat(igv+16)-2.0) + gvpwr

      else if (mgov.eq.4) then
c ********************* G2 governor *******************************

c       store initial governor power * steady state droop
        govdat(igv+17) = gvpwr*govdat(igv+3)

c       store time constants in convenient format
        govdat(igv+ 4) = govdat(igv+ 4)*2./dt + 1.0
        govdat(igv+ 5) = govdat(igv+ 5)*2./dt + 1.0
        govdat(igv+ 6) = govdat(igv+ 6)*2./dt + 1.0
        govdat(igv+16) = govdat(igv+16)*2./dt + 1.0

c       initialize states
        x1  = 0.0
        x2  = 0.0
        x3  = 0.0
        x4  = 0.0
        x5  = 0.0
        x6  = 0.0
        x7  = 0.0
        x8  = gvpwr
        x9  = gvpwr
        x10 = 0.0
        x11 = 0.0
        x12 = gvpwr
        x13 = gvpwr
        x14 = gvpwr

c       store states that need to be stored
        govdat(igv+34) = x6
        govdat(igv+35) = x8
        govdat(igv+36) = x13
        govdat(igv+37) = x14
        govdat(igv+40) = x1
        govdat(igv+41) = x12
        govdat(igv+42) = x13

c       assign history values
        govdat(igv+22) = x2*(govdat(igv+4)-2.0) + x1
        govdat(igv+23) = x3*(govdat(igv+5)-2.0)
     &                   - 2.0/dt*govdat(igv+18)*x2
        govdat(igv+24) = x4*(govdat(igv+5)-2.0)
     &                   - 2.0/dt*govdat(igv+19)*x2
        govdat(igv+25) = x5*(govdat(igv+5)-2.0)
     &                   - 2.0/dt*x4
        govdat(igv+27) = 2.0/dt*x8 + govdat(igv+20)*x7
        govdat(igv+28) = x11*(govdat(igv+6)-2.0) + govdat(igv+21)*x10
        govdat(igv+29) = 2.0/dt*x12 + x11
        govdat(igv+38) = x9*(govdat(igv+16)-2.0) + govdat(igv+26)
 
      else if (mgov.eq.5) then
c ********************* GW governor *******************************

c       store initial governor power 
        govdat(igv+7) = gvpwr

c       store time constants in convenient format
        govdat(igv+4) = 2.0*govdat(igv+4)/dt + 1.0
        govdat(igv+5) = 2.0*govdat(igv+5)/dt + 1.0
        govdat(igv+6) = 2.0*govdat(igv+6)/dt + 1.0

c       initialize states
        x1 = 0.0
        x2 = 0.0
        x3 = 0.0
        x4 = gvpwr


c       assign history values
        govdat(igv+8) = x2*(govdat(igv+5)-2.0) - x1*(govdat(igv+4)-2.0)
        govdat(igv+9) = x3*(govdat(igv+6)-2.0) + x2

      endif

c       test for governor power exceeding limits
        if (gvpwr .gt. 1.001*govdat(igv+1) .or. 
     &      gvpwr .lt. 0.999*govdat(igv+2)) then
          write(errbuf(1),100) govname(ign), govdat(igv+2),
     &                         gvpwr, govdat(igv+1)
100       format('Initial power at ', a8, ' exceeds governor limits: ',
     &           f7.3, ' <', f7.3, ' <', f7.3)
          call prterr('E', 1)
          iabort = 1
        endif

c     Put Phigh into Pmax and/or Plow into Pmin depending on Iblock
      if (abs(govdat(igv+11)-1.0).lt.1.0e-6) then
        govdat(igv+2) = govdat(igv+10)
      else if (abs(govdat(igv+11)-2.0).lt.1.0e-6) then
        govdat(igv+1) = govdat(igv+ 9)
      else if (abs(govdat(igv+11)-3.0).lt.1.0e-6) then
        govdat(igv+2) = govdat(igv+10)
        govdat(igv+1) = govdat(igv+ 9)
      endif

      return
      end
