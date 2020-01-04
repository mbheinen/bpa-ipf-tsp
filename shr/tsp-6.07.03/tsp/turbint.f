C    %W% %G%
      subroutine turbint(ign, ptb, pgv, dt)
c     initializes turbine model

      implicit none

      include 'tspinc/params.inc'
      include 'tspinc/turb.inc'
      include 'tspinc/prt.inc'

c     local variables
c     ign       -  generator number
c     itb       -  turbine number
c     itb       -  index to turbdat array
c     dt        -  past time step (in seconds)
c     ptb       -  turbine output (pm1 for cc govs)
c     pgv       -  governor output

      real temp(18), dt, pgv, ptb, x1, x2, x3, x4, x5, x6, x7, x8
      integer ign, itb, j
      character gname*16, cardtype*2

c     assign turbine offset for local use
      itb = turbindx(ign)

      if (turbtyp(ign) .eq. 1) then
c ********************* TA turbine *******************************

c       store time constants in convenient format
        turbdat(itb+ 1) = turbdat(itb+ 1)*2.0/dt + 1.0

c       initialize states
        pgv = ptb
        x1  = pgv
        x2  = pgv

c       assign history values
        turbdat(itb+2) =  x2*(turbdat(itb+1)-2.0) + x1

      else if (turbtyp(ign) .eq. 2) then
c ********************* TB turbine *******************************

c       store time constants in convenient format
        turbdat(itb+ 1) = turbdat(itb+ 1)*2.0/dt + 1.0
        turbdat(itb+ 2) = turbdat(itb+ 2)*2.0/dt + 1.0
        turbdat(itb+ 3) = turbdat(itb+ 3)*2.0/dt + 1.0

c       initialize states
        pgv = ptb
        x1  = pgv
        x2  = pgv
        x3  = pgv
        x4  = pgv

c       assign history values
        turbdat(itb+4) =  x2*(turbdat(itb+1)-2.0) + x1
        turbdat(itb+5) =  x2*(turbdat(itb+2)-2.0) + x1
        turbdat(itb+6) =  x2*(turbdat(itb+3)-2.0) + x1

      else if (turbtyp(ign) .eq. 3) then
c ********************* TC turbine *******************************

c       store time constants in convenient format
        turbdat(itb+ 1) = turbdat(itb+ 1)*2.0/dt + 1.0
        turbdat(itb+ 2) = turbdat(itb+ 2)*2.0/dt + 1.0
        turbdat(itb+ 3) = turbdat(itb+ 3)*2.0/dt + 1.0
        turbdat(itb+ 4) = turbdat(itb+ 4)*2.0/dt + 1.0

c       initialize states
        pgv = ptb
        x1  = pgv
        x2  = pgv
        x3  = pgv
        x4  = pgv
        x5  = pgv

c       assign history values
        turbdat(itb+5) =  x2*(turbdat(itb+1)-2.0) + x1
        turbdat(itb+6) =  x3*(turbdat(itb+2)-2.0) + x2
        turbdat(itb+7) =  x4*(turbdat(itb+3)-2.0) + x3
        turbdat(itb+8) =  x5*(turbdat(itb+4)-2.0) + x4



      else if (turbtyp(ign) .eq. 4) then
c ********************* TD turbine *******************************

c       store time constants in convenient format
        turbdat(itb+ 1) = turbdat(itb+ 1)*2.0/dt + 1.0
        turbdat(itb+ 2) = turbdat(itb+ 2)*2.0/dt + 1.0
        turbdat(itb+ 3) = turbdat(itb+ 3)*2.0/dt + 1.0

c       initialize states (ptb = pm1)
        pgv = ptb/( turbdat(itb+7) + turbdat(itb+9) )
        x1  = pgv
        x2  = pgv
        x3  = pgv
        x4  = pgv

c       assign history values
        turbdat(itb+4) =  x2*(turbdat(itb+1)-2.0) + x1
        turbdat(itb+5) =  x2*(turbdat(itb+2)-2.0) + x1
        turbdat(itb+6) =  x2*(turbdat(itb+3)-2.0) + x1


      else if (turbtyp(ign) .eq. 5) then
c ********************* TE turbine *******************************

c       store time constants in convenient format
        turbdat(itb+ 1) = turbdat(itb+ 1)*2.0/dt + 1.0
        turbdat(itb+ 2) = turbdat(itb+ 2)*2.0/dt + 1.0
        turbdat(itb+ 3) = turbdat(itb+ 3)*2.0/dt + 1.0

c       initialize states
        pgv = ptb/( turbdat(itb+7) + turbdat(itb+8) )
        x1  = pgv
        x2  = pgv
        x3  = pgv
        x4  = pgv

c       assign history values
        turbdat(itb+4) =  x2*(turbdat(itb+1)-2.0) + x1
        turbdat(itb+5) =  x2*(turbdat(itb+2)-2.0) + x1
        turbdat(itb+6) =  x2*(turbdat(itb+3)-2.0) + x1


      else if (turbtyp(ign) .eq. 6) then
c ********************* TF turbine *******************************

c       store time constants in convenient format
        turbdat(itb+ 1) = turbdat(itb+ 1)*2.0/dt + 1.0
        turbdat(itb+ 2) = turbdat(itb+ 2)*2.0/dt + 1.0
        turbdat(itb+ 3) = turbdat(itb+ 3)*2.0/dt + 1.0
        turbdat(itb+ 4) = turbdat(itb+ 4)*2.0/dt + 1.0

c       initialize states
        pgv = ptb/(turbdat(itb+9) + turbdat(itb+11) + turbdat(itb+13))
        x1  = pgv
        x2  = pgv
        x3  = pgv
        x4  = pgv
        x5  = pgv

c       assign history values
        turbdat(itb+5) =  x2*(turbdat(itb+1)-2.0) + x1
        turbdat(itb+6) =  x3*(turbdat(itb+2)-2.0) + x2
        turbdat(itb+7) =  x4*(turbdat(itb+3)-2.0) + x3
        turbdat(itb+8) =  x5*(turbdat(itb+4)-2.0) + x4


      else if (turbtyp(ign) .eq. 7) then
c ********************* TG turbine *******************************

c       store time constants in convenient format
        turbdat(itb+ 1) = turbdat(itb+ 1)*2.0/dt + 1.0
        turbdat(itb+ 2) = turbdat(itb+ 2)*2.0/dt + 1.0
        turbdat(itb+ 3) = turbdat(itb+ 3)*2.0/dt + 1.0
        turbdat(itb+ 4) = turbdat(itb+ 4)*2.0/dt + 1.0

c       initialize states
        pgv = ptb/(  turbdat(itb+ 9) + turbdat(itb+11) 
     &             + turbdat(itb+13) + turbdat(itb+15))
        x1  = pgv
        x2  = pgv
        x3  = pgv
        x4  = pgv
        x5  = pgv

c       assign history values
        turbdat(itb+5) =  x2*(turbdat(itb+1)-2.0) + x1
        turbdat(itb+6) =  x3*(turbdat(itb+2)-2.0) + x2
        turbdat(itb+7) =  x4*(turbdat(itb+3)-2.0) + x3
        turbdat(itb+8) =  x5*(turbdat(itb+4)-2.0) + x4


      else if (turbtyp(ign) .eq. 8) then
c ********************* TW turbine *******************************

c       store time constants in convenient format
        turbdat(itb+1) = turbdat(itb+1)*2.0/dt + 1.0

c       initialize states
        pgv = ptb
        x1  = pgv
        x2  = pgv

c       assign history values
        turbdat(itb+2) =  x2*(turbdat(itb+1)-2.0) + x1


      else
          call genname (ign, gname)                                        !dem
          write (errbuf(1),100) gname
  100     format('Missing turbine data for ', a)
          call prterr('E', 1)
      endif

      return
      end

