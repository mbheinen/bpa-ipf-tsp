C    @(#)kmptie.f	20.5 10/13/99
        function kmptie (m,n)
 
C       TIESRT IS SORTED WITH THE FOLLOWING FIELDS:
 
C       1. AREA-1
C       2. AREA-2
C       3. BUS-1
C       4. BUS-2
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/qsdup.inc'

        integer MAXTIESORT
        parameter (MAXTIESORT = 200)
        common /tiesrt/ tiesrt(MAXTIESORT)
        integer tiesrt
C
C
        if (m .eq. n) then
           kmptie = 0
        else
           mm = abs(tiesrt(m))
           if (tiesrt(m) .gt. 0) then
              k11 = tie(2,mm)
              k12 = tie(8,mm)
              m11 = tie(1,mm)
              m12 = tie(7,mm)
           else
              k11 = tie(8,mm)
              k12 = tie(2,mm)
              m11 = tie(7,mm)
              m12 = tie(1,mm)
           endif
           nn = abs(tiesrt(n))
           if (tiesrt(n) .gt. 0) then
              k21 = tie(2,nn)
              k22 = tie(8,nn)
              m21 = tie(1,nn)
              m22 = tie(7,nn)
           else
              k21 = tie(8,nn)
              k22 = tie(2,nn)
              m21 = tie(7,nn)
              m22 = tie(1,nn)
           endif
           kmptie = kompr (arcnam(k11),arcnam(k21),junk)
           if (kmptie .eq. 0) then
              kmptie = kompr (arcnam(k12),arcnam(k22),junk)
           endif
           if (kmptie .eq. 0) then
              kmptie = kompr (bus(m11),bus(m21),junk)
           endif
           if (kmptie .eq. 0) then
              kmptie = 100.0*(base(m11) - base(m21))
           endif
           if (kmptie .eq. 0) then
              kmptie = kompr (bus(m12),bus(m22),junk)
           endif
           if (kmptie .eq. 0) then
              kmptie = 100.0*(base(m12) - base(m22))
           endif
           if (kmptie .eq. 0) dupsw=.true.
        endif
        return
        end
