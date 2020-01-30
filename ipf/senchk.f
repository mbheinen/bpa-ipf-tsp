C    @(#)senchk.f	20.3 2/13/96
      integer function senchk (kt)
C
C     THIS FUNCTION DETERMINES WHETHER PV OR PQ CONSTRAINTS SHOULD
C     BE OBSERVED FOR TYPE "BQ" AND "BG" BUSES.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/tbx.inc'
 
C
        if (kvolt(kt) .eq. 0) then
           senchk = 0
        else if (kspare(39) .eq. 0 .and. kspare(40) .eq. 0) then
           senchk = kvolt(kt)
        else
           senchk = kvolt(kt)
           i1 = iflag(kt)
           i2 = iflag(kt+1) - 1
C
C          The conditions for a "BQ" bus to hold PV are:
C             1. Q_shunt is adjustable and Shunt .ne. 0, or
C             2. Q_generation is adjustable and Qgen is .ne. 0.
C
C          The conditions for a "BG" bus to hold PV are:
C             1. Q_generation is adjustable and Qgen is .ne. 0.
C
           do 90 i = i1,i2
           if (jflag(1,i) .eq. 8) then
              jt = jflag(2,i)
              if (tbx(1,jt) .eq. 2) then
C
C                CHECK LATITUDE OF TYPE "BQ" BUS
C
                 qadj = tbx(6,jt)*(e(kt)**2 + f(kt)**2)
                 if ((abs(qadj) .gt. 0.010) .and. kspare(39) .eq. 0)
     1           then
                    senchk = kvolt(kt)
                    go to 900
                 else if (abs(tbx(3,jt)-tbx(4,jt)-qadj) .gt. 0.010 .and.
     1              kspare(40) .eq. 0) then
                    senchk = kvolt(kt)
                    go to 900
                 else
                    senchk = 0
                    go to 900
                 endif
              else if (tbx(1,jt) .eq. 3) then
C
C                CHECK LATITUDE OF TYPE "BG" BUS
C
                 qadj = tbx(6,jt)*(e(kt)**2 + f(kt)**2)
                 if (abs(tbx(3,jt)-tbx(4,jt)-qadj) .gt. 0.010 .and.
     1              kspare(40) .eq. 0) then
                    senchk = kvolt(kt)
                    go to 900
                 else
                    senchk = 0
                    go to 900
                 endif
              endif
           endif
   90      continue
           senchk = kvolt(kt)
        endif
  900   continue
        return
        end
