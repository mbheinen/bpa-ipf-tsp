C    @(#)dpdu.f	20.6 7/18/96
      subroutine dpdu (k1,m1,y1,k2,m2,dydb,deltax,pij,dgdu)
C
C     This subroutine computes the sensitivity dPij/dBkl.
C
C     Pij is defined by K1,M1,
C     Bkl is defined by K2,M2.
C
C     The sensitivity dYkl/dBkl is defined by DYDB.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'
 
C
      complex v(2), dydb(2,2), y1(2,2), a(2), s(2)
      integer itype(2)
C
C     Compute Pij
C
      v(1) = cmplx (e(k1),f(k1))
      v(2) = cmplx (e(m1),f(m1))
      a(1) = y1(1,1) * v(1) + y1(1,2) * v(2)
      s(1) = v(1) * conjg (a(1))
      pij = real (s(1)) * bmva
C
C     Compute dG/dU.  This is usually zero unless K1-M1 is identical
C     to K2-M2.
C
      dgdu = 0.0
      if (k1 .eq. k2 .and. m1 .eq. m2 .or.
     1    k1 .eq. m2 .and. m1 .eq. k2) then
         dgdu = real (v(1)*conjg(v(1)) * dydb(1,1))
     1         + real (v(2)*conjg(v(1)) * dydb(1,2))
      endif
C
C     Determine constraints affected by dU. Scenarios are
C
C     xx1. Qi, Qj if Q injection is constrained,
C     x1x. Pi, Pj if P injection is constrained,
C     1xx. Sk and Sl if Pij is an intertie line spanning areas k and l
C
      do 110 i = 1,2
      if (i .eq. 1) then
         kt = k2
         mt = m2
      else
         kt = m2
         mt = k2
      endif
      if (kt .le. nbslck) then
         itype(i) = 0
      else
         itype(i) = 10
      endif
      if (kvolt(kt) .eq. 0) then
         itype(i) = itype(i) + 1
      endif
      if (kspare(20) .eq. 0 .and. jtie .gt. 0) then
C
C     Check for slack bus (conditional upon AI_CONTROL option
C     on > SENSITIVITY < not enabled).
C
         i1 = iflag(kt)
         i2 = iflag(kt+1) - 1
         kb = opt2inp(kt)
         mb = opt2inp(mt)
         do 100 j = i1,i2
         if (jflag(1,j) .eq. 3) then
            itype(i) = itype(i)/100*100 + mod (itype(i),10)
         else if (jflag(1,j) .eq. 5) then
            jt = jflag(2,j)
            if (tie(1,jt) .eq. kb .and. tie(7,jt) .eq. mb .or.
     1          tie(1,jt) .eq. mb .and. tie(7,jt) .eq. kb) then
               itype(i) = 100 + mod (itype(i),100)
            endif
         endif
  100    continue
      endif
  110 continue
 
      do 120 i = 1,2
      if (i .eq. 1) then
         kt = k2
         mt = m2
      else
         kt = m2
         mt = k2
      endif
      v(1) = cmplx (e(kt),f(kt))
      v(2) = cmplx (e(mt),f(mt))
C
C     Compute dG/dU = ... + dPk/dBkl * Lambda Pk
C                         + dPl/dBkl * Lambda Pl
C
      if (mod (itype(i)/10,10) .eq. 1) then
          if (i .eq. 1) then
             dpdb = real (v(1)*conjg(v(1)) * dydb(1,1)) +
     1              real (v(2)*conjg(v(1)) * dydb(1,2))
          else
             dpdb = real (v(1)*conjg(v(1)) * dydb(2,2)) +
     1              real (v(2)*conjg(v(1)) * dydb(2,1))
          endif
          dgdu = dgdu + dpdb * dpt(1,kt+ntota)
      endif
C
C     Compute dG/dU = ... + dQk/dBkl * Lambda Qk
C                         + dQl/dBkl * Lambda Ql
C
      if (mod (itype(i),10) .eq. 1) then
          if (i .eq. 1) then
             dqdb = -aimag (v(1)*conjg(v(1)) * dydb(1,1)) -
     1               aimag (v(2)*conjg(v(1)) * dydb(1,2))
          else
             dqdb = -aimag (v(1)*conjg(v(1)) * dydb(2,2)) -
     1               aimag (v(2)*conjg(v(1)) * dydb(2,1))
          endif
          dgdu = dgdu + dqdb * dpt(2,kt+ntota)
      endif
  120 continue
C
C     Compute dG/dU = ... + dAm/dBkl * Lambda Am
C                         + dAl/dBkl * Lambda Al
C
      if (mod (itype(1)/100,10) .eq. 1) then
         kt = k2
         mt = m2
         v(1) = cmplx (e(kt),f(kt))
         v(2) = cmplx (e(mt),f(mt))
         i1 = iflag(kt)
         i2 = iflag(kt+1) - 1
         kb = opt2inp(kt)
         mb = opt2inp(mt)
         do 130 j = i1,i2
         if (jflag(1,j) .eq. 5) then
            jt = jflag(2,j)
            if (tie(1,jt) .eq. kb .and. tie(7,jt) .eq. mb) then
               dir = +1.0
            else if (tie(1,jt) .eq. kb .and. tie(7,jt) .eq. mb) then
               dir = -1.0
            else
               go to 130
            endif
            dsdb = real (v(1)*conjg(v(1)) * dydb(1,1)) +
     1             real (v(2)*conjg(v(1)) * dydb(1,2))
            ka1 = tie(2,jt)
            ka1 = karea(1,ka1)
            if (ka1 .gt. nbslck) then
               dgdu = dgdu + dir * dsdb * dpt(1,ka1+ntota)
            endif
            ka1 = tie(8,jt)
            ka1 = karea(1,ka1)
            if (ka1 .gt. nbslck) then
               dgdu = dgdu - dir * dsdb * dpt(1,ka1+ntota)
            endif
            go to 140
         endif
  130    continue
  140    continue
      endif
 
      dgdu = dgdu * bmva
 
      return
      end
