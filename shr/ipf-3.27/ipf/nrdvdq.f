C    @(#)nrdvdq.f	20.11 10/13/99
        real function nrdvdq (jt, iter, kt, vk, qk, dqv, dvdq2)
C
C       This function estimates the derivative dV/dQ using a
C       combination of partials DV/DQ and measured slopes
C       delta_V / delta_Q.
C
C       Input parameters:
C
C       JT   - TBX index.
C       ITER - Current iteration.
C       KT   - Internal bus number.
C       VK   - voltage.
C       QK   - reactive injection.
C       DQV  - partial dQ/dV about (Vk, Qk).
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tbxsrt.inc'
      include 'ipfinc/trmdbg.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/slnphs.inc'

        dvdq0 = 1.0 / dqv
        dvdq1 = dvdq0
        dvdq2 = dvdq0
C
C       Determine B_shunt buried in Ykk.
C
        ltyp = tbx(1,jt)
        ityp = tbx(7,jt)
        bshunt = tbx(6,jt) * vk ** 2
        if (ltyp .eq. 2) then
          if (ityp .eq. 3) then
            bshunt = amin1 (0.0, bshunt)
          else if (ityp .eq. 4) then
            bshunt = amax1 (0.0, bshunt)
          endif
        else if (ltyp .eq. 5) then
          nt = tbx(5,jt)
          bshunt = (xdata(5,nt) + xdata(6,nt)) / bmva
          dvdq0 = xsen(nt)
        endif
        qshunt = bshunt * vk ** 2
 
        if (ltbxsl(1,jt) .gt. 0) then
          dvdq1 = tbxslp(5,jt)
          dv = vk - tbxslp(3,jt)
          dq = qk + qshunt 
     &       - tbxslp(4,jt) - tbxslp(2,jt) * tbxslp(3,jt) ** 2
c
c         Compute dvdq2 only if a meaningful perturbation has occurred.
c
          if (abs(dq) .gt. 0.05 .and. abs (dv) .gt. 0.002) then
            if (ityp .ne. ltbxsl(1,jt) .or.
     &          abs (bshunt-tbxslp(2,jt)) .gt. 0.05 .or.
     &          abs (qk-tbxslp(4,jt)) .gt. 0.05) then
              dvdq2 = amin1 (dvdq0, dvdq1, dv / dq)
             
            else if (ltyp .eq. 5 .and. 
     &               abs(bshunt-tbxslp(2,jt)) .gt. 0.05 .and. 
     &               ityp .ne. ltbxsl(1,jt)) then
              dvdq2 = amin1 (dvdq0, dvdq1, dv / dq)
            else
              dvdq2 = amin1 (dvdq0, dvdq1)
              dv = 0.0
              dq = 0.0
            endif
          else
            dv = 0.0
            dq = 0.0
            dvdq2 = dvdq0
          endif
        else
          dv = 0.0
          dq = 0.0
          dvdq2 = dvdq0
        endif
C
C       Choose the "best" sensitivity of the three that are available.
C
C       DVDQ0 = dV/dQ computed by sensitivities or partials.
C       DVDQ1 = dV/dQ computed by average of previous delta_V/delta_Q.
C       DVDQ2 = dV/dQ computed by present delta_v/delta_Q.
C
C       Flagged to not use DVDQ2 (i.e., sensitivity studies)? Use DVDQ0.
C
        if (ltbxsl(1,jt) .eq. 0) then
          if (dvdq0 .lt. 0.0) dvdq0 = 0.5 * abs (dvdq0)
          dvdq = dvdq0
C
C       DVDQ2 viable?
C
        else if (dvdq2 .lt. 0.0) then
C
C         New dV/dQ < 0. Use 75% of DVDQ0 and attenuate
C         TBXRAT(*) for future use of sensitivity calculations.
C
          if (dvdq1 .lt. 0.0) then
            if (dvdq0 .lt. 0.0) dvdq0 = 0.5 * abs (dvdq0)
            dvdq = dvdq0
          else
            dvdq = dvdq1
          endif
 
        else if (dvdq2 .gt. 0.0) then
C
C         Acceptable value: Use perturbed value.
C
          dvdq = dvdq2
 
        else if (dvdq1 .gt. 0.0) then
C
C         Acceptable value: Use previous value.
C
          dvdq = dvdq1
 
        else
C
C         Acceptable value: Use "average" of previous values.
C
          if (dvdq0 .lt. 0.0) dvdq0 = 0.5 * abs (dvdq0)
          if (dvdq1 .lt. 0.0) dvdq1 = 0.5 * abs (dvdq1)
          dvdq = 0.50 * dvdq0 + 0.50 * dvdq1
 
        endif
 
        if (dvdq .lt. 0.0) dvdq = 0.5 * abs (dvdq)
 
        if (ltbxsl(1,jt) .eq. 0 .or.
     &      abs(dq) .gt. abs(tbxslp(6,jt))) then
          ltbxsl(1,jt) = ityp
          tbxslp(2,jt) = bshunt
          tbxslp(3,jt) = vk
          tbxslp(4,jt) = qk 
          tbxslp(5,jt) = dvdq
          tbxslp(6,jt) = dq
        endif
 
        nrdvdq = dvdq
 
        return
        end
