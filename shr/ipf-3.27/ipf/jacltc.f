C    @(#)jacltc.f	20.7 6/27/97
      function jacltc (jt, inquir)
C
C     Compute Jacobian elements for LTC's. They are stored in KOLUM, 
C     ROWH, ROWN, ROWJ, ROWL, KOLUM, and DPT.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/ltcsln.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/trmdbg.inc'
 
      common /tran_phid/ tran_phid(MAXLTC)
      integer tran_phid
C
      character flag * 1, trace * 6
C
      common /txstat/ txstat(MAXLTC)
      integer txstat
 
      common /txsens/ ltxsen(MAXLTC), txsens(2,MAXLTC)

C     Compute Jacobian elements for LTC  
C     JACLTC assignments - 0: 1*DT = DX 
C                          1: 1*DT + 100000.0*DV = 0.0  
C                          2: 1*DT + H*DA + N*DV = DP or
C                             1*DT + H*DA + N*DV = DQ   
C                          3: 1*DT + J'*DA + L'*DV + ... = DQk'
C                          4: 1*DT + J'*DA + L'*DV + ... = DQm'

      ityp = mod(ltran(10,jt),100)  
      lt = ltran(10,jt)/100 
      kt = ltran(1,jt)
      mt = ltran(9,jt)
      jacltc = 0
      dx = 0.0  
      trace = ' '   
      dx1 = tran(7,jt)-tap(jt)  
      dx2 = tran(8,jt)-tap(jt)  
      flag = ' '

C     Create a default identity row; redefine if auto control.  

      call ltcctl (jt, ityp, jacltc, 0, dx, dy)  
      if (ityp .gt. 10) then 
        trace(1:) = 'A' 

C       Inactive LTC's have identity elements in Jacobian (Default). 
C       Temporarily disabled LTC's (11 <= ITYP < 20) are reset for   
C       subsequent iterations. Permanently disabled LTC's (ITYP > 20)
C       remain disabled. 

        if (ityp .gt. 10 .and. ityp .le. 20) then
           ityp = mod (ityp, 10) 
           ltran(10,jt) = 100*lt + ityp 
        endif   
        dx = 0.0
      else if ((msw .eq. 2 .or. txstat(jt) .eq. -2) .and.
     &         tran(11,jt) .gt. 1.0 .and. ityp .ne. 3) then
        trace(1:) = 'B' 

C       Discretized LTS's are adjusted directly.
C       MSW assignments: 1 - normal (continous BX and LTC taps)
C                        2 - discretize BX,LTC taps
C                        3 - discretization completed  

        if (txstat(jt) .eq. -2) txstat(jt) = -1 
        tmax = tran(6,jt) / tran(8,jt)  
        tmin = tran(6,jt) / tran(7,jt)  
        tact = tran(6,jt) / tap(jt) 
        tsteps = (tmax - tmin) / (tran(11,jt) - 1.0)
        stepac = (tact - tmin) / tsteps 
        stepds = int(stepac + 0.501)
        tdis = tran(6,jt) / (tmin + stepds * tsteps)
        dx = tdis - tap(jt) 
        dxtemp = dx/tap(jt)
        call ltcctl (jt, ityp, jacltc, 0, dxtemp, dy)
C       
C       Force iteration count for small discrete steps. 
C       
        if (abs (dx) .gt. option(24) .and. 
     &      abs (dx) .lt. 10.0 * option(24)) then 
           kownta = kownta + 1  
        endif   
        
      else if (((msw .eq. 3 .and. txstat(jt) .ge. 0) .or. 
     &          (txstat(jt) .eq. -1)) .and.
     &         tran(11,jt) .gt. 1.0 .and. ityp .ne. 3) then   
        trace(1:) = 'C' 
C       
C       Continue LTC adjustments, but only in discrete steps.   
C       
C       MSW assignments: 1 - normal (continous BX and LTC taps) 
C                        2 - discretize BX,LTC taps 
C                        3 - discretization completed   
C       
        tmax = tran(6,jt) / tran(8,jt)  
        tmin = tran(6,jt) / tran(7,jt)  
        tsteps = (tmax - tmin) / (tran(11,jt) - 1.0)
        told = tran(6,jt) / tap(jt) 
        stepol = (told - tmin) / tsteps 
        tserr = amod (stepol+0.01, 1.0) 
        if (abs(tserr) .gt. 0.02) then  
           if (idswa .ne. 0) then   
              write (dbug, 200) jt, stepol  
  200         format (' LTC ',i3,' step ',f8.4,' is not discrete')  
           endif
           if (iterm .ne. 0) then   
              write (*, 200) jt, stepol 
           endif
        endif   
C       
C       Estimate discrete LTC tap adjustment for manual 
C       voltage control.
C       
        nt = ltran(2,jt)
        kc = 0  
        if (nt.eq.-1) kc=kt 
        if (nt.eq.-2) kc=mt 
C       
C       Test for no controlled bus specified.   
C       
        trace(2:) = '1' 
        if (kc.eq.0) go to 500  
        trace(2:) = '2' 
        if (kvolt(kc).ne.0) go to 500   
        call nrpqv (kt,pk,dpk,qk,dqk,v1)
        if (kt .le. nbslck) dpk = 0.0   
        if (kvolt(kt) .ne. 0) dqk = 0.0 
        call nrpqv (mt,pm,dpm,qm,dqm,v2)
        if (mt .le. nbslck) dpm = 0.0   
        if (kvolt(mt) .ne. 0) dqm = 0.0 
C       
C       Ignore LTC adjustment if terminal buses have not converged. 
C       
        trace(2:) = '3' 
        if (amax1(abs(dpk),abs(dpm)) .gt. 2.0*option(6) .or.
     &      amax1(abs(dqk),abs(dqm)) .gt. 2.0*option(6)) go to 500 
        
        dv1 = -dim(v1,vlimx(kt)) + dim(vlimn(kt),v1)
        dv2 = -dim(v2,vlimx(mt)) + dim(vlimn(mt),v2)
        if (nt.eq.-1) then  
          dv = dv1
          dt = dv/v2  
          dp = dpk  
          dq = dqk  
          vk = v1   
        else
          dv = dv2
          dt = -dv*v1/v2**2   
          dp = dpm  
          dq = dqm  
          vk = v2   
        endif   
        trace(2:) = '4' 
        if (dv .eq. 0.0 .and. vlimn(kc) .lt. vlimx(kc)) go to 500   
        trace(2:) = '5' 

        if (inquir.eq.0) ddtot = ddtot + abs(dv)
        trace(2:) = '6' 
        tnew = dmin1 (dble(tmax), dmax1 
     &        (dble(tmin), dble(tran(6,jt)) / (tap(jt) + dt)))  
        stepnx = (tnew - tmin) / tsteps 
        stepne = int (stepnx + 0.501)   
C       
C       This scheme is susceptible to hunting when D_tap = 0.50.
C       Accept the tap which with DV < 0 providing that the voltage 
C       is within its global limits.
C       
        if (abs (stepnx - stepol) .lt. 0.75 .and.   
     &      abs (stepnx - stepol) .gt. 0.25) then   
           if (abs(txsens(2,jt)-stepol) .le. 0.05) then
              stepne = stepol
           else if (dv .lt. 0.0) then
              if (nt .eq. -1) then  
                  call glbvlt (opt2inp(kt), vmin, vmax)   
              else  
                  call glbvlt (opt2inp(mt), vmin, vmax)   
              endif 
              dvx = -dim (vk, vmax) + dim (vmin, vk)
              if (dvx .eq. 0.0) stepne = stepol 
           else if (dv .gt. 0.0) then   
              if (nt .eq. -1) then  
                  call glbvlt (opt2inp(kt), vmin, vmax)   
              else  
                  call glbvlt (opt2inp(mt), vmin, vmax)   
              endif 
C       
C             If global voltage limits are violated, raise voltage  
C             if DSTEPS > 0.5.  
C       
              dvx = -dim (vk, vmax) + dim (vmin, vk)
              if (dvx .ne. 0.0) then
              else  
C       
C                Tap adjustment will be rounded to a discrete value.
C                Estimate the new voltage using a 50% margin.   
C       
                 if (nt .eq. -1) then   
                    vnew = vk + 2.0 * dt * v2   
                 else   
                    vnew = vk - 2.0 * dt * v2 ** 2 / v1 
                 endif  
                 dvx = -dim (vnew, vmax) + dim (vmin, vnew) 
                 if (dvx .ne. 0.0) stepne = stepol  
              endif 
           endif
        else
           txsens(2,jt) = stepne
        endif   
C       
C       Limit change to one step per iteration. 
C       
        dsteps = stepne - stepol
        if (abs (dsteps) .gt. 1.1) then 
           stepne = stepol + sign (1.0, dsteps) 
        endif   
        tdis = tran(6,jt) / (tmin + stepne * tsteps)
        dx = tdis - tap(jt) 
        dxtemp = dx/tap(jt)
        call ltcctl (jt, ityp, jacltc, 0, dxtemp, dy)
        
      else if (msw .eq. 2 .and. tran(11,jt) .gt. 1.0 .and. ityp .eq. 3)
     &  then
        trace(1:) = 'D' 
C       
C       Discretized LTC phase shifters are adjusted directly.   
C       
C       MSW assignments: 1 - normal (continous BX and LTC taps) 
C                        2 - discretize BX,LTC taps 
C                        3 - discretization completed   
C       
        tmax = 57.29577951*tran(7,jt)   
        tmin = 57.29577951*tran(8,jt)   
        tact = 57.29577951*tap(jt)  
        dv = (tmax - tmin)/(tran(11,jt) - 1.0)  
        stepac = (tact - tmin)/dv   
        stepds = int(stepac + 0.501)
        tdis = tmin + stepds*dv 
        dx = 0.0174532925*(tdis - tact) 
        call ltcctl (jt, ityp, jacltc, 0, dx, dy)
        
      else if ((ityp .eq. 1 .or. ityp .eq. 4) .and. 
     &         (itsw .ge. 2 .and. itsw .le. 4) .and.
     &          ittot .ge. 3 .and. txstat(jt) .ne. -1 .and. 
     &         (msw  .ne. 3 .or. tran(11,jt) .le. 1.0)) then
        trace(1:) = 'E' 
C       
C       Examine LTC voltage/reactive control
C       
        nt = ltran(2,jt)
        kc = 0  
        if (nt.eq.-1) kc=kt 
        if (nt.eq.-2) kc=mt 
C       
C       Test for no controlled bus specified.   
C       
        if (kc.eq.0) go to 500  
        trace(2:) = '1' 
        if (kvolt(kc).ne.0) go to 500   
        trace(2:) = '2' 
        call nrpqv (kt,pk,dpk,qk,dqk,v1)
        if (kt .le. nbslck) dpk = 0.0   
        if (kvolt(kt) .ne. 0) dqk = 0.0 
        call nrpqv (mt,pm,dpm,qm,dqm,v2)
        if (mt .le. nbslck) dpm = 0.0   
        if (kvolt(mt) .ne. 0) dqm = 0.0 
        dv1 = -dim(v1,vlimx(kt)) + dim(vlimn(kt),v1)
        dv2 = -dim(v2,vlimx(mt)) + dim(vlimn(mt),v2)
        if (nt.eq.-1) then  
          dv=dv1
          dt=dv/v2  
          dp = dpk  
          dq = dqk  
          vk = v1   
        else
          dv=dv2
          dt=-dv*v1/v2**2   
          dp = dpm  
          dq = dqm  
          vk = v2   
        endif   
        if (dv .eq. 0.0 .and. vlimn(kc) .lt. vlimx(kc)) go to 500   

        trace(2:) = '3' 
        if (inquir.eq.0) ddtot = ddtot + abs(dv)
C       
C       Test for near-convergence for NR control
C       (use more severe test if LTC is in auto control in previous 
C       iteration). 
C       
        if (txstat(jt).eq.0) then   
           if (amax1(abs(dpk),abs(dpm)) .gt. 2.0*option(6) .or. 
     &         amax1(abs(dqk),abs(dqm)) .gt. 2.0*option(6)) go to 500  

        else
           if (amax1(abs(dpk),abs(dpm)) .gt. 2.0*option(6) .or. 
     &         amax1(abs(dqk),abs(dqm)) .gt. 2.0*option(6)) go to 500  

        endif   
        trace(2:) = '4' 
        if ((abs(dt) .gt. option(24) .and.  
     &       sign(1.0,dt) .eq. sign(1.0,dt+0.01*(dqk-dqm))) .or.
     &      (abs(dt).lt.option(24))) then   
           dx = dt  
           jacltc = 1   
           dxtemp = dv / vk
           call ltcctl (jt, ityp, jacltc, kc, dxtemp, dy)
           trace(2:) = '5'  
        endif   
C       
C     Temporary LTC control of QKM to "optimize" bus reactive 
C       
      else if (ityp .eq. 5) then
         trace(1:) = 'F'
         jacltc = 2 
         call ltcctl (jt, ityp, jacltc, 0, pkm, dpkm)
         if (inquir.eq.0) dttot = dttot + abs (dpkm)
         dx=tran(5,jt)  
         ltran(10,jt) = 100*lt+1
         tran(4,jt)=0   
         tran(5,jt)=0   
C       
C     Temporary LTC control of Tap to "optimize" voltages
C       
      else if (ityp .eq. 6 .or. ityp .eq. 7) then
         trace(1:) = 'F'
         if ((ltran(2,jt) .eq. -1 .and. kc .eq. kt) .or.
     &       (ltran(2,jt) .eq. -2 .and. kc .eq. mt)) then

           jacltc = 1
           dx = tran(5,jt)
           call ltcctl (jt, ityp, jacltc, kc, dx, dy)
         else
           dx = tran(5,jt)
           vk = dsqrt (e(kt)**2 + f(kt)**2)
           vm = dsqrt (e(mt)**2 + f(mt)**2)
           if (ityp .eq. 6) then
             kc = kt
             dx = dx * vk / vm
           else 
             kc = mt
             dx = -dx * vk / vm**2
           endif
           call ltcctl (jt, ityp, jacltc, 0, dx, dy)
         endif
         ltran(10,jt) = 100*lt+1
         tran(4,jt)=0   
         tran(5,jt)=0   
C       
C     Temporary LTC control of QK to "optimize" bus reactive 
C       
      else if (ityp .eq. 8) then
         trace(1:) = 'F'
         jacltc = 3
         dkpm = tran(4,jt)
         call ltcctl (jt, ityp, jacltc, 0, pkm, dpkm)
         if (inquir.eq.0) dttot = dttot + abs (dpkm)
         dx = tran(5,jt)  
         ltran(10,jt) = 100*lt+1
         tran(4,jt)=0   
         tran(5,jt)=0   
C       
C     Temporary LTC control of QM to "optimize" bus reactive 
C       
      else if (ityp .eq. 9) then
         trace(1:) = 'F'
         jacltc = 4
         dkpm = tran(4,jt)
         call ltcctl (jt, ityp, jacltc, 0, pkm, dpkm)
         if (inquir.eq.0) dttot = dttot + abs (dpkm)
         dx = tran(5,jt)  
         ltran(10,jt) = 100*lt+1
         tran(4,jt)=0   
         tran(5,jt)=0   
C       
C     LTC control for type RQ,RN
C       
      else if (ityp .eq. 2 .and. ittot .ge. 3) then 
         trace(1:) = 'G'
         jacltc = 2 
         call ltcctl (jt, ityp, jacltc, 0, pkm, dpkm)
         if (inquir.eq.0) dttot = dttot + abs (dpkm)
         if (dpkm .eq. 0.0 .and.
     &       tran(4,jt) - tran(5,jt) .gt. 2.0*option(6)) then  
            dpkm = 0.0  
            jacltc = 0  
            call ltcctl (jt, ityp, jacltc, 0, dx, dy)
            trace(2:) = '1' 
         endif  
C       
C     LTC control for type RP,RM
C       
      else if (ityp .eq. 3 .and. ittot .ge. 3 .and. 
     &        (msw  .ne. 3 .or. tran(11,jt) .le. 1.0)) then 
        trace(1:) = 'H' 
        jacltc = 2  
        call ltcctl (jt, ityp, jacltc, 0, pkm, dpkm) 
        if (inquir.eq.0) dttot = dttot + abs (dpkm) 
        dadp = phse(jt) 
        if (dadp.ge.0) dadp = -0.1  
        if (abs (phse(jt)) .gt. 3.14) then  
C       
C           If PHSE(*) < -PI, system is radial or phase shifter
C           is very small MVA and LTC autocontrol may cause Jacobian 
C           singularity!
C
            dx = dadp * dpkm
            jacltc = 0  
            call ltcctl (jt, ityp, jacltc, 0, dx, dy)
            trace(2:) = '1' 

        else if (dpkm .eq. 0.0 .and.
     &           tran(4,jt) - tran(5,jt) .gt. 2.0 * option(6)) then
C       
C           Apply phase shifter bias PHASE_SHIFTER_BIAS = BPA or WSCC.
C
C           BPA: "Optimize" LTC phase shifter by gravitating towards 
C                zero degrees phase shift. 
C       
C           WSCC: "Optimize" LTC phase shifter by gravitating towards 
C                original phase shift. 
C       
            trace(2:) = '2' 
            if (iopton(21) .eq. 0) then
               tnew = 0.0  
               jx = 0.0
            else
               jx = tran_phid(jt)
               if (jx .gt. 0) then
                  tnew = phid(5,jx)
               else if (jx .lt. 0) then
                  tnew = -phid(5,-jx)
               else
                  tnew = 0.0  
               endif
            endif

            tnew = tnew - ddim(dble(tnew),dble(tran(7,jt))) + 
     &             ddim(dble(tran(8,jt)),dble(tnew))   
            dx = tnew - tap(jt) 
            pnew = pkm + dx / dadp  
            if (jx .ne. 0 .and.
     &          ddim(dble(pnew),dble(tran(4,jt))) +
     &          ddim(dble(tran(5,jt)),dble(pnew)) .eq. 0.0)
     &          then
               jacltc = 0
               trace(2:) = '3'  
               call ltcctl (jt, ityp, jacltc, 0, dx, dy) 
            else
               pnew = pnew - ddim(dble(pnew),dble(tran(4,jt))) +
     & 		      ddim(dble(tran(5,jt)),dble(pnew))   
               xpkm = pnew - pkm   
               dx = xpkm * dadp
               if (abs(dx) .gt. 10.0 * option(24)) then 
                  dpkm = 0.0   
                  jacltc = 0   
                  trace(2:) = '3'  
                  call ltcctl (jt, ityp, jacltc, 0, dx, dy) 
               endif   
            endif

            if (jx .ne. 0) then
               ix = iabs (jx)
               write (*, 7777) jt, jx, jacltc, phid(5,ix), tap(jt), 
     &                         tnew, dx, pkm, dpkm
 7777          format (' PHASE-SHIFTER ', 3i4, 6f10.5)
            endif
            ixxx = ittot    ! Dummy variable set for watchpoint
         endif  
         if (jacltc.ne.0) then  
C       
C           Estimate effects of nodal injection error.  
C       
            call nrpqv (kt,pk,dpk,qk,dqk,v1)
            if (kt .le. nbslck) dpk = 0.0   
            if (kvolt(kt) .ne. 0) dqk = 0.0 
            call nrpqv (mt,pm,dpm,qm,dqm,v2)
            if (mt .le. nbslck) dpm = 0.0   
            if (kvolt(mt) .ne. 0) dqm = 0.0 
            dp = 0.25*(dpk - dpm)   
C       
C           Determine most feasible adjustment strategy based upon nodal
C           injection/phase shift error ratio.  
C       
            if (abs (dpkm).gt.option(6)) then   
               da = dp/dpkm 
               if (da.gt.2.0.or.da.lt.-2.0) then
C       
C                 Most of error is nodal injection error. LTC adjustment
C                 can be reliabliy predicted because Jacobian adjustment
C                 is steepest decent of sum-of-square errors. LTC   
C                 phase shift adjustments will be dominated by nodal
C                 injection errors. 
C       
                  if (abs(dp).lt.1.0) then  
                    dx = (dp - dpkm) * dadp 
                  else  
                    dx = 0.0
                    jacltc = 0  
                    call ltcctl (jt, ityp, jacltc, 0, dx, dy)
                    trace(3:) = '1' 
                  endif 
               else if (da.gt.1.0.or.da.lt.-1.0) then   
C       
C                 Most of error is still nodal injection error. However,
C                 LTC adjustments are nearly counterbalanced between
C                 nodal injection errors and phase shift errors.
C       
                  if (abs(dp).lt.1.0) then  
                    dx = dpkm * dadp
                  else  
                    dx = 0.0
                    jacltc = 0  
                    call ltcctl (jt, ityp, jacltc, 0, dx, dy)
                    trace(3:) = '2' 
                  endif 
               else 
C       
C                 A simple manual adjustment together with  
C                 nodal convergence should alleviate phase shift
C                 error.
C       
                  if (abs(dp).lt.1.0) then  
                    dx = (dpkm - dp) * dadp 
                  else  
                    dx = dpkm * dadp
                    jacltc = 0  
                    call ltcctl (jt, ityp, jacltc, 0, dx, dy)
                    trace(3:) = '3' 
                  endif 
               endif
            else
C       
C              Near-convergence suggests auto LTC adjustment
C       
               dx = dpkm * dadp 
               trace(3:) = '4'  
            endif   
         endif  
         if (iterm .ne. 0) then 
            write (*,260) jt,pkm,dpkm,dp,jacltc,da,dx,dadp  
  260       format(' LTC PHASE SHIFT ',i3,' PKM,DPKM,DP ',3e12.5/   
     &             '          JACLTC ',i3,' DA,DX       ',2e12.5/   
     &                '                     DADP        ',2e12.5)   
         endif  
C       
      endif 
C       
C     Test LTC for truncation/excessive adjustment  
C       
  500 trace(4:) = '.'   
      dt = dx   
      if (ityp .ge. 10) then 
         dt = 0.0   
         dx = 0.0   
         go to 502  
      else if (ityp.eq.3) then  
         if (abs (dx).gt.0.25) then 
           dx = sign (0.25,dx)  
           jacltc = 0   
           call ltcctl (jt, ityp, jacltc, 0, dx, dy) 
           trace(5:) = '1'  
         endif  
      else  
        if (abs (dx).gt.0.050) then 
           dx = sign (0.050,dx) 
           jacltc = 0   
           dxtemp = dx/tap(jt)
           call ltcctl (jt, ityp, jacltc, 0, dxtemp, dy)
           trace(5:) = '2'  
        endif   
      endif 
C       
C     Examine DX for limit truncation; change to
C     manual control if positive; disable if 100%   
C     of adjustment is truncated.   
C       
      tnew = tap(jt) + dx   
      dy = -ddim(dble(tnew),dble(tran(7,jt))) + 
     &      ddim(dble(tran(8,jt)),dble(tnew)) 
      if (abs (dy).gt.1.0e-5) then  
        dx = dx + dy
        jacltc = 0  
        if (ityp.eq.3) then 
           call ltcctl (jt, ityp, jacltc, 0, dx, dy) 
        else
           dxtemp = dx/tap(jt)
           call ltcctl (jt, ityp, jacltc, 0, dxtemp, dy)
        endif   
        trace(6:) = '1' 
        tnew = tap(jt) + dx 
      endif 
C       
C     Use caution (manual adjustment) if LTC's  on limit:   
C     Perform manual adjustments using 50% of estimated value.  
C       
      if (jacltc.gt.0) then 
         if (abs(dx).le.option(24)) then
            if (dx1.lt.2.0*option(24).or.dx2.gt.-2.0*option(24)) then 
               jacltc=0 
               dx = 0.0 
            endif   
         else if (dx.gt.0.0) then   
            if (dx.gt.dx1.or.-0.250*dx.lt.dx2) then 
               dx=amin1(0.5*dx,dx1) 
               jacltc=0 
            endif   
         else   
            if (dx.lt.dx2.or.-0.250*dx.gt.dx1) then 
               dx=amax1(0.5*dx,dx2) 
               jacltc=0 
            endif   
         endif  
         if (jacltc.eq.0) then  
           if (ityp .eq. 3) then
              call ltcctl (jt, ityp, jacltc, 0, dx, dy)  
           else 
              dxtemp = dx/tap(jt)
              call ltcctl (jt, ityp, jacltc, 0, dxtemp, dy)
           endif
           trace(6:) = '2'  
         endif  
         tnew = tap(jt) + dx
      endif 
C       
C     Restrict LTC phase shifter / area tie lines to small manual   
C     adjustments.  
C       
      if (ityp .eq. 3 .and. ltran(10,jt) .gt. 100 .and. jacltc .eq. 0)
     &   then   
         dadp = phse(jt)
         if (dadp.ge.0) dadp = -0.1 
         if (abs (dx) .gt. 0.10 .or. abs (dx/dadp) .gt. 0.25) then  
            dx1 = sign (0.10,dx)
            dx2 = sign (0.25 * dadp, dx)
            if (abs (dx) .lt. abs (dx1)) dx = dx1   
            if (abs (dx) .lt. abs (dx2)) dx = dx2   
            jacltc = 0  
            call ltcctl (jt, ityp, jacltc, 0, dx, dy)
            trace(6:) = '3' 
         endif  
      endif 
C       
C     Restrict LTC adjustment to manual if flagged by LTCSLN(*).
C       
      if (ltcsln(jt) .ne. 0) then   
         jacltc = 0 
C       
C        Limit DX to 5.00 p.u. d_qkm or 2.0*option(24) p.u. d_tap.   
C       
         if (ityp .eq. 1) then  
            if (abs(dx) .gt. 2.0*option(24)) then
               la1 = ltran(3,jt)
               if (bkmu(la1) * dx .gt. 5.00) then  
c
c	As an intermediate step convert back to single precision
c	Conversion from single to double precision - KLN
c
                  dx = sngl (dsign (dble(dx), 5.00 / bkmu(la1)) ) 
                  if (abs (dx) .lt. 2.0*option(24)) 
     &               dx = sign (2.0*option(24), dx)
               endif
            endif   
         endif  
         call ltcctl (jt, ityp, jacltc, 0, dx, dy)   
         trace(6:) = '4'
      endif 
  502 if (jacltc.eq.0) then 
C       
C       Manual adjustment in Jacobian is invoked by simple constraint:
C       
C             1.0*DT = DV(error)
C       
C       Note that if auto LTC control of either V or Q is already   
C       implemented but limits have changed it to manual, then the  
C       equation must be "reconstrained".   
C       
        if (inquir.eq.0) then   
           if (mod (ityp,10) .eq. 3) then   
              if (abs(dx).gt.0.100) then
                 kownta = kownta + 1
                 flag = '$' 
              endif 
           else 
              if (abs(dx).gt.10.0*option(24)) then
                 kownta = kownta + 1
                 flag = '$' 
              endif 
           endif
        endif   
        if (idswa.ne.0) write (dbug,510) flag,jt,trace,kc,dpkm,dv,dx,
     &   dp,dq,tap(jt),tnew,dx1,dx2 
  510   format (' Manual LTC adjustment ',a1,i4,1x,a,i5,9f10.5) 
      else if (jacltc.eq.1) then
C       
C       Auto NR adjustment in Jacobian is invoked by the
C       following contraint:
C       
C       0.00001*Dt + 1.0*Dv   = Dv(error)  
C       
        if (inquir.eq.0) then   
           if (ityp.eq.3) then  
              if (abs(dx).gt.0.100) then
                 kownta = kownta + 1
                 flag = '$' 
              endif 
           else 
              if (abs(dx).gt.10.0*option(24)) then
                 kownta = kownta + 1
                 flag = '$' 
              endif 
           endif
        endif   
        if (idswa.gt.0) write (dbug,520) flag,jt,trace,kc,dv,dx,
     &    dp,dq,tap(jt),tnew,dx1,dx2
  520   format (' Auto LTC V-adjustment ',a1,i4,1x,a,i5,10x,8f10.5) 
      else  
C       
C       Auto NR adjustment in Jacobian is invoked by any of the
C       following contraints:
C       
C       1.0*Dt + RH*Da + RN*Dv = Dq(error)  
C       1.0*DT + RH*DA + RN*DV + ... = DQk
C       1.0*DT + RH*DA + RN*DV + ... = DQm
C       
        if (inquir.eq.0) then   
           if (abs (dpkm).gt.option(6)) then
              kownta = kownta + 1   
              flag = '$'
           endif
        endif   
        if (idswa.gt.0) write (dbug,530) flag,jt,trace,dpkm,dx,tap(jt),
     &    tnew,dx1,dx2  
  530   format (' Auto LTC S-adjustment ',a1,i4,1x,a,5x,f10.5,10x,f10.5,
     &    20x,4f10.5)   
C       
        if (ityp.eq.3.and.iterm.ne.0) write (*,540) jt,dt,dx,tap(jt),
     &     tnew,jacltc,tran(8,jt),tran(7,jt)
  540   format (' LTC ADJUSTMENT  ',i3,' DT,DX,TAP,TNEW ',4e12.5/   
     &        '         JACLTC ',i3,' TMIN,TMAX      ',2e12.5)  
C       
      endif 
      if (inquir .eq. 0 .and. txstat(jt) .ge. 0) txstat(jt) = jacltc
      return
      end   
