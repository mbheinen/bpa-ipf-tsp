C    @(#)setgen.f	20.8 8/19/99
        subroutine setgen (kt,mt,jt,qk)
C
C       This routine establishes the most appropriate V-control:
C
C          V(KT) --> V(MT)
C
C       Direct control (auto) is preferred over its counterpart -
C       indirect control (manual) - but it is prone to solution
C       divergence. Several tests must be fulfilled for direct
C       control to be established.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/intbus.inc'
 
        character tag*1
        real nrdvdq
C
C       DQV(V) estimates DQ/DV around operating point
C
        dqv(v) = amax1 (0.100, 0.250*abs (qk/v - sngl(bkku(kt))*v))  !uu
C
        ksw = 0
        tag = ' '
 
        if (kvolt(kt) .eq. 0) go to 1090

C       Check if voltage is within V-limits. This anachronism may
C       occur if relaxation occurs in NRBKSL immediately following   
C       a previous restoration from PQ-limit to PV-scheduled.

        vk = dsqrt (e(kt)**2 + f(kt)**2) 
        dvk = dim(vk,vlimx(kt)) - dim(vlimn(kt),vk)
        if (abs(dvk) .gt. 1.0e-3) then  
          ksw = 1 
          go to 1060  
        endif   
        vksq = vk**2   
        qsuscp = tbx(6,jt) * vksq  
        qmax = tbx(3,jt) * vksq - amin1 (0.0,qsuscp/bmva)  
        qmin = tbx(4,jt) * vksq - amax1 (0.0,qsuscp/bmva)  
        dvdq = nrdvdq (jt, ittot, kt, vk, qk, dqv(vk), dvdqap) 

C       BG or BX bus is in state PV; Establish Auto/Manual control
C       subject to the following conditions:  
C
C       1. Auto control if remote bus is adjacent controlled bus.

        if (mt .eq. 0 .or. mt .eq. kt) then
          go to 1090  
        else   
          do l = km(kt), km(kt)-1+kmlen(kt) 
            if (ikmu(l) .eq. mt) go to 890         !uur
          enddo
          ksw = 1 
  890     continue
        endif  

C       2. Remote bus must PQ. 

        if (kvolt(mt) .ne. 0) then 
          go to 1090  
        endif  

C       3. Generator/remote bus must be reasonably convergent. 

        call nrpqv (mt,pm,dpm,qm,dqm,vm)   
        dvm = -dim(vm,vlimx(mt)) + dim(vlimn(mt),vm) 
        if (abs(dpm) + abs(dqm) .gt. 100.0*option(7)) go to 1090   

C       4. Remote bus must violate V-limits

        if (dvm .eq. 0 .and. vlimn(mt) .lt. vlimx(mt)) then 
          if (dvk .eq. 0.0) then  
            dvk = amax1 (0.00501, 0.25*dvdq)
            dvk = amin1 (dvk, 0.75*(vlimx(kt)-vk)) 
            evk = dim (vm+dvk/vk*vm, vlimx(mt)) * vk/vm
            dvk = dvk - evk
            if (abs (dvk) .gt. option(24)) then  
              dqx = dvk / dvdq  
C       
C             Attenuate V-adjustment if driven into Q-limits.   
C       
              if (abs (dqx) .gt. 2.0) dqx = sign (2.0, dqx) 
              qnew = qk + dqx   
              dqx = dim (qnew, qmax) - dim (qmin, qnew) 
              dvx = dqx * dvdq  
              if (dvx .ne. 0.0 .and.
     1            abs(dvk) .gt. abs (dvx) .and. 
     2            sign (1.0,dvk) .eq. sign (1.0,dvx) .and.  
     3            abs (dvk - dvx) .gt. option(24)) then  
                if (sign(1.0, dvk) .eq. sign (1.0, dvx)) then
                   dvk = dvk - dvx  
                else
                   dvk = -dvx
                endif
              endif 
            endif
            ksw = 1
            go to 1060
          endif
        endif  

C       5. Generator control is established. Auto control is   
C          enabled if the following conditions are fulfilled.  
C          Manual control is enabled otherwise.

        dvk = vk*dvm/vm
        vnew = vk + dvk
        dx = -dim(vnew,vlimx(kt)) + dim(vlimn(kt),vnew)
        if (abs(dx) .gt. 1.0e-6) then  
          ksw = 100   
          vnew = vnew + dx
        endif  
        dvk = vnew - vk

C       6. V-adjustment should not exceed 0.050. 

        if (abs(dvk) .gt. 0.050) then  
          dvk = sign (0.050,dvk)  
          ksw = ksw + 50  
        endif  

C       7. Generator should not be driven into V-limit. Make allowances
C          for spurious adjustment. The "0.25" factor is an estimate of
C          the probability that the actual adjustment is opposite of the
C          direction predicted. 

        dv1 = dim(vlimx(kt),vk)
        dv2 = -dim(vk,vlimn(kt)) 
        if (amin1(abs(dv1),abs(dv2)).le.1.0e-3) ksw = ksw + 10 
        if (dvk .gt. 0.0) then 
          if (-0.25*dvk.lt.dv2) ksw = ksw + 20
        else   
          if (-0.25*dvk.gt.dv1) ksw = ksw + 20
        endif  

C       8. Generator should not be driven into Q-limit.   

 1050   qnew = qk + dvk / dvdq 
        dqx = dim (qnew, qmax) - dim (qmin, qnew) 
        dvx = dqx * dvdq  
        if (dvx .ne. 0.0 .and.
     1      abs(dvk) .gt. abs (dvx) .and. 
     2      sign (1.0,dvk) .eq. sign (1.0,dvx) .and.  
     3      abs (dvk - dvx) .gt. option(24)) then  
          dvk = dvk - dvx
        endif 
        if (qnew-qmax .gt. option(8) .or.
     &      qnew-qmin .lt. -option(8)) ksw = ksw + 5   
C       
C       Set up control 
C       
 1060   if (ksw .ne. 0) then
          kvolt(kt) = kt  
          if (abs(dvk) .lt. option(24)) dvk = 0.0
          volt(kt) = dvk/vk   
          if (abs(dvk) .gt. 5.0e-3) then
            kowntb = kowntb + 1  
            tag = '$'
          endif   
        else   
          kvolt(kt) = mt  
          volt(mt) = dvm/vm   
          if (abs(dvm).gt.5.0e-3) then
            kowntb = kowntb + 1  
            tag = '$'
          endif   
        endif  
C       
C       Generator control enabled  
C       
        if (idswb .ne. 0) then 
          write (dbug,1080) tag,kt,mt,ksw,kvolt(kt),kvolt(mt),
     1        vk,vm,volt(kt),volt(mt)   
 1080     format (' SETGEN - ',a,1x,5i5,4f10.5)   
        endif  
 1090   continue   
        return 
        end
