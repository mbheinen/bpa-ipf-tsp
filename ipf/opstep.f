C    @(#)opstep.f	20.14 8/19/99
        subroutine opstep (jt, kt, mt, nt, bold, bnew, dvdq)
C
C       This function evaluates the OPtimal discrete STEP (BOLD, B1
C       or B2) such that
C
C       1. the estimated voltage is within limits,
C       2. the estimated voltage is "optimized" to V_max, and
C       3. the controlled voltage is within V-limits.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc' 
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tbxsrt.inc'
      include 'ipfinc/xblim.inc'
      include 'ipfinc/xdata.inc'
C
      logical recompute_dvdq, freeze_vk

        recompute_dvdq = .false.
        vk = dsqrt (e(kt) ** 2 + f(kt) **2 )

C       Obtain step intervals B1 <= BOLD <= B2.

        call xstep (nt, bold, b1, b2)

C       B1, B2 are evaluated in the limit (0.0, B_limit), where
C       B1 < B2.
        
        bup = amax1 (b1, b2)
        bdown = amin1 (b1, b2)

        bnew = bold
        dvk = -dim(vk,vlimx(kt)) + dim(vlimn(kt),vk)   
 
        if (mt .eq. 0 .or. mt .eq. kt) then
          dvm = 0.0
        else
          vm = dsqrt (e(mt) ** 2 + f(mt) ** 2 )
          dvm = -dim(vm,vlimx(mt)) + dim(vlimn(mt),vm) 
        endif   
        if (abs(xblim(1,nt)-bold) .le. 0.01) then  
          if (xvlim(1,nt) .eq. 0.0) recompute_dvdq = .true.
          xvlim(1,nt) = vk
          if (mt .ne. 0 .and. mt .ne. kt) xvrlim(1,nt) = vm
        endif
        if (abs(xblim(2,nt)-bold) .le. 0.01) then  
          if (xvlim(2,nt) .eq. 0.0) recompute_dvdq = .true.
          xvlim(2,nt) = vk
          if (mt .ne. 0 .and. mt .ne. kt) xvrlim(2,nt) = vm
        endif
        if (xvlim(1,nt) .gt. 0.0 .and. 
     &      vk .le. xvlim(1,nt) .and.
     &      bold-0.01 .gt. xblim(1,nt)) then
          if (bold .lt. xdata(4,nt)) then
            xblim(1,nt) = bold
            xvlim(1,nt) = vk
            if (mt .gt. 0 .and. mt .ne. kt) xvrlim(1,nt) = vm
          endif
        else if (xvlim(2,nt) .gt. 0.0 .and. 
     &      vk .ge. xvlim(2,nt) .and.
     &      bold+0.01 .lt. xblim(2,nt)) then
          if (bold .gt. xdata(3,nt)) then
            xblim(2,nt) = bold
            xvlim(2,nt) = vk
            if (mt .gt. 0 .and. mt .ne. kt) xvrlim(2,nt) = vm
          endif
        endif
c
c       If (1) xvlim and xblim are completely defined, and (2) there
c       has been a q-perturbation (recompute_dvdq = .true.) then xvlim
c       and xblim provide a better estimate of the sensitivity dv/dq
c
        if (xblim(1,nt) .ne. -9999.0 .and. 
     &      xblim(2,nt) .ne. -9999.0 .and.
     &      xvlim(1,nt) .gt. 0.0 .and.
     &      xvlim(2,nt) .gt. 0.0 .and.
     &      recompute_dvdq) then
          dq = xblim(2,nt) * xvlim(2,nt) ** 2 
     &       - xblim(1,nt) * xvlim(1,nt) ** 2 
          dv = xvlim(2,nt) - xvlim(1,nt)
          if (abs(dq) .gt. 5.0 .and. abs (dv) .gt. 0.001 .and.
     &        sign (1.0, dq) .eq. sign (1.0, dv)) then
            dvdq = dv / (dq / bmva)
            tbxslp(5,jt) = dvdq
            xsen(nt) = dvdq
          endif
        endif

        dv1 = dvdq * (bdown - bold) * vk ** 2 / bmva   
        dv2 = dvdq * (bup - bold) * vk ** 2 / bmva   
        dv3 = 0.0   
        dv4 = 0.0   

        freeze_vk = .true.
        if (dvk .gt. 0.0 .and. dvk .gt. -dvm) then  
c
c         V_min is violated.  Will raising B to bup improve voltage?
c
          if (bold .lt. xdata(4,nt)) then
            xblim(1,nt) = bold
            xvlim(1,nt) = vk  
            if (mt .gt. 0 .and. mt .ne. kt) xvrlim(1,nt) = vm
          endif

          if (dv2 .gt. 0) then
            vknew = vk + dv2   
            evk = -dim(vknew,vlimx(kt)) + dim(vlimn(kt),vknew)  
            if (abs(dvk) .gt. abs(evk)) then  
              if (xblim(2,nt) .eq. -9999.0) then
                bnew = bup
                xblim(2,nt) = bnew   
                xvlim(2,nt) = 0.0   
                xvrlim(2,nt) = 0.0   
                freeze_vk = .false.
              else if (abs (xblim(2,nt)-bup) .le. 0.01 .and.
     &                 xvlim(2,nt) .gt. 0.0 .and.
     &                 dvk .gt. dim(xvlim(2,nt), vlimx(kt))) then
                if (mt .eq. 0 .or. mt .eq. kt) then
                  bnew = bup
                  freeze_vk = .false.
                else if (xvrlim(2,nt) .lt. vlimx(mt)) then
                  bnew = bup
                  freeze_vk = .false.
                else
                  freeze_vk = .true.
                endif
              else if (xblim(2,nt)-bup .gt. 0.01) then
                bnew = bup
                freeze_vk = .false.
              else if (xblim(2,nt)-bup .lt. -0.01 .and.
     &                 xvlim(2,nt) .gt. 0.0 .and.
     &                 dvk .gt. dim(xvlim(2,nt), vlimx(kt))) then
                bnew = bup
                if (xblim(2,nt) .lt. xdata(4,nt)) then
                  xblim(1,nt) = xblim(2,nt)   
                  xvlim(1,nt) = xvlim(2,nt)  
                  xvrlim(1,nt) = xvrlim(2,nt)  
                endif
                if (bnew .gt. xdata(3,nt)) then
                  xblim(2,nt) = bnew   
                  xvlim(2,nt) = 0.0
                  xvrlim(2,nt) = 0.0
                endif
                freeze_vk = .false.
              endif  
            else if (xblim(2,nt) .eq. bup) then
              if (dim(xvlim(2,nt), vlimx(kt)) .lt. dvk) then
                if (mt .eq. 0 .or. mt .eq. kt) then
                  bnew = bup
                  freeze_vk = .false.
                else if (xvrlim(2,nt) .lt. vlimx(mt)) then
                  bnew = bup
                  freeze_vk = .false.
                else
                  freeze_vk = .true.
                endif
              endif
            endif 
          endif
       
        else if (dvk .lt. 0.0) then 
c
c         V_max is violated.  Will lowering B to bdown improve voltage?
c
          if (bold .gt. xdata(3,nt)) then
            xblim(2,nt) = bold
            xvlim(2,nt) = vk  
            if (mt .gt. 0 .and. mt .ne. kt) xvrlim(2,nt) = vm
          endif

          if (dv1 .lt. 0) then 
            vknew = vk + dv1   
            evk = -dim(vknew,vlimx(kt)) + dim(vlimn(kt),vknew)  
            if (abs(dvk) .gt. abs(evk)) then  
              if (xblim(1,nt) .eq. -9999.0) then
                bnew = bdown
                xblim(1,nt) = bnew   
                xvlim(1,nt) = 0.0   
                xvrlim(1,nt) = 0.0   
                freeze_vk = .false.
              else if (abs(xblim(1,nt)-bdown) .le. 0.01 .and.
     &                 xvlim(1,nt) .gt. 0.0 .and.
     &                 dvk .lt. -dim(vlimn(kt), xvlim(1,nt))) then
                if (mt .eq. 0 .or. mt .eq. kt) then
                  bnew = bdown
                  freeze_vk = .false.
                else if (xvrlim(1,nt) .gt. vlimn(mt)) then
                  bnew = bdown
                  freeze_vk = .false.
                else
                  freeze_vk = .true.
                endif
              else if (xblim(1,nt)-bdown .lt. -0.01) then
                bnew = bdown
                freeze_vk = .false.
              else if (xblim(1,nt)-bdown .gt. 0.01 .and.
     &                 xvlim(1,nt) .gt. 0.0 .and.
     &                 dvk .lt. -dim(vlimn(kt), xvlim(1,nt))) then
                bnew = bdown
                if (xblim(1,nt) .gt. xdata(3,nt)) then
                  xblim(2,nt) = xblim(1,nt)   
                  xvlim(2,nt) = xvlim(1,nt)  
                  xvrlim(2,nt) = xvrlim(1,nt)
                endif
                if (bnew .lt. xdata(4,nt)) then
                  xblim(1,nt) = bnew   
                  xvlim(1,nt) = 0.0   
                  xvrlim(1,nt) = 0.0   
                endif
                freeze_vk = .false.
              endif  
            else if (xblim(1,nt) .eq. bdown) then
              if (-dim(vlimn(kt), xvlim(1,nt)) .gt. dvk) then
                if (mt .eq. 0 .or. mt .eq. kt) then
                  bnew = bdown
                  freeze_vk = .false.
                else if (xvrlim(1,nt) .gt. vlimx(mt)) then
                  bnew = bdown
                  freeze_vk = .false.
                else
                  freeze_vk = .true.
                endif
              endif
            endif 
          endif
        
        else if (kspare(24) .eq. 1) then
C       
C         WSCC option: Estimate the voltage if BNEW is restored one
C         step towards the original value. 
C       
          if (xinit(nt)-bold .lt. -0.01) then  
            if (vk + dv1 .gt. vlimn(kt)) then
              bx = bdown
              freeze_vk = .false.
            else
              go to 900
            endif
          else if (xinit(nt)-bold .gt. 0.01) then  
            if (vk + dv2 .lt. vlimx(kt)) then
              bx = bup
              freeze_vk = .false.
            else
              go to 900
            endif
          else 
            go to 900 
          endif

          dvk = dvdq * (bx - bold) / bmva
          if (mt .eq. 0 .or. mt .eq. kt) then
            dvm = 0.0
          else
            dvm = dvk * vm / vk
            vmnew = vm + dvm
            dvm = dvm - dim(vmnew,vlimx(mt)) + dim(vlimn(mt),vmnew) 
            dvk = vm * dvm / vk 
          endif   
          vknew = vk + dvk
          evk = -dim(vknew,vlimx(kt)) + dim(vlimn(kt),vknew) 

          if (dvk .gt. 0 .and. evk .eq. 0.0) then
            if (xblim(2,nt) .eq. -9999.0) then
              bnew = bx
              xblim(2,nt) = bnew   
              xvlim(2,nt) = 0.0   
              xvrlim(2,nt) = 0.0   
              freeze_vk = .false.
            else if (abs(xblim(2,nt)-bx) .le. 0.01 .and.
     &               xvlim(2,nt) .gt. 0.0 .and.
     &               xvlim(2,nt) .lt. vlimx(kt)) then
              bnew = bx
              freeze_vk = .false.
            else if (xblim(2,nt)-bx .gt. 0.01) then
              bnew = bx
             freeze_vk = .false.
            else if (xblim(2,nt)-bx .lt. -0.01 .and.
     &               xvlim(2,nt) .gt. 0.0 .and.
     &               xvlim(2,nt) .lt. vlimx(kt)) then
              bnew = bx
              if (xblim(2,nt) .lt. xdata(4,nt)) then
                xblim(1,nt) = xblim(2,nt)   
                xvlim(1,nt) = xvlim(2,nt)  
                xvrlim(1,nt) = xvrlim(2,nt)  
              endif
              if (bnew .gt. xdata(3,nt)) then
                xblim(2,nt) = bnew   
                xvlim(2,nt) = 0.0   
                xvrlim(2,nt) = 0.0
              endif
              freeze_vk = .false.
            endif  
          else if (dvk .lt. 0 .and. evk .eq. 0.0) then
            if (xblim(1,nt) .eq. -9999.0) then
              bnew = bx
              xblim(1,nt) = bnew   
              xvlim(1,nt) = 0.0   
              xvrlim(1,nt) = 0.0   
              freeze_vk = .false.
            else if (abs(xblim(1,nt)-bx) .le. 0.01 .and.
     &               xvlim(1,nt) .gt. 0.0 .and.
     &               xvlim(1,nt) .gt. vlimn(kt)) then
              bnew = bx
             freeze_vk = .false.
            else if (xblim(1,nt)-bx .lt. -0.01) then
              bnew = bx
              freeze_vk = .false.
            else if (xblim(1,nt)-bx .gt. 0.01 .and.
     &               xvlim(1,nt) .gt. 0.0 .and.
     &               xvlim(1,nt) .gt. vlimn(kt)) then
              bnew = bx
              if (xblim(1,nt) .gt. xdata(3,nt)) then
                xblim(2,nt) = xblim(1,nt)   
                xvlim(2,nt) = xvlim(1,nt)  
                xvrlim(2,nt) = xvrlim(1,nt)  
              endif
              if (bnew .lt. xdata(4,nt)) then
                xblim(1,nt) = bnew   
                xvlim(1,nt) = 0.0   
                xvrlim(1,nt) = 0.0   
              endif
              freeze_vk = .false.
            endif  
          endif

        else if (kspare(24) .eq. 2) then
C       
C         BPA option: Strive for V_max solution.   
C       
          if (mt .eq. 0 .or. mt .eq. kt) then
            dvk = dv2
            vknew = vk + 1.25 * dv2
            dvk2 = dim(vknew,vlimx(kt)) - dim(vlimn(kt),vknew) 
            if (abs (dvk2) .gt. 0.0) then
              dvk = 0.0
            else
              freeze_vk = .false.
            endif
          else if (dvm .lt. 0.0) then
            dvk = dv2
            vmnew = vm + 1.25 * dv2 * vm / vk
            dvm2 = dim(vmnew,vlimx(mt)) - dim(vlimn(mt),vmnew) 
            if (abs (dvm2) .gt. abs(dvm)) then
              dvk = 0.0
            else
              freeze_vk = .false.
            endif
          else if (dvm .eq. 0.0) then 
            dvk = dv1
            dvm = dv1 * vm / vk
            vmnew = vm + 1.25 * dvm
            dvm2 = -dim(vmnew,vlimx(mt)) + dim(vlimn(mt),vmnew) 
            if (abs (dvm2) .gt. 0.0) then
              dvk = 0.0
            else
              freeze_vk = .false.
            endif
          endif   
          if (freeze_vk) go to 900
          vknew = vk + dvk
          evk = -dim(vknew,vlimx(kt)) + dim(vlimn(kt),vknew) 
          if (evk .eq. 0.0) then
            if (xblim(2,nt) .eq. -9999.0) then
              bnew = bup
              xblim(2,nt) = bnew   
              xvlim(2,nt) = 0.0   
              xvrlim(2,nt) = 0.0
            else if (abs(xblim(2,nt)-bup) .le. 0.01 .and.
     &               xvlim(2,nt) .gt. 0.0 .and.
     &               xvlim(2,nt) .lt. vlimx(kt)) then
              if (mt .eq. 0 .or. mt .eq. kt) then
                bnew = bup
              else if (xvrlim(1,nt) .lt. vlimx(mt)) then
                bnew = bup
              endif
            else if (xblim(2,nt)-bup .gt. 0.01) then
              bnew = bup
            else if (xblim(2,nt)-bup .lt. -0.01 .and.
     &               xvlim(2,nt) .gt. 0.0 .and.
     &               xvlim(2,nt) .lt. vlimx(kt)) then
              bnew = bup
              if (xblim(2,nt) .lt. xdata(4,nt)) then
                xblim(1,nt) = xblim(2,nt)   
                xvlim(1,nt) = xvlim(2,nt)  
                xvrlim(1,nt) = xvrlim(2,nt)  
              endif
              if (bnew .gt. xdata(3,nt)) then
                xblim(2,nt) = bnew   
                xvlim(2,nt) = 0.0   
                xvrlim(2,nt) = 0.0
              endif
            endif  
          else if (dvk .lt. 0 .and. (dvk + evk) / dvk .gt. 0.90) then
            if (xblim(1,nt) .eq. -9999.0) then
              bnew = bdown
              xblim(1,nt) = bnew   
              xvlim(1,nt) = 0.0   
              xvrlim(1,nt) = 0.0   
            else if (abs(xblim(1,nt)-bdown) .le. 0.01 .and.
     &               xvlim(1,nt) .gt. 0.0 .and.
     &               xvlim(1,nt) .gt. vlimn(kt)) then
              if (mt .eq. 0 .or. mt .eq. kt) then
                bnew = bdown
              else if (xvrlim(1,nt) .lt. vlimx(mt)) then
                bnew = bdown
              endif
            else if (xblim(1,nt)-bdown .lt. -0.01) then
              bnew = bdown
            else if (xblim(1,nt)-bdown .gt. 0.01 .and.
     &               xvlim(1,nt) .gt. 0.0 .and.
     &               xvlim(1,nt) .gt. vlimn(kt)) then
              bnew = bdown
              if (xblim(1,nt) .gt. xdata(3,nt)) then
                xblim(2,nt) = xblim(1,nt)   
                xvlim(2,nt) = xvlim(1,nt)  
                xvrlim(2,nt) = xvrlim(1,nt)  
              endif
              if (bnew .lt. xdata(4,nt)) then
                xblim(1,nt) = bnew   
                xvlim(1,nt) = 0.0   
                xvrlim(1,nt) = 0.0   
              endif
            endif  
          endif
        
        endif   
        if (abs(bnew - bold) .gt. 0.01) then
          bkku(kt) = bkku(kt) + (bnew - bold) / bmva   ! diagonal
          if (bnew .gt. 0.0) then  
            xdata(5,nt) = 0.0 
            xdata(6,nt) = bnew
          else if (bnew .lt. 0.0) then 
            xdata(5,nt) = bnew
            xdata(6,nt) = 0.0 
          else 
            xdata(5,nt) = 0.0 
            xdata(6,nt) = 0.0 
          endif
        endif   
        
  900   if (idswb .ne. 0) then  
          dvk = -dim(vk,vlimx(kt)) + dim(vlimn(kt),vk)   
          write (dbug, 100) jt, nt, kt, bold, bdown, bup, bnew, dv1, 
     &       dv2, vk, dvk
  100     format (' OPSTEP/ BX ',3i6,8e12.5)   
        endif   
        return  
        end 
