C    @(#)switchbx.f	20.3 11/12/98
C****************************************************************
C
C     File: switchbx.f
C
C     Purpose: Function to perform X-bus switching within subroutine 
c              NRTX if conditions are feasible
C
C
C     Input parameters:
c              jt = tbx() index
c              nt = xdata() index
c              kt = terminal bus
c              vk = terminal bus voltage
c              qc = circulating var flow 
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: nrtx.f
C
C****************************************************************
      integer function switchbx (jt, nt, kt, vk, qc, bold, bnew)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/trmdbg.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/xblim.inc'

      save 

      switchbx = 0

      dvk = -dim(vk,vlimx(kt)) + dim(vlimn(kt),vk)   
      mt = iabs(ifix(sngl(tbx(8,jt))))
      if (mt .eq. 0 .or. mt .eq. kt) then
        dvm = 0.0
      else
        vm = dsqrt (e(mt) ** 2 + f(mt) ** 2 )
        dvm = -dim(vm,vlimx(mt)) + dim(vlimn(mt),vm) 
        dvkm = vk * dvm / vm 
        vnew = vk + dvkm 
        dvk = dvkm - dim(vnew,vlimx(kt)) + dim(vlimn(kt),vnew) 
      endif   

      nt = tbx(5,jt)
      call xstep (nt, bold, b1, b2)

C     B1, B2 are evaluated in the limit (0.0, B_limit), 
C     where B1 < B2.
                          
      bup = amax1 (b1, b2) 
      bdown = amin1 (b1, b2) 

      if (abs(bup-bold) .gt. 0.1 .and. qc .lt. -0.1 .and.
     &    -qc .gt. bup * vk ** 2) then
        bnew = bup
        bdown = 0.0
      else if (abs(bdown-bold) .lt. -0.1 .and. 
     &         qc .gt. 0.1 .and.
     &         -qc .lt. bdown * vk ** 2) then
        bnew = bdown
        bup = 0.0
      else
        bnew = bold
        bup = 0.0
        bdown = 0.0
      endif
      if (xblim(1,nt) .eq. -9999.0) then 
        xblim(1,nt) = xdata(5,nt) + xdata(6,nt)
        xblim(2,nt) = xblim(1,nt)   
        xvlim(1,nt) = vk
        xvlim(2,nt) = vk
      endif  
      if (abs(xblim(1,nt)-bold) .le. 0.01) then  
        xvlim(1,nt) = vk
      endif
      if (abs(xblim(2,nt)-bold) .le. 0.01) then  
        xvlim(2,nt) = vk
      endif

      dvdq = xsen(nt)
      dv1 = dvdq * (bdown - bold) * vk ** 2 / bmva   
      dv2 = dvdq * (bup - bold) * vk ** 2 / bmva   
      dv3 = 0.0   
      dv4 = 0.0   

      if (dvk .gt. 0.0) then  
c
c       V_min is violated.  Will raising B to bup improve voltage?
c
        xblim(1,nt) = bold
        xvlim(1,nt) = vk  
        if (xblim(2,nt) .lt. bold) then
          xblim(2,nt) = bold
          xvlim(2,nt) = vk  
        endif

        if (dv2 .gt. 0) then
          vnew = vk + dv2   
          evk = -dim(vnew,vlimx(kt)) + dim(vlimn(kt),vnew)  
          if (abs(dvk) .gt. abs(evk)) then  
            if (xblim(2,nt) .eq. -9999.0) then
              bnew = bup
              xblim(2,nt) = bnew   
              xvlim(2,nt) = 0.0   
            else if (abs (xblim(2,nt)-bup) .le. 0.01 .and.
     &               dvk .gt. dim(xvlim(2,nt),vlimx(kt))) then
              bnew = bup
            else if (xblim(2,nt)-bup .gt. 0.01) then
              bnew = bup
            else if (xblim(2,nt)-bup .lt. -0.01) then
              bnew = bup
              xblim(2,nt) = bnew   
              xvlim(2,nt) = 0.0   
            endif  
          endif 
        endif

      else if (dvk .lt. 0.0) then 
c
c       V_max is violated.  Will lowering B to bdown improve voltage?
c
        xblim(2,nt) = bold
        xvlim(2,nt) = vk  
        if (xblim(1,nt) .gt. bold) then
          xblim(1,nt) = bold
          xvlim(1,nt) = vk  
        endif

        if (dv1 .lt. 0) then 
          vnew = vk + dv1   
          evk = -dim(vnew,vlimx(kt)) + dim(vlimn(kt),vnew)  
          if (abs(dvk) .gt. abs(evk)) then  
            if (xblim(1,nt) .eq. -9999.0) then
              bnew = bdown
              xblim(1,nt) = bnew   
              xvlim(1,nt) = 0.0   
            else if (abs(xblim(1,nt)-bdown) .le. 0.01 .and.
     &               dvk .lt. -dim(vlimn(kt), xvlim(1,nt))) then
              bnew = bdown
            else if (xblim(1,nt)-bdown .lt. -0.01) then
              bnew = bdown
            else if (xblim(1,nt)-bdown .gt. 0.01 .and.
     &               xvlim(1,nt) .ge. vlimn(kt)) then
              bnew = bdown
              xblim(1,nt) = bnew   
              xvlim(1,nt) = 0.0   
            endif  
          endif 
        endif

      else if (abs(bnew-bold) .gt. 0.1) then
c
c       Within V-limits. Determine feasibility of changing 
c       BOLD -> BNEW.
c
        bx = bnew
        bnew = bold
        dvk = dvdq * (bx - bold) / bmva
        if (mt .eq. 0 .or. mt .eq. kt) then
        else
          dvm = dvk * vm / vk
          vnew = vm + dvm
          dvm = dvm - dim(vnew,vlimx(mt)) + dim(vlimn(mt),vnew) 
          dvk = vm * dvm / vk 
        endif   
        vnew = vk + dvk
        evk = -dim(vnew,vlimx(kt)) + dim(vlimn(kt),vnew) 

        if (dvk .gt. 0 .and. evk .eq. 0.0) then
          if (xblim(2,nt) .eq. -9999.0) then
            bnew = bx
            xblim(2,nt) = bnew   
            xvlim(2,nt) = 0.0   
          else if (abs(xblim(2,nt)-bx) .le. 0.01 .and.
     &             dvk .gt. dim(xvlim(2,nt), vlimx(kt))) then
            bnew = bx
          else if (xblim(2,nt)-bx .gt. 0.01) then
            bnew = bx
          else if (xblim(2,nt)-bx .lt. -0.01 .and.
     &             xvlim(2,nt) .le. vlimx(kt)) then
            bnew = bx
            xblim(2,nt) = bnew   
            xvlim(2,nt) = 0.0   
          endif  
        else if (dvk .lt. 0 .and. evk .eq. 0.0) then
          if (xblim(1,nt) .eq. -9999.0) then
            bnew = bx
            xblim(1,nt) = bnew   
            xvlim(1,nt) = 0.0   
          else if (abs(xblim(1,nt)-bx) .le. 0.01 .and.
     &             dvk .lt. -dim(vlimn(kt), xvlim(1,nt))) then
            bnew = bx
          else if (xblim(1,nt)-bx .lt. -0.01) then
            bnew = bx
          else if (xblim(1,nt)-bx .gt. 0.01 .and.
     &             xvlim(1,nt) .ge. vlimn(kt)) then
            bnew = bx
            xblim(1,nt) = bnew   
            xvlim(1,nt) = 0.0   
          endif  
        endif
      endif
      if (abs(bnew-bold) .gt. 0.1) then
        bkku(kt) = bkku(kt) + (bnew - bold) / bmva 
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
        switchbx = 1
        if (idswb .ne. 0) then  
          write (dbug, 100) jt, nt, kt, bold, bdown, bup, bnew, qc
  100     format (' SWITCHBX ',3i6, 8e12.5)   
        endif   
      endif
      return
      end
