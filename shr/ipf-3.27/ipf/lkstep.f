C    @(#)lkstep.f	20.3 2/13/96
        subroutine lkstep (jt, kt, mt, nt, bold, bnew, dvdq, vmin, vmax)
C                      emulates a CJ4 voltage array by  
C         1. no operation when the voltaqe is within the deadband   
C            (Vmin < Vk < Vmax);
C         2. mandatory switching operations if voltage is outside of
C            deadband.
C
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/xblim.inc'
      include 'ipfinc/xdata.inc'
 
        vk = dsqrt (e(kt) ** 2 + f(kt) **2 )
C                     Obtain step intervals B1 <= BOLD <= B2.
        call xstep (nt, bold, b1, b2)
C                     B1, B2 are evaluated in the limit (0.0, B_limit).
C                     Thus, ABS (B1) < ABS (B2).
        bnew = bold
        dvk = -dim(vk,vmax) + dim(vmin,vk)
 
        dv1 = dvdq * (b1 - bold) * vk ** 2 / bmva
        dv2 = dvdq * (b2 - bold) * vk ** 2 / bmva
        dv3 = 0.0
        dv4 = 0.0
 
        if (dvk .gt. 0.0) then
           olddv = xvlim(1,nt)
           if (dv1 .gt. 0) then
              bnew = b1
           else if (dv2 .gt. 0) then
              bnew = b2 
           endif
        else if (dvk .lt. 0.0) then 
           olddv = xvlim(1,nt)  
           if (dv1 .lt. 0) then 
              bnew = b1 
           else if (dv2 .lt. 0) then
              bnew = b2 
           endif
        endif   
        if (abs(bnew - bold) .gt. 0.01) then
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
        endif   
  900   if (idswb .ne. 0) then  
           write (dbug, 100) jt, nt, kt, bold, b1, b2, bnew, dv1, dv2,  
     1       dv3, dv4   
  100      format (' LKSTEP/ BX ',3i6,8e12.5)   
        endif   
        return  
        end 
