C    @(#)bestep.f	20.3 2/13/96
        function bestep (kt, mt, nt, qk, q1, q2, dvdq)
C
C       This function evaluates the BEst discrete 
C       STEP (Q1 or Q2) such that BESTEP has
C
C       1. an estimated voltage which is within limits, and
C       2. the selected Q is closest to QK.
C
c	QK, Q1, Q2, DVDQ should be double precision.
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		vlimn(r*4), vlimx(r*4)
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e (r*8), e(r*8)
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		idswb 
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
c
c			
        dimension viol(2,2),dx(2,2)
c
c***kln Single to double
c	
	double precision viol, dx, dq1, dq2, dx1, dx2, vk, vk1, 
     +		         vk2, dv, dv1, dv2, vm, vm1, vm2
C
        dq1 = q1 - qk
        dq2 = q2 - qk
        dv1 = dvdq * dq1
        dv2 = dvdq * dq2
        vk = dsqrt (e(kt) ** 2 + f(kt) **2 ) 
        vk1 = vk + dv1  
        vk2 = vk + dv2  
        viol(1,1) = -ddim (dble(vlimn(kt)),vk1)           
        viol(1,2) = ddim (vk1,dble(vlimx(kt)))            
        viol(2,1) = -ddim (dble(vlimn(kt)),vk2)           
        viol(2,2) = ddim (vk2,dble(vlimx(kt)))            

C       Modify these limits if remote control used   

        if (mt .gt. 0 .and. mt .ne. kt) then
           vm = dsqrt (e(mt) ** 2 + f(mt) **2 )  
           vm1 = vm + dv1 * vm / vk 
           vm2 = vm + dv2 * vm / vk 
           dv = -ddim (dble(vlimn(mt)),vm1)               
           viol(1,1) = dmin1 (dv,viol(1,1)) 
           dv = ddim (vm1,dble(vlimx(mt)))                
           viol(1,2) = dmax1 (dv,viol(1,2)) 
           dv = -ddim (dble(vlimn(mt)),vm2)               
           viol(2,1) = dmin1 (dv,viol(2,1)) 
           dv = ddim (vm2,dble(vlimx(mt)))                
           viol(2,2) = dmax1 (dv,viol(2,2)) 
        endif   

C       Determine "safest" choice (minimax: minimum of maximum  
C       violation). 

        do 90 i = 1,2   
        do 90 j = 1,2   
   90   dx(j,i) = dabs (viol(j,i))   
        dx1 = dmax1 (dx(1,1),dx(1,2))   
        dx2 = dmax1 (dx(2,1),dx(2,2))   
        if (dx1 .lt. dx2 ) then 
           bestep = q1  
        else if (dx1 .gt. dx2) then 
           bestep = q2  
        else
           if (dabs(dq1) .lt. abs(dq2)) then 
              bestep = q1   
           else 
              bestep = q2   
           endif
        endif   
        if (idswb .ne. 0) write (dbug,100) kt,mt,viol,q1,q2,bestep  
  100   format (' BESTEP/ Type X ',2i6,7e12.5)  
        return  
        end 
